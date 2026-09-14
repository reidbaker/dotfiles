import 'models/pr_item.dart';
import 'models/triage_config.dart';
import 'models/triaged_item.dart';

/// Assigns each pull request to a priority tier.
///
/// Tiers are selected by evaluating every applicable rule and taking the
/// lowest rank, rather than by the order of `if` statements. That makes the
/// priority order a property of [MyWorkTier] and [ReviewQueueTier] alone, so
/// it can be audited against SKILL.md by reading the enums.
class PrClassifier {
  const PrClassifier({this.clock});

  final DateTime Function()? clock;

  DateTime get _now => clock != null ? clock!() : DateTime.now();

  /// Classifies a list of PRs into "My Work" and "Review Queue".
  (List<TriagedItem> myWork, List<TriagedItem> reviewQueue) classifyAll({
    required List<PrItem> myPrs,
    required List<PrItem> reviewPrs,
    required TriageConfig config,
  }) {
    final myWork = myPrs.map((pr) => classifyMyWork(pr, config)).toList()
      ..sort(_compareTriagedItems);

    final reviewQueue =
        reviewPrs.map((pr) => classifyReviewQueue(pr, config)).toList()
          ..sort(_compareTriagedItems);

    return (myWork, reviewQueue);
  }

  /// Classifies a PR authored by one of the configured accounts.
  TriagedItem classifyMyWork(PrItem pr, TriageConfig config) {
    final days = businessDaysWaiting(pr, config);
    final override = config.getOverrideFor(pr.repo, pr.number);

    final scoped = _scopeItem(pr, config, QueueType.myWork, days, override);
    if (scoped != null) return scoped;

    if (pr.isDraft) {
      return _annotate(_draftItem(pr, config, days), override);
    }

    final candidates = <TriagedItem>[
      ?_checkReadyToMerge(pr, days),
      ?_checkWaitingCicd(pr, config, days),
      ?_checkFlakyCi(pr, config, days),
      ?_checkMinorFeedback(pr, days),
      ?_checkFailingCi(pr, config, days),
      ?_checkSubstantialFeedback(pr, days),
      _reviewAgeItem(pr, config, days),
    ];
    return _annotate(_lowestRank(candidates), override);
  }

  /// Classifies an incoming review request or assignment.
  TriagedItem classifyReviewQueue(PrItem pr, TriageConfig config) {
    final days = businessDaysWaiting(pr, config);
    final override = config.getOverrideFor(pr.repo, pr.number);

    final scoped =
        _scopeItem(pr, config, QueueType.reviewQueue, days, override);
    if (scoped != null) return scoped;

    if (pr.isDraft) {
      return _annotate(
        _buildItem(
          pr: pr,
          queue: QueueType.reviewQueue,
          tier: ReviewQueueTier.draftReview,
          reason: 'Author has not marked this ready for review',
          days: days,
        ),
        override,
      );
    }

    final signals = _ReviewSignals.of(pr, config);
    final candidates = <TriagedItem>[
      ?_checkReReviewReady(pr, config, days),
      ?_checkTeamReview(pr, days, signals),
      ?_checkCleanExternal(pr, days, signals),
      ?_checkWaitingOnAuthor(pr, days, signals),
      ?_checkBlockedExternal(pr, days, signals),
      _buildItem(
        pr: pr,
        queue: QueueType.reviewQueue,
        tier: ReviewQueueTier.other,
        reason: signals.backlogReason(pr),
        days: days,
      ),
    ];
    return _annotate(_lowestRank(candidates), override);
  }

  // ---------------------------------------------------------------------
  // My Work rules
  // ---------------------------------------------------------------------

  /// Tier 1. Requires a positive CI signal: either checks ran and passed, or
  /// the repository demonstrably runs no checks. A rollup we merely failed to
  /// parse must never produce "[Action: Merge]".
  TriagedItem? _checkReadyToMerge(PrItem pr, int days) {
    final ciOk = pr.hasPassingCi || pr.hasNoChecksConfigured;
    final threadsClear =
        pr.unresolvedThreadsExact && pr.unresolvedReviewThreads == 0;
    if (!pr.isApproved || pr.isMergeBlocked || !ciOk || !threadsClear) {
      return null;
    }
    return _buildItem(
      pr: pr,
      queue: QueueType.myWork,
      tier: MyWorkTier.readyToMerge,
      reason: pr.hasPassingCi
          ? 'Approved, CI green, mergeable, no unresolved threads'
          : 'Approved, no CI configured, mergeable, no unresolved threads',
      days: days,
    );
  }

  /// Tier 2. Only fires when the author can actually unblock CI.
  ///
  /// The presubmit trigger label is scoped per repository because it does not
  /// exist everywhere: telling a `dart-lang` contributor to apply `CICD` asks
  /// them to add a label the repo has never defined. It also requires a
  /// positive "zero checks ran" signal rather than the absence of a rollup,
  /// and never fires on drafts, whose CI is not expected to be running.
  TriagedItem? _checkWaitingCicd(PrItem pr, TriageConfig config, int days) {
    if (pr.isDraft || pr.hasPassingCi) return null;

    // An explicit block is checked first. A PR held behind a red tree may also
    // have no checks yet, and telling the author to trigger CI in that state
    // sends them to do something that cannot succeed.
    if (pr.hasAnyLabel(config.cicdBlockedLabels)) {
      return _buildItem(
        pr: pr,
        queue: QueueType.myWork,
        tier: MyWorkTier.waitingOnCicdTask,
        reason: 'Blocked by a tree-status or code-freeze label',
        days: days,
        actionOverride:
            '[Action: Waiting for tree to go green / freeze to resolve]',
      );
    }

    final trigger = config.presubmitTriggerLabelFor(pr.repo);
    if (trigger != null &&
        !pr.hasLabel(trigger) &&
        pr.hasNoChecksConfigured) {
      return _buildItem(
        pr: pr,
        queue: QueueType.myWork,
        tier: MyWorkTier.waitingOnCicdTask,
        reason: 'No checks have run and the "$trigger" trigger label is absent',
        days: days,
        actionOverride: '[Action: Apply $trigger label to trigger CI]',
      );
    }


    if (pr.failingChecks.any(_isActionRequired)) {
      return _buildItem(
        pr: pr,
        queue: QueueType.myWork,
        tier: MyWorkTier.waitingOnCicdTask,
        reason: 'A check run requires manual approval before it can proceed',
        days: days,
        actionOverride: '[Action: Approve the pending CI run]',
      );
    }

    return null;
  }

  /// Tiers 3 and 10. Drafts are handled in one place so their reason string
  /// can carry the CI and review-thread state instead of discarding it.
  TriagedItem _draftItem(PrItem pr, TriageConfig config, int days) {
    final blockers = _activeBlockingReviews(pr);
    final clean = pr.hasPassingCi &&
        !pr.isMergeBlocked &&
        blockers.isEmpty &&
        pr.unresolvedReviewThreads == 0;

    if (clean) {
      return _buildItem(
        pr: pr,
        queue: QueueType.myWork,
        tier: MyWorkTier.draftReadyForReview,
        reason: 'Draft with green CI, no conflicts and no open feedback',
        days: days,
      );
    }

    return _buildItem(
      pr: pr,
      queue: QueueType.myWork,
      tier: MyWorkTier.draft,
      reason: 'Draft: ${_draftBlockers(pr, blockers).join('; ')}',
      days: days,
    );
  }

  List<String> _draftBlockers(PrItem pr, List<PrReview> blockers) {
    final parts = <String>[
      if (pr.hasFailingCi) 'CI failing (${pr.failingChecks.length} checks)',
      if (pr.ciStatus == CiStatus.pending) 'CI still running',
      if (pr.hasNoChecksConfigured) 'no checks have run',
      if (pr.isMergeBlocked) 'merge conflicts',
      if (pr.unresolvedReviewThreads > 0)
        '${pr.unresolvedReviewThreads} unresolved review threads',
      if (blockers.isNotEmpty)
        'changes requested by ${blockers.map((r) => r.author).join(', ')}',
    ];
    return parts.isEmpty ? const <String>['work in progress'] : parts;
  }


  /// Tier 4.
  TriagedItem? _checkFlakyCi(PrItem pr, TriageConfig config, int days) {
    if (!pr.hasFailingCi || !_isFlakyCiFailure(pr, config)) return null;
    return _buildItem(
      pr: pr,
      queue: QueueType.myWork,
      tier: MyWorkTier.flakyCiFailure,
      reason: 'CI failed only on known flaky checks: '
          '${pr.failingChecks.join(', ')}',
      days: days,
    );
  }

  /// Tier 5.
  TriagedItem? _checkMinorFeedback(PrItem pr, int days) {
    if (!pr.isApproved || pr.unresolvedReviewThreads == 0) return null;
    return _buildItem(
      pr: pr,
      queue: QueueType.myWork,
      tier: MyWorkTier.minorFeedbackWithApproval,
      reason: 'Approved with ${pr.unresolvedReviewThreads} unresolved comments',
      days: days,
    );
  }

  /// Tier 6.
  TriagedItem? _checkFailingCi(PrItem pr, TriageConfig config, int days) {
    if (!pr.hasFailingCi || _isFlakyCiFailure(pr, config)) return null;
    return _buildItem(
      pr: pr,
      queue: QueueType.myWork,
      tier: MyWorkTier.failingCiWorkRelated,
      reason: 'CI failed on checks requiring code fixes: '
          '${pr.failingChecks.join(', ')}',
      days: days,
    );
  }

  /// Tier 7.
  TriagedItem? _checkSubstantialFeedback(PrItem pr, int days) {
    final blockers = _activeBlockingReviews(pr);
    if (blockers.isEmpty && pr.unresolvedReviewThreads == 0) return null;
    final parts = <String>[
      if (blockers.isNotEmpty)
        'changes requested by ${blockers.map((r) => r.author).join(', ')}',
      if (pr.unresolvedReviewThreads > 0)
        '${pr.unresolvedReviewThreads} unresolved review threads',
    ];
    return _buildItem(
      pr: pr,
      queue: QueueType.myWork,
      tier: MyWorkTier.substantialFeedback,
      reason: parts.join('; '),
      days: days,
    );
  }

  /// Tiers 8 and 9.
  TriagedItem _reviewAgeItem(PrItem pr, TriageConfig config, int days) {
    final stalled = days >= config.staleReviewBusinessDays;
    return _buildItem(
      pr: pr,
      queue: QueueType.myWork,
      tier: stalled ? MyWorkTier.stalledInReview : MyWorkTier.freshInReview,
      reason: stalled
          ? 'Awaiting review for $days business days '
              '(threshold ${config.staleReviewBusinessDays})'
          : 'Awaiting review for $days business days',
      days: days,
    );
  }

  // ---------------------------------------------------------------------
  // Review queue rules
  // ---------------------------------------------------------------------

  /// Tier 1: the author acted on your feedback, or GitHub re-requested you.
  ///
  /// Deliberately reads [PrItem.allReviews] rather than `latestReviews`:
  /// GitHub removes a reviewer from `latestReviews` once a re-review is
  /// requested from them, which is precisely the state this tier detects.
  /// It also keys "new work exists" off the head commit date, because
  /// `updatedAt` moves on label edits and bot comments.
  TriagedItem? _checkReReviewReady(PrItem pr, TriageConfig config, int days) {
    final mine = pr.latestReviewBy(config.isMyAccount);
    if (mine == null) return null;

    final head = pr.headCommitDate;
    final pushedSince = head != null && head.isAfter(mine.submittedAt);
    final reRequested = pr.requestedReviewers.any(config.isMyAccount);
    if (!pushedSince && !reRequested) return null;

    return _buildItem(
      pr: pr,
      queue: QueueType.reviewQueue,
      tier: ReviewQueueTier.reReviewReady,
      reason: reRequested
          ? 'Re-review requested from you after your ${mine.state} review'
          : 'Author pushed new commits after your ${mine.state} review',
      days: days,
    );
  }

  /// Tier 2.
  TriagedItem? _checkTeamReview(PrItem pr, int days, _ReviewSignals s) {
    if (!s.isTeamAuthor || s.isBlocked || s.waitingOnAuthor) return null;
    if (pr.hasFailingCi) return null;
    return _buildItem(
      pr: pr,
      queue: QueueType.reviewQueue,
      tier: ReviewQueueTier.teamReviewRequest,
      reason: 'Teammate ${pr.author} is waiting on review (CI: ${pr.ciStatus.name})',
      days: days,
    );
  }

  /// Tier 3.
  TriagedItem? _checkCleanExternal(PrItem pr, int days, _ReviewSignals s) {
    if (s.isTeamAuthor || s.isBlocked || s.waitingOnAuthor) return null;
    if (pr.hasFailingCi) return null;
    return _buildItem(
      pr: pr,
      queue: QueueType.reviewQueue,
      tier: ReviewQueueTier.cleanExternalPr,
      reason: 'External contributor PR with no blockers (CI: ${pr.ciStatus.name})',
      days: days,
    );
  }

  /// Tier 4.
  TriagedItem? _checkWaitingOnAuthor(PrItem pr, int days, _ReviewSignals s) {
    if (s.claMissing) return null;
    final teamAuthorBlocked = s.isTeamAuthor && s.blockers.isNotEmpty;
    if (!s.hasWaitingLabel && !teamAuthorBlocked) return null;
    return _buildItem(
      pr: pr,
      queue: QueueType.reviewQueue,
      tier: ReviewQueueTier.waitingOnAuthor,
      reason: s.hasWaitingLabel
          ? 'Labelled as waiting on the author'
          : 'Changes requested by ${s.blockers.map((r) => r.author).join(', ')}',
      days: days,
    );
  }

  /// Tier 6. Emits an honest reason naming whichever condition actually
  /// matched, instead of always claiming a CLA problem.
  TriagedItem? _checkBlockedExternal(PrItem pr, int days, _ReviewSignals s) {
    if (!s.isBlocked) return null;
    final parts = <String>[
      if (s.claMissing) 'CLA not signed',
      if (s.blockers.isNotEmpty)
        'changes requested by ${s.blockers.map((r) => r.author).join(', ')}',
    ];
    return _buildItem(
      pr: pr,
      queue: QueueType.reviewQueue,
      tier: ReviewQueueTier.blockedExternalPr,
      reason: parts.join('; '),
      days: days,
    );
  }

  // ---------------------------------------------------------------------
  // Shared helpers
  // ---------------------------------------------------------------------

  /// Handles `pr_overrides` that move a PR, plus fork and non-primary repo
  /// scoping. Returns null when the PR should be classified normally.
  TriagedItem? _scopeItem(
    PrItem pr,
    TriageConfig config,
    QueueType queue,
    int days,
    PrOverride? override,
  ) {
    if (override != null && override.changesPlacement) {
      return _overrideItem(pr, queue, days, override);
    }
    if (config.isFork(pr.repo)) {
      return _annotate(
        _buildItem(
          pr: pr,
          queue: queue,
          tier: _forkTier(queue),
          reason: 'Personal fork or scratch repository (${pr.repo})',
          days: days,
          isPrimary: false,
        ),
        override,
      );
    }
    if (!config.isPrimaryRepo(pr.repo)) {
      return _annotate(
        _buildItem(
          pr: pr,
          queue: queue,
          tier: _nonPrimaryTier(queue),
          reason: '${pr.repo} is not listed in primary_orgs or primary_repos',
          days: days,
          isPrimary: false,
        ),
        override,
      );
    }
    return null;
  }

  TriagedItem _overrideItem(
    PrItem pr,
    QueueType queue,
    int days,
    PrOverride override,
  ) {
    final pinned = switch (override.tier) {
      final String name when queue == QueueType.myWork =>
        MyWorkTier.byName(name) as TriageTier?,
      final String name => ReviewQueueTier.byName(name) as TriageTier?,
      _ => null,
    };
    if (override.tier != null && pinned == null) {
      throw FormatException(
        'pr_overrides for ${pr.repo}#${pr.number}: unknown tier '
        '"${override.tier}".',
      );
    }
    return _buildItem(
      pr: pr,
      queue: queue,
      tier: pinned ?? _forkTier(queue),
      reason: override.note != null
          ? 'Annotated: ${override.note}'
          : 'Placement set by pr_overrides',
      days: days,
      isPrimary: pinned != null,
      actionOverride: override.action,
    );
  }

  /// Applies an annotation-only override: keeps the computed tier but surfaces
  /// the note and any custom action.
  TriagedItem _annotate(TriagedItem item, PrOverride? override) {
    if (override == null || override.changesPlacement) return item;
    if (override.note == null && override.action == null) return item;
    return TriagedItem(
      pr: item.pr,
      queue: item.queue,
      tier: item.tier,
      reason: override.note != null
          ? '${item.reason} | Annotated: ${override.note}'
          : item.reason,
      businessDaysElapsed: item.businessDaysElapsed,
      actionOverride: override.action ?? item.actionOverride,
      isPrimary: item.isPrimary,
    );
  }

  static TriageTier _forkTier(QueueType queue) => queue == QueueType.myWork
      ? MyWorkTier.forkOrPoc
      : ReviewQueueTier.forkOrPocReview;

  static TriageTier _nonPrimaryTier(QueueType queue) =>
      queue == QueueType.myWork
          ? MyWorkTier.nonPrimaryRepo
          : ReviewQueueTier.nonPrimaryRepoReview;

  /// Human `CHANGES_REQUESTED` reviews that the author has not yet responded
  /// to with new commits. Bot verdicts and reviews predating the head commit
  /// are excluded.
  List<PrReview> _activeBlockingReviews(PrItem pr) {
    final head = pr.headCommitDate;
    return pr.humanReviews
        .where((r) => r.isChangesRequested)
        .where((r) => head == null || r.submittedAt.isAfter(head))
        .toList();
  }

  bool _isFlakyCiFailure(PrItem pr, TriageConfig config) {
    if (pr.failingChecks.isEmpty) return false;
    final keywords =
        config.flakyTestKeywords.map((k) => k.toLowerCase()).toList();
    return pr.failingChecks.every((check) {
      final lower = check.toLowerCase();
      return keywords.any(lower.contains);
    });
  }

  static bool _isActionRequired(String check) {
    final lower = check.toLowerCase();
    return lower.contains('action_required') || lower.contains('action required');
  }

  TriagedItem _lowestRank(List<TriagedItem> candidates) =>
      candidates.reduce((a, b) => a.tierRank <= b.tierRank ? a : b);

  TriagedItem _buildItem({
    required PrItem pr,
    required QueueType queue,
    required TriageTier tier,
    required String reason,
    required int days,
    bool isPrimary = true,
    String? actionOverride,
  }) {
    return TriagedItem(
      pr: pr,
      queue: queue,
      tier: tier,
      reason: reason,
      businessDaysElapsed: days,
      actionOverride: actionOverride,
      isPrimary: isPrimary,
    );
  }

  int _compareTriagedItems(TriagedItem a, TriagedItem b) {
    if (a.tierRank != b.tierRank) return a.tierRank.compareTo(b.tierRank);
    if (a.businessDaysElapsed != b.businessDaysElapsed) {
      return b.businessDaysElapsed.compareTo(a.businessDaysElapsed);
    }
    return b.pr.updatedAt.compareTo(a.pr.updatedAt);
  }

  /// Business days a PR has been waiting, measured from the most recent
  /// review request and falling back to creation date.
  ///
  /// `updatedAt` is deliberately not used: any bot comment or label edit
  /// would reset the clock, so "days waiting" would really mean "days since
  /// anything happened".
  int businessDaysWaiting(PrItem pr, TriageConfig config) {
    final reference = pr.lastReviewRequestedAt ?? pr.createdAt;
    return calculateBusinessDays(reference, _now, holidays: config.holidays);
  }

  /// Business days between [from] and [to], excluding weekends and [holidays].
  ///
  /// Both endpoints are normalised to local time first. GitHub timestamps
  /// parse as UTC while the clock is local, and reading `.year/.month/.day`
  /// off two different calendars produced an off-by-one for any PR touched
  /// during the evening.
  int calculateBusinessDays(
    DateTime from,
    DateTime to, {
    List<DateTime> holidays = const [],
  }) {
    final start = from.toLocal();
    final finish = to.toLocal();
    if (finish.isBefore(start)) return 0;

    final skip = {
      for (final h in holidays) DateTime(h.year, h.month, h.day),
    };

    var cur = DateTime(start.year, start.month, start.day);
    final end = DateTime(finish.year, finish.month, finish.day);
    var count = 0;
    while (cur.isBefore(end)) {
      // Constructing the next date rather than adding a Duration keeps this
      // correct across daylight-saving transitions.
      cur = DateTime(cur.year, cur.month, cur.day + 1);
      if (cur.weekday == DateTime.saturday) continue;
      if (cur.weekday == DateTime.sunday) continue;
      if (skip.contains(cur)) continue;
      count++;
    }
    return count;
  }
}

/// Precomputed review-queue signals, so each rule reads them rather than
/// recomputing and diverging.
class _ReviewSignals {
  const _ReviewSignals({
    required this.isTeamAuthor,
    required this.claMissing,
    required this.hasWaitingLabel,
    required this.blockers,
  });

  factory _ReviewSignals.of(PrItem pr, TriageConfig config) {
    final head = pr.headCommitDate;
    return _ReviewSignals(
      isTeamAuthor: config.isTeamMember(pr.author),
      claMissing: pr.hasAnyLabel(config.claMissingLabels),
      hasWaitingLabel: pr.hasAnyLabel(config.waitingLabels),
      blockers: pr.humanReviews
          .where((r) => r.isChangesRequested)
          .where((r) => head == null || r.submittedAt.isAfter(head))
          .toList(),
    );
  }

  final bool isTeamAuthor;
  final bool claMissing;
  final bool hasWaitingLabel;
  final List<PrReview> blockers;

  bool get isBlocked => claMissing || (blockers.isNotEmpty && !isTeamAuthor);
  bool get waitingOnAuthor => hasWaitingLabel || claMissing;

  String backlogReason(PrItem pr) {
    if (pr.hasFailingCi) {
      return 'CI is failing; likely to change before review is useful';
    }
    return 'No clear next action for a reviewer';
  }
}
