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
      ..sort((a, b) => _compareTriagedItems(a, b, config));

    final reviewQueue =
        reviewPrs.map((pr) => classifyReviewQueue(pr, config)).toList()
          ..sort((a, b) => _compareTriagedItems(a, b, config));

    return (myWork, reviewQueue);
  }

  /// Classifies a PR authored by one of the configured accounts.
  TriagedItem classifyMyWork(PrItem pr, TriageConfig config) {
    final days = businessDaysWaiting(pr, config);
    final override = config.getOverrideFor(pr.repo, pr.number);

    final scoped = _scopeItem(pr, config, QueueType.myWork, days, override);
    if (scoped != null) return scoped;

    final ci = _PrCi.of(pr, config);
    if (pr.isDraft) {
      return _annotate(_draftItem(pr, ci, days), override);
    }

    final candidates = <TriagedItem>[
      ?_checkReadyToMerge(pr, ci, days),
      ?_checkWaitingCicd(pr, ci, config, days),
      ?_checkFlakyCi(pr, ci, config, days),
      ?_checkMinorFeedback(pr, days),
      ?_checkFailingCi(pr, ci, config, days),
      ?_checkSubstantialFeedback(pr, days),
      _reviewAgeItem(pr, config, days),
    ];
    return _annotate(_lowestRank(candidates), override);
  }

  /// Classifies an incoming review request or assignment.
  TriagedItem classifyReviewQueue(PrItem pr, TriageConfig config) {
    final days = businessDaysWaiting(pr, config);
    final override = config.getOverrideFor(pr.repo, pr.number);

    final scoped = _scopeItem(
      pr,
      config,
      QueueType.reviewQueue,
      days,
      override,
    );
    if (scoped != null) return scoped;

    // Someone asking you by @-mention is checked before the team-only, draft,
    // conflict and CI rules: those describe the PR, but the comment is a
    // direct request for your attention.
    final asked = _checkExplicitlyAsked(pr, config);
    if (asked != null) {
      return _annotate(
        _lowestRank([?_checkReReviewReady(pr, config, days), asked]),
        override,
      );
    }

    final signals = _ReviewSignals.of(pr, config);

    // A request that reached you only through a team is checked before the
    // draft rule, so a team-only draft cannot outrank direct requests.
    if (signals.isTeamOnly) {
      return _annotate(_teamOnlyItem(pr, days, signals), override);
    }

    if (pr.isDraft) {
      return _annotate(
        _buildItem(
          pr: pr,
          queue: QueueType.reviewQueue,
          tier: pr.isMergeBlocked
              ? ReviewQueueTier.other
              : ReviewQueueTier.draftReview,
          reason: pr.isMergeBlocked
              ? 'Draft with merge conflicts; the author needs to rebase '
                    'before review is useful'
              : 'Author has not marked this ready for review',
          days: days,
        ),
        override,
      );
    }

    final stalled = _checkCoReviewerStalled(pr, config, days, signals);
    final candidates = <TriagedItem>[
      ?_checkReReviewReady(pr, config, days),
      ?_checkTeamReview(pr, days, signals),
      ?_checkCleanExternal(pr, days, signals),
      ?_checkWaitingOnAuthor(pr, days, signals),
      ?stalled,
      ?_checkBlockedExternal(pr, days, signals),
      _buildItem(
        pr: pr,
        queue: QueueType.reviewQueue,
        tier: ReviewQueueTier.other,
        reason: signals.backlogReason(pr),
        days: days,
      ),
    ];
    final winner = _lowestRank(candidates);
    // A higher tier wins the placement, but the stalled co-reviewer is still
    // worth a ping, so it is kept in the reason rather than dropped.
    if (stalled != null && !identical(winner, stalled)) {
      return _annotate(
        _withReasonNote(
          winner,
          '${_stalledNote(signals, days)} (ping or reassign)',
        ),
        override,
      );
    }
    return _annotate(winner, override);
  }

  // ---------------------------------------------------------------------
  // My Work rules
  // ---------------------------------------------------------------------

  /// Tier 1. Requires a positive CI signal: either checks ran and passed, or
  /// the repository demonstrably runs no checks. A rollup we merely failed to
  /// parse must never produce "[Action: Merge]".
  ///
  /// A PR whose only failures are [TriageConfig.nonPrChecks] is kept out on
  /// purpose. Its own checks may still be running (see [_PrCi]), and the
  /// merge bot holds every PR while the tree is red anyway, so "[Action:
  /// Merge]" would send the author to do something that cannot happen yet.
  /// [_checkWaitingCicd] picks those PRs up instead.
  TriagedItem? _checkReadyToMerge(PrItem pr, _PrCi ci, int days) {
    final ciOk = ci.isPassing || pr.hasNoChecksConfigured;
    if (!pr.isApproved || pr.isMergeBlocked || !ciOk || !_threadsClear(pr)) {
      return null;
    }
    return _buildItem(
      pr: pr,
      queue: QueueType.myWork,
      tier: MyWorkTier.readyToMerge,
      reason: ci.isPassing
          ? 'Approved, CI green, mergeable, no unresolved threads'
          : 'Approved, no CI configured, mergeable, no unresolved threads',
      days: days,
    );
  }

  /// Tier 2. Only fires when the author can actually unblock CI, or when an
  /// otherwise mergeable PR is held only by repository state.
  ///
  /// The presubmit trigger label is scoped per repository because it does not
  /// exist everywhere: telling a `dart-lang` contributor to apply `CICD` asks
  /// them to add a label the repo has never defined. It also requires a
  /// positive "zero checks ran" signal rather than the absence of a rollup,
  /// and never fires on drafts, whose CI is not expected to be running.
  TriagedItem? _checkWaitingCicd(
    PrItem pr,
    _PrCi ci,
    TriageConfig config,
    int days,
  ) {
    if (pr.isDraft || ci.isPassing) return null;

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
    if (trigger != null && !pr.hasLabel(trigger) && pr.hasNoChecksConfigured) {
      return _buildItem(
        pr: pr,
        queue: QueueType.myWork,
        tier: MyWorkTier.waitingOnCicdTask,
        reason: 'No checks have run and the "$trigger" trigger label is absent',
        days: days,
        actionOverride: '[Action: Apply $trigger label to trigger CI]',
      );
    }

    if (ci.ownFailing.any(_isActionRequired)) {
      return _buildItem(
        pr: pr,
        queue: QueueType.myWork,
        tier: MyWorkTier.waitingOnCicdTask,
        reason: 'A check run requires manual approval before it can proceed',
        days: days,
        actionOverride: '[Action: Approve the pending CI run]',
      );
    }

    // Approved and otherwise ready, but the only red checks report the tree
    // or a freeze. Ready to Merge would overstate it (see
    // [_checkReadyToMerge]); falling through to the review-age tiers would
    // call an approved PR "awaiting review".
    if (ci.isRepoStateOnly &&
        pr.isApproved &&
        !pr.isMergeBlocked &&
        _threadsClear(pr)) {
      return _buildItem(
        pr: pr,
        queue: QueueType.myWork,
        tier: MyWorkTier.waitingOnCicdTask,
        reason:
            'Approved, mergeable, no unresolved threads; '
            '${ci.repoStateNote}; merge waits for the tree',
        days: days,
        actionOverride:
            '[Action: Waiting for tree to go green / freeze to resolve]',
      );
    }

    return null;
  }

  /// Tiers 3 and 10. Drafts are handled in one place so their reason string
  /// can carry the CI and review-thread state instead of discarding it.
  TriagedItem _draftItem(PrItem pr, _PrCi ci, int days) {
    final blockers = _activeBlockingReviews(pr);
    final clean =
        ci.isPassing &&
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
      reason: 'Draft: ${_draftBlockers(pr, ci, blockers).join('; ')}',
      days: days,
    );
  }

  List<String> _draftBlockers(PrItem pr, _PrCi ci, List<PrReview> blockers) {
    final parts = <String>[
      if (ci.isFailing) 'CI failing (${ci.ownFailing.length} checks)',
      if (ci.isRepoStateOnly)
        '${ci.repoStateNote}; PR checks not confirmed green',
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
  TriagedItem? _checkFlakyCi(
    PrItem pr,
    _PrCi ci,
    TriageConfig config,
    int days,
  ) {
    if (!ci.isFailing || !_isFlakyCiFailure(ci, config)) return null;
    return _buildItem(
      pr: pr,
      queue: QueueType.myWork,
      tier: MyWorkTier.flakyCiFailure,
      reason:
          'CI failed only on known flaky checks: '
          '${ci.ownFailing.join(', ')}',
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
  TriagedItem? _checkFailingCi(
    PrItem pr,
    _PrCi ci,
    TriageConfig config,
    int days,
  ) {
    if (!ci.isFailing || _isFlakyCiFailure(ci, config)) return null;
    return _buildItem(
      pr: pr,
      queue: QueueType.myWork,
      tier: MyWorkTier.failingCiWorkRelated,
      reason:
          'CI failed on checks requiring code fixes: '
          '${ci.ownFailing.join(', ')}',
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

  /// Tier 2: a human @-mentioned one of your accounts in a PR comment within
  /// [TriageConfig.mentionWindowBusinessDays], and you have not reviewed
  /// since.
  ///
  /// Draft state, merge conflicts and CI status do not rule this out: the
  /// person asking may want an early design read, and the reason string
  /// reports those states so you can judge.
  TriagedItem? _checkExplicitlyAsked(PrItem pr, TriageConfig config) {
    final mention = pr.latestMentionOf(config.isMyAccount);
    if (mention == null) return null;

    final mine = pr.latestReviewBy(config.isMyAccount);
    if (mine != null && !mention.createdAt.isAfter(mine.submittedAt)) {
      return null;
    }

    final since = calculateBusinessDays(
      mention.createdAt,
      _now,
      holidays: config.holidays,
    );
    if (since > config.mentionWindowBusinessDays) return null;

    final approvals = pr.humanReviews
        .where((r) => r.isApproved && !config.isMyAccount(r.author))
        .map((r) => r.author.toLowerCase())
        .toSet()
        .length;
    final ci = _PrCi.of(pr, config);
    final parts = <String>[
      '${mention.author} asked for your review $since business days ago '
          '("${mention.excerpt}")',
      if (approvals > 0)
        '$approvals approval${approvals == 1 ? '' : 's'} from others',
      if (pr.isDraft) 'draft',
      if (pr.isMergeBlocked) 'merge conflicts',
      if (ci.isFailing) 'CI failing',
      if (ci.isRepoStateOnly) ci.repoStateNote,
    ];
    return _buildItem(
      pr: pr,
      queue: QueueType.reviewQueue,
      tier: ReviewQueueTier.explicitlyAsked,
      reason: parts.join('; '),
      days: since,
    );
  }

  /// Tier 3.
  TriagedItem? _checkTeamReview(PrItem pr, int days, _ReviewSignals s) {
    if (!s.isTeamAuthor || s.isBlocked || s.waitingOnAuthor) return null;
    if (s.ci.isFailing || pr.isMergeBlocked) return null;
    return _buildItem(
      pr: pr,
      queue: QueueType.reviewQueue,
      tier: ReviewQueueTier.teamReviewRequest,
      reason: 'Teammate ${pr.author} is waiting on review (CI: ${s.ci.label})',
      days: days,
    );
  }

  /// Tier 4.
  TriagedItem? _checkCleanExternal(PrItem pr, int days, _ReviewSignals s) {
    if (s.isTeamAuthor || s.isBlocked || s.waitingOnAuthor) return null;
    if (s.ci.isFailing || pr.isMergeBlocked) return null;
    return _buildItem(
      pr: pr,
      queue: QueueType.reviewQueue,
      tier: ReviewQueueTier.cleanExternalPr,
      reason: 'External contributor PR with no blockers (CI: ${s.ci.label})',
      days: days,
    );
  }

  /// Tier 5.
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

  /// Tier 7. You were asked by name and another individually requested
  /// reviewer has not reviewed in [TriageConfig.staleCoReviewerBusinessDays].
  ///
  /// Failing CI does not rule this out: a ping is useful either way. The age
  /// is the PR's most recent review request, not each reviewer's own.
  TriagedItem? _checkCoReviewerStalled(
    PrItem pr,
    TriageConfig config,
    int days,
    _ReviewSignals s,
  ) {
    if (!s.requestedDirectly || pr.isMergeBlocked) return null;
    if (s.claMissing || s.hasWaitingLabel) return null;
    if (s.silentCoReviewers.isEmpty) return null;
    if (days < config.staleCoReviewerBusinessDays) return null;
    return _buildItem(
      pr: pr,
      queue: QueueType.reviewQueue,
      tier: ReviewQueueTier.coReviewerStalled,
      reason: '${_stalledNote(s, days)} (CI: ${s.ci.label})',
      days: days,
    );
  }

  static String _stalledNote(_ReviewSignals s, int days) =>
      '${s.silentCoReviewers.join(', ')} requested $days business days ago '
      'with no review';

  static TriagedItem _withReasonNote(TriagedItem item, String note) =>
      TriagedItem(
        pr: item.pr,
        queue: item.queue,
        tier: item.tier,
        reason: '${item.reason}; $note',
        businessDaysElapsed: item.businessDaysElapsed,
        actionOverride: item.actionOverride,
        isPrimary: item.isPrimary,
      );

  /// Tier 12. The request reached you only through a team.
  TriagedItem _teamOnlyItem(PrItem pr, int days, _ReviewSignals s) {
    final parts = <String>[
      'Requested from ${pr.requestedTeams.join(', ')} (not you)',
      '${s.peopleCount} people already on it${s.isCrowded ? ' (crowded)' : ''}',
      if (pr.isMergeBlocked) 'merge conflicts',
      if (pr.isDraft) 'draft',
    ];
    return _buildItem(
      pr: pr,
      queue: QueueType.reviewQueue,
      tier: ReviewQueueTier.teamOnlyRequest,
      reason: parts.join('; '),
      days: days,
    );
  }

  /// Tier 8. Emits an honest reason naming whichever condition actually
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

  /// True when every unresolved thread is accounted for and none is open.
  static bool _threadsClear(PrItem pr) =>
      pr.unresolvedThreadsExact && pr.unresolvedReviewThreads == 0;

  bool _isFlakyCiFailure(_PrCi ci, TriageConfig config) {
    if (ci.ownFailing.isEmpty) return false;
    final keywords = config.flakyTestKeywords
        .map((k) => k.toLowerCase())
        .toList();
    return ci.ownFailing.every((check) {
      final lower = check.toLowerCase();
      return keywords.any(lower.contains);
    });
  }

  static bool _isActionRequired(String check) {
    final lower = check.toLowerCase();
    return lower.contains('action_required') ||
        lower.contains('action required');
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

  int _compareTriagedItems(TriagedItem a, TriagedItem b, TriageConfig config) {
    if (a.tierRank != b.tierRank) return a.tierRank.compareTo(b.tierRank);
    if (a.tier == ReviewQueueTier.teamOnlyRequest) {
      final aCrowded = _ReviewSignals.of(a.pr, config).isCrowded;
      final bCrowded = _ReviewSignals.of(b.pr, config).isCrowded;
      if (aCrowded != bCrowded) return aCrowded ? 1 : -1;
    }
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

    final skip = {for (final h in holidays) DateTime(h.year, h.month, h.day)};

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
    required this.requestedDirectly,
    required this.isTeamOnly,
    required this.peopleCount,
    required this.isCrowded,
    required this.silentCoReviewers,
    required this.ci,
  });

  factory _ReviewSignals.of(PrItem pr, TriageConfig config) {
    final head = pr.headCommitDate;
    final requestedDirectly = pr.requestedReviewers.any(config.isMyAccount);
    final assigned = pr.assignedReviewers.any(config.isMyAccount);
    final reviewedBefore = pr.latestReviewBy(config.isMyAccount) != null;
    final people = <String>{
      for (final login in pr.requestedReviewers)
        if (!config.isMyAccount(login) && !_isBotLogin(login))
          login.toLowerCase(),
      for (final r in pr.humanReviews)
        if (!config.isMyAccount(r.author)) r.author.toLowerCase(),
    };
    return _ReviewSignals(
      isTeamAuthor: config.isTeamMember(pr.author),
      claMissing: pr.hasAnyLabel(config.claMissingLabels),
      hasWaitingLabel: pr.hasAnyLabel(config.waitingLabels),
      blockers: pr.humanReviews
          .where((r) => r.isChangesRequested)
          .where((r) => head == null || r.submittedAt.isAfter(head))
          .toList(),
      requestedDirectly: requestedDirectly,
      isTeamOnly:
          pr.requestedTeams.isNotEmpty &&
          !requestedDirectly &&
          !assigned &&
          !reviewedBefore,
      peopleCount: people.length,
      isCrowded: people.length >= config.crowdedReviewThreshold,
      // GitHub removes a reviewer from requestedReviewers once they review,
      // so everyone still listed has not reviewed since being asked.
      silentCoReviewers: [
        for (final login in pr.requestedReviewers)
          if (!config.isMyAccount(login) && !_isBotLogin(login)) login,
      ],
      ci: _PrCi.of(pr, config),
    );
  }

  static bool _isBotLogin(String login) {
    final lower = login.toLowerCase();
    return lower.endsWith('[bot]') || kBotReviewerLogins.contains(lower);
  }

  final bool isTeamAuthor;
  final bool claMissing;
  final bool hasWaitingLabel;
  final List<PrReview> blockers;

  /// One of your accounts is in `requestedReviewers` (asked by name).
  final bool requestedDirectly;

  /// Only a team was asked: you were not asked by name, are not assigned,
  /// and have never reviewed the PR.
  final bool isTeamOnly;

  /// Distinct people other than you who are requested or have reviewed.
  final int peopleCount;
  final bool isCrowded;

  /// Other individually requested reviewers who have not reviewed.
  final List<String> silentCoReviewers;

  /// The PR's CI with [TriageConfig.nonPrChecks] set aside.
  final _PrCi ci;

  bool get isBlocked => claMissing || (blockers.isNotEmpty && !isTeamAuthor);
  bool get waitingOnAuthor => hasWaitingLabel || claMissing;

  String backlogReason(PrItem pr) {
    if (pr.isMergeBlocked) {
      return 'Merge conflicts; the author needs to rebase before review is '
          'useful';
    }
    if (ci.isFailing) {
      return 'CI is failing; likely to change before review is useful';
    }
    return 'No clear next action for a reviewer';
  }
}

/// A PR's CI state with [TriageConfig.nonPrChecks] set aside, so that a red
/// tree or a code freeze is never read as the PR's own CI failing.
///
/// Every rule reads CI through this rather than [PrItem.hasFailingCi],
/// [PrItem.hasPassingCi] or [PrItem.failingChecks], which keeps [PrItem]
/// free of configuration.
///
/// A failing rollup whose only named failures are non-PR checks
/// ([isRepoStateOnly]) is deliberately neither failing nor passing.
/// `PrItem._extractCiInfo` keeps only the names of failing checks, and
/// GitHub's rollup reports `FAILURE` ahead of `PENDING`, so in that state the
/// PR's own checks may have passed or may still be running. Nothing here
/// claims green for it.
class _PrCi {
  const _PrCi._({
    required this.status,
    required this.ownFailing,
    required this.repoStateFailing,
  });

  factory _PrCi.of(PrItem pr, TriageConfig config) {
    final own = <String>[];
    final repoState = <String>[];
    for (final check in pr.failingChecks) {
      (config.isNonPrCheck(check) ? repoState : own).add(check);
    }
    return _PrCi._(
      status: pr.ciStatus,
      ownFailing: own,
      repoStateFailing: repoState,
    );
  }

  final CiStatus status;

  /// Failing checks that belong to the PR.
  final List<String> ownFailing;

  /// Failing checks that report repository or release state.
  final List<String> repoStateFailing;

  /// The rollup is red, and every named failure is a non-PR check.
  bool get isRepoStateOnly =>
      status == CiStatus.failing &&
      repoStateFailing.isNotEmpty &&
      ownFailing.isEmpty;

  /// The PR's own CI is failing. A red rollup with no named failures still
  /// counts: the failing check may sit beyond the contexts that were fetched.
  bool get isFailing => status == CiStatus.failing && !isRepoStateOnly;

  /// Checks ran and passed. Never true when [isRepoStateOnly].
  bool get isPassing => status == CiStatus.passing;

  /// For example `tree-status red (repository state, not this PR)`.
  String get repoStateNote =>
      '${{...repoStateFailing}.join(', ')} red '
      '(repository state, not this PR)';

  /// Status for `CI: ...` reason fragments. Never says "failing" when only
  /// non-PR checks are red.
  String get label => isRepoStateOnly
      ? 'no PR check failing; ${{...repoStateFailing}.join(', ')} red'
      : status.name;
}
