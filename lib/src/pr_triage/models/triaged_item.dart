import 'pr_item.dart';

/// Which of the two triage queues an item belongs to.
enum QueueType { myWork, reviewQueue }

/// Common contract for a priority tier in either queue.
///
/// Implemented by [MyWorkTier] and [ReviewQueueTier] so that shared plumbing
/// can accept a tier without resorting to `dynamic` dispatch.
abstract interface class TriageTier {
  /// Lower is more urgent. Ranks are queue-local and are not comparable
  /// across queues.
  int get rank;

  /// Human readable tier name shown in reports.
  String get displayName;

  /// Action prompt emitted when no per-PR override supplies one.
  String get defaultAction;
}

/// Priority tiers for pull requests authored by the configured accounts.
///
/// Ranks are dense and ordered; [PrClassifier] selects the lowest applicable
/// rank rather than relying on the order of `if` statements, so this list is
/// the single source of truth for priority.
enum MyWorkTier implements TriageTier {
  readyToMerge(1, 'Ready to Merge', '[Action: Merge]'),
  waitingOnCicdTask(
    2,
    'Waiting on CI/CD Task / Action Required',
    '[Action: Unblock CI/CD task / Trigger CI]',
  ),
  draftReadyForReview(
    3,
    'Draft Ready for Review (CI Green)',
    '[Action: Mark ready for review & notify reviewers]',
  ),
  flakyCiFailure(
    4,
    'Failing CI (Flaky candidate)',
    '[Action: Investigate/re-run flaky CI]',
  ),
  minorFeedbackWithApproval(
    5,
    'Approved with Minor Comments',
    '[Action: Address minor comments & land]',
  ),
  failingCiWorkRelated(
    6,
    'Failing CI (Needs Code Fix)',
    '[Action: Fix failing tests/lints]',
  ),
  substantialFeedback(
    7,
    'Substantial Review Feedback / Changes Requested',
    '[Action: Address review feedback]',
  ),
  stalledInReview(
    8,
    'Stalled in Review (>= threshold business days)',
    '[Action: Ping reviewer(s)]',
  ),
  freshInReview(
    9,
    'In Review (within normal window)',
    '[Action: Awaiting review]',
  ),
  draft(
    10,
    'Active Draft / WIP (Primary Repo)',
    '[Action: Resume development / Work in progress]',
  ),
  nonPrimaryRepo(
    11,
    'Repository Not In primary_orgs',
    '[Action: Low urgency - add repo to primary_orgs to promote]',
  ),
  forkOrPoc(12, 'Personal Fork / POC', '[Action: POC / Fork - Low urgency]');

  const MyWorkTier(this.rank, this.displayName, this.defaultAction);

  @override
  final int rank;
  @override
  final String displayName;
  @override
  final String defaultAction;

  /// Looks up a tier by its enum name, for `pr_overrides` that pin a tier.
  static MyWorkTier? byName(String name) {
    for (final tier in values) {
      if (tier.name.toLowerCase() == name.toLowerCase()) return tier;
    }
    return null;
  }
}

/// Priority tiers for incoming review requests and assignments.
enum ReviewQueueTier implements TriageTier {
  reReviewReady(
    1,
    'Re-Review Ready (Feedback Addressed)',
    '[Action: Re-review and sign off]',
  ),
  teamReviewRequest(2, 'Teammate PR Review', '[Action: Review team PR]'),
  cleanExternalPr(
    3,
    'Clean External Contributor PR',
    '[Action: Review external PR]',
  ),
  waitingOnAuthor(
    4,
    'Waiting on Author Response',
    '[Action: Awaiting author updates]',
  ),
  draftReview(
    5,
    'Draft PR in Review Queue',
    '[Action: Deprioritized - Awaiting author to mark ready for review]',
  ),
  coReviewerStalled(
    6,
    'Co-Reviewer Stalled',
    '[Action: Ping co-reviewer(s) or reassign]',
  ),
  blockedExternalPr(
    7,
    'Blocked External PR (CLA/Blockers)',
    '[Action: Low priority (blocked)]',
  ),
  nonPrimaryRepoReview(
    8,
    'Review in Repository Not In primary_orgs',
    '[Action: Low urgency - add repo to primary_orgs to promote]',
  ),
  forkOrPocReview(
    9,
    'Personal Fork Review',
    '[Action: Fork Review - Low urgency]',
  ),
  other(10, 'Other / Backlog', '[Action: Monitor]'),
  teamOnlyRequest(
    11,
    'Team-Only Request (not addressed to you)',
    '[Action: Low priority - team request, not addressed to you]',
  );

  const ReviewQueueTier(this.rank, this.displayName, this.defaultAction);

  @override
  final int rank;
  @override
  final String displayName;
  @override
  final String defaultAction;

  /// Looks up a tier by its enum name, for `pr_overrides` that pin a tier.
  static ReviewQueueTier? byName(String name) {
    for (final tier in values) {
      if (tier.name.toLowerCase() == name.toLowerCase()) return tier;
    }
    return null;
  }
}

/// A pull request paired with the tier it was classified into.
class TriagedItem {
  const TriagedItem({
    required this.pr,
    required this.queue,
    required this.tier,
    required this.reason,
    required this.businessDaysElapsed,
    this.actionOverride,
    this.isPrimary = true,
  });

  final PrItem pr;
  final QueueType queue;
  final TriageTier tier;
  final String reason;
  final int businessDaysElapsed;

  /// Set when a `pr_overrides` entry supplies a bespoke action string.
  final String? actionOverride;
  final bool isPrimary;

  String get tierName => tier.displayName;
  int get tierRank => tier.rank;
  String get actionPrompt => actionOverride ?? tier.defaultAction;

  /// Compact `{repo, number, url}` reference, used by the `top_*` arrays so
  /// items are not serialized in full more than once.
  Map<String, dynamic> toRefJson() => {
    'repo': pr.repo,
    'number': pr.number,
    'title': pr.title,
    'url': pr.url,
    'queue': queue.name,
    'tier_name': tierName,
    'tier_rank': tierRank,
    'action_prompt': actionPrompt,
    'reason': reason,
    'business_days_elapsed': businessDaysElapsed,
  };

  Map<String, dynamic> toJson() => {
    'pr': pr.toJson(),
    'queue': queue.name,
    'tier': (tier as Enum).name,
    'tier_name': tierName,
    'tier_rank': tierRank,
    'action_prompt': actionPrompt,
    'reason': reason,
    'business_days_elapsed': businessDaysElapsed,
    'is_primary': isPrimary,
  };
}
