/// Represents the overall status of CI checks on a PR.
enum CiStatus {
  passing,
  failing,
  pending,

  /// No rollup was present on the head commit. Callers must pair this with
  /// [PrItem.totalCheckCount] to distinguish "this repo runs no checks" from
  /// "we could not read the rollup".
  none;

  static CiStatus fromString(String? state) {
    if (state == null) return CiStatus.none;
    final normalized = state.toUpperCase();
    return switch (normalized) {
      'SUCCESS' || 'PASSED' => CiStatus.passing,
      'FAILURE' ||
      'FAILED' ||
      'ERROR' ||
      'TIMED_OUT' ||
      'ACTION_REQUIRED' =>
        CiStatus.failing,
      'PENDING' || 'EXPECTED' || 'IN_PROGRESS' || 'QUEUED' => CiStatus.pending,
      _ => CiStatus.none,
    };
  }
}

/// Logins that publish automated reviews. Their verdicts must never gate
/// human triage decisions.
const Set<String> kBotReviewerLogins = {
  'gemini-code-assist',
  'github-actions',
  'dependabot',
  'auto-submit',
  'flutter-dashboard',
  'copilot-pull-request-reviewer',
};

/// Represents a review left on a PR.
class PrReview {
  const PrReview({
    required this.author,
    required this.state,
    required this.submittedAt,
  });

  factory PrReview.fromJson(Map<String, dynamic> json) {
    return PrReview(
      author: (json['author']?['login'] ?? json['author'] ?? '').toString(),
      state: (json['state'] ?? 'COMMENTED').toString().toUpperCase(),
      submittedAt: DateTime.tryParse(json['submittedAt']?.toString() ?? '') ??
          DateTime.fromMillisecondsSinceEpoch(0, isUtc: true),
    );
  }

  final String author;
  final String state; // APPROVED, CHANGES_REQUESTED, COMMENTED, DISMISSED
  final DateTime submittedAt;

  bool get isApproved => state == 'APPROVED';
  bool get isChangesRequested => state == 'CHANGES_REQUESTED';
  bool get isCommented => state == 'COMMENTED';

  /// True for automated reviewers, matched by suffix or known-login set.
  bool get isBot {
    final lower = author.toLowerCase();
    return lower.endsWith('[bot]') || kBotReviewerLogins.contains(lower);
  }

  Map<String, dynamic> toJson() => {
        'author': author,
        'state': state,
        'submitted_at': submittedAt.toIso8601String(),
      };
}

/// Represents a Pull Request with all enriched triage metadata.
class PrItem {
  const PrItem({
    required this.number,
    required this.title,
    required this.url,
    required this.repo,
    required this.author,
    this.isDraft = false,
    this.mergeable = 'MERGEABLE',
    this.reviewDecision = 'NONE',
    this.ciStatus = CiStatus.none,
    this.totalCheckCount = 0,
    this.failingChecks = const [],
    this.unresolvedReviewThreads = 0,
    this.totalReviewThreads = 0,
    this.unresolvedThreadsExact = true,
    this.labels = const [],
    required this.updatedAt,
    required this.createdAt,
    this.headCommitDate,
    this.lastReviewRequestedAt,
    this.latestReviews = const [],
    this.allReviews = const [],
    this.requestedReviewers = const [],
    this.requestedTeams = const [],
    this.assignedReviewers = const [],
  });

  factory PrItem.fromJson(Map<String, dynamic> json) {
    final repoName = (json['repository']?['nameWithOwner'] ??
            json['repository']?['name'] ??
            json['repo'] ??
            '')
        .toString();

    final authorName =
        (json['author']?['login'] ?? json['author'] ?? '').toString();

    final ci = _extractCiInfo(json);

    return PrItem(
      number: (json['number'] ?? 0) as int,
      title: (json['title'] ?? '').toString(),
      url: (json['url'] ?? '').toString(),
      repo: repoName,
      author: authorName,
      isDraft: (json['isDraft'] ?? false) as bool,
      mergeable: (json['mergeable'] ?? 'UNKNOWN').toString().toUpperCase(),
      reviewDecision:
          (json['reviewDecision'] ?? 'NONE').toString().toUpperCase(),
      ciStatus: ci.status,
      totalCheckCount: ci.totalCheckCount,
      failingChecks: ci.failingChecks,
      unresolvedReviewThreads: (json['unresolvedReviewThreads'] ??
              json['unresolved_review_threads'] ??
              0) as int,
      totalReviewThreads: (json['totalReviewThreads'] ??
              json['total_review_threads'] ??
              0) as int,
      unresolvedThreadsExact: (json['unresolvedThreadsExact'] ??
              json['unresolved_threads_exact'] ??
              true) as bool,
      labels: _extractLabels(json['labels']),
      updatedAt: _parseDate(json['updatedAt']) ?? DateTime.now(),
      createdAt: _parseDate(json['createdAt']) ?? DateTime.now(),
      headCommitDate:
          _parseDate(json['headCommitDate'] ?? json['head_commit_date']),
      lastReviewRequestedAt: _parseDate(
          json['lastReviewRequestedAt'] ?? json['last_review_requested_at']),
      latestReviews: _extractReviews(json['latestReviews'] ?? json['reviews']),
      allReviews: _extractReviews(json['allReviews'] ?? json['all_reviews']),
      requestedReviewers: _extractStringList(
          json['reviewRequests'] ?? json['requestedReviewers']),
      requestedTeams:
          _extractTeamList(json['reviewRequests'] ?? json['requestedTeams']),
      assignedReviewers: _extractStringList(json['assignees']),
    );
  }

  final int number;
  final String title;
  final String url;
  final String repo;
  final String author;
  final bool isDraft;

  /// GitHub's lazily-computed mergeability: `MERGEABLE`, `CONFLICTING`, or
  /// `UNKNOWN`. `UNKNOWN` is extremely common on a first read because the
  /// first request merely enqueues the background computation, so triage
  /// treats it as "not disqualifying" rather than "not mergeable".
  final String mergeable;
  final String reviewDecision;
  final CiStatus ciStatus;

  /// Number of check contexts on the head commit. Zero means the rollup was
  /// read successfully and genuinely contained no checks.
  final int totalCheckCount;
  final List<String> failingChecks;
  final int unresolvedReviewThreads;
  final int totalReviewThreads;

  /// False when more review threads exist than were fetched, in which case
  /// [unresolvedReviewThreads] is a lower bound and `== 0` must not be trusted.
  final bool unresolvedThreadsExact;
  final List<String> labels;
  final DateTime updatedAt;
  final DateTime createdAt;

  /// Commit date of the head commit; the only reliable "new work exists"
  /// signal. [updatedAt] moves on label edits and bot comments.
  final DateTime? headCommitDate;
  final DateTime? lastReviewRequestedAt;

  /// At most one review per author, as returned by GitHub's `latestReviews`.
  /// Note GitHub omits a reviewer here once a re-review is requested from
  /// them, so this must not be used to answer "did I review this?".
  final List<PrReview> latestReviews;

  /// Full recent review history including reviews absent from
  /// [latestReviews]. Use this to find your own past reviews.
  final List<PrReview> allReviews;
  final List<String> requestedReviewers;
  final List<String> requestedTeams;
  final List<String> assignedReviewers;

  bool get isApproved => reviewDecision == 'APPROVED';
  bool get isChangesRequested => reviewDecision == 'CHANGES_REQUESTED';
  bool get hasFailingCi => ciStatus == CiStatus.failing;
  bool get hasPassingCi => ciStatus == CiStatus.passing;

  /// True only when GitHub positively reported a conflict. `UNKNOWN` is not a
  /// conflict, it is an absence of information.
  bool get isMergeBlocked => mergeable == 'CONFLICTING' || mergeable == 'DIRTY';

  /// True when the rollup was read and reported zero checks, i.e. the repo
  /// genuinely runs no CI on this PR.
  bool get hasNoChecksConfigured =>
      totalCheckCount == 0 && ciStatus == CiStatus.none;

  /// Reviews left by humans. Bot verdicts never block or promote a PR.
  List<PrReview> get humanReviews =>
      latestReviews.where((r) => !r.isBot).toList();

  /// Case-insensitive exact label match.
  bool hasLabel(String label) {
    final target = label.toLowerCase();
    return labels.any((l) => l.toLowerCase() == target);
  }

  /// Case-insensitive exact match against any of [candidates]. A candidate
  /// ending in `*` is treated as a prefix pattern.
  bool hasAnyLabel(Iterable<String> candidates) {
    final lowered = labels.map((l) => l.toLowerCase()).toList();
    for (final candidate in candidates) {
      final target = candidate.toLowerCase().trim();
      if (target.isEmpty) continue;
      if (target.endsWith('*')) {
        final prefix = target.substring(0, target.length - 1);
        if (lowered.any((l) => l.startsWith(prefix))) return true;
      } else if (lowered.contains(target)) {
        return true;
      }
    }
    return false;
  }

  /// The most recent review left by any login in [logins], or null.
  PrReview? latestReviewBy(bool Function(String login) logins) {
    final mine = <PrReview>[
      ...allReviews.where((r) => logins(r.author)),
      ...latestReviews.where((r) => logins(r.author)),
    ];
    if (mine.isEmpty) return null;
    return mine.reduce((a, b) => a.submittedAt.isAfter(b.submittedAt) ? a : b);
  }

  static DateTime? _parseDate(dynamic value) {
    if (value == null) return null;
    return DateTime.tryParse(value.toString());
  }

  static List<String> _extractLabels(dynamic labelsJson) {
    if (labelsJson is List) {
      return labelsJson
          .map((e) => (e is Map ? e['name'] : e).toString().trim())
          .where((s) => s.isNotEmpty)
          .toList();
    }
    if (labelsJson is Map && labelsJson['nodes'] is List) {
      return (labelsJson['nodes'] as List)
          .map((e) => e['name']?.toString().trim() ?? '')
          .where((s) => s.isNotEmpty)
          .toList();
    }
    return const [];
  }

  static List<PrReview> _extractReviews(dynamic reviewsJson) {
    final nodes = _nodesOf(reviewsJson);
    if (nodes == null) return const [];
    return nodes
        .whereType<Map<String, dynamic>>()
        .map(PrReview.fromJson)
        .where((r) => r.author.isNotEmpty)
        .toList();
  }

  static List<dynamic>? _nodesOf(dynamic json) {
    if (json is List) return json;
    if (json is Map && json['nodes'] is List) return json['nodes'] as List;
    return null;
  }

  static ({CiStatus status, int totalCheckCount, List<String> failingChecks})
      _extractCiInfo(Map<String, dynamic> json) {
    final rollup = json['statusCheckRollup'] ?? _headCommitRollup(json);

    if (rollup is Map) {
      final status = CiStatus.fromString(rollup['state']?.toString());
      final contextsJson = rollup['contexts'];
      final total = (contextsJson is Map ? contextsJson['totalCount'] : null);
      final contexts = _nodesOf(contextsJson) ??
          (contextsJson is List ? contextsJson : const []);
      return (
        status: status,
        totalCheckCount: (total as int?) ?? contexts.length,
        failingChecks: _extractFailingChecksFromContexts(contexts),
      );
    }

    if (json['ciStatus'] != null) {
      final status = CiStatus.fromString(json['ciStatus'].toString());
      // A caller-supplied status implies at least one check exists unless it
      // explicitly said otherwise.
      final declared = json['totalCheckCount'] as int?;
      return (
        status: status,
        totalCheckCount: declared ?? (status == CiStatus.none ? 0 : 1),
        failingChecks: _extractStringList(json['failingChecks']),
      );
    }

    return (
      status: CiStatus.none,
      totalCheckCount: (json['totalCheckCount'] as int?) ?? 0,
      failingChecks: _extractStringList(json['failingChecks']),
    );
  }

  /// Safely reaches `commits.nodes[0].commit.statusCheckRollup`.
  ///
  /// Uses `firstOrNull` semantics rather than `[0]`, which throws `RangeError`
  /// on a PR whose commit list came back empty.
  static dynamic _headCommitRollup(Map<String, dynamic> json) {
    final nodes = _nodesOf(json['commits']);
    if (nodes == null || nodes.isEmpty) return null;
    final first = nodes.first;
    if (first is! Map) return null;
    return first['commit']?['statusCheckRollup'];
  }

  static List<String> _extractFailingChecksFromContexts(dynamic contexts) {
    if (contexts is! List) return const [];
    final failing = <String>[];
    for (final ctx in contexts) {
      if (ctx is Map) {
        final state =
            (ctx['conclusion'] ?? ctx['state'])?.toString().toUpperCase();
        if (_isFailingCheckState(state)) {
          final name = (ctx['name'] ?? ctx['context'] ?? '').toString().trim();
          if (name.isNotEmpty) failing.add(name);
        }
      }
    }
    return failing;
  }

  static bool _isFailingCheckState(String? state) {
    return state == 'FAILURE' ||
        state == 'FAILED' ||
        state == 'ERROR' ||
        state == 'TIMED_OUT' ||
        state == 'ACTION_REQUIRED';
  }

  /// Extracts user logins, transparently unwrapping GitHub's
  /// `reviewRequests { nodes { requestedReviewer { login } } }` shape.
  static List<String> _extractStringList(dynamic list) {
    final nodes = _nodesOf(list);
    if (nodes == null) return const [];
    return nodes
        .map((e) {
          if (e is! Map) return e.toString().trim();
          final inner = e['requestedReviewer'] ?? e;
          if (inner is! Map) return '';
          return (inner['login'] ?? '').toString().trim();
        })
        .where((s) => s.isNotEmpty)
        .toList();
  }

  /// Extracts team slugs from `reviewRequests`. Flutter routes most reviews
  /// through teams, so dropping these loses the majority of the signal.
  static List<String> _extractTeamList(dynamic list) {
    final nodes = _nodesOf(list);
    if (nodes == null) return const [];
    return nodes
        .map((e) {
          if (e is! Map) return '';
          final inner = e['requestedReviewer'] ?? e;
          if (inner is! Map) return '';
          return (inner['slug'] ?? inner['name'] ?? '').toString().trim();
        })
        .where((s) => s.isNotEmpty)
        .toList();
  }

  Map<String, dynamic> toJson() => {
        'number': number,
        'title': title,
        'url': url,
        'repo': repo,
        'author': author,
        'is_draft': isDraft,
        'mergeable': mergeable,
        'review_decision': reviewDecision,
        'ci_status': ciStatus.name,
        'total_check_count': totalCheckCount,
        'failing_checks': failingChecks,
        'unresolved_review_threads': unresolvedReviewThreads,
        'total_review_threads': totalReviewThreads,
        'unresolved_threads_exact': unresolvedThreadsExact,
        'labels': labels,
        'updated_at': updatedAt.toIso8601String(),
        'created_at': createdAt.toIso8601String(),
        'head_commit_date': headCommitDate?.toIso8601String(),
        'last_review_requested_at': lastReviewRequestedAt?.toIso8601String(),
        'latest_reviews': latestReviews.map((r) => r.toJson()).toList(),
        'requested_reviewers': requestedReviewers,
        'requested_teams': requestedTeams,
        'assigned_reviewers': assignedReviewers,
      };
}
