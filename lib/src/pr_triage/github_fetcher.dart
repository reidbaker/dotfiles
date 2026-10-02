import 'dart:convert';
import 'dart:io';

import 'models/pr_item.dart';
import 'models/triage_config.dart';

typedef CommandRunner = Future<ProcessResult> Function(
  String executable,
  List<String> arguments,
);

/// Default process runner, extracted so tests can inject a fake.
Future<ProcessResult> defaultCommandRunner(
  String executable,
  List<String> arguments,
) => Process.run(executable, arguments);

/// Result of a PR search, including whether GitHub had more results than the
/// configured page size allowed us to read.
class PrSearchResult {
  const PrSearchResult({required this.items, this.truncated = false});

  final List<PrItem> items;

  /// True when GitHub reported more matches than were returned, so the caller
  /// can say "showing 50 of 137" instead of silently dropping the remainder.
  final bool truncated;
}

class GitHubPrFetcher {
  GitHubPrFetcher({CommandRunner? runner})
    : _runner = runner ?? defaultCommandRunner;

  final CommandRunner _runner;

  /// Fetches the authenticated GitHub username.
  Future<String?> getCurrentUser() async {
    try {
      final res = await _runner('gh', ['api', 'user', '--jq', '.login']);
      if (res.exitCode == 0) {
        return (res.stdout as String).trim();
      }
    } catch (_) {
      // Ignore and fallback
    }
    return null;
  }

  /// Fetches open PRs authored by any of the accounts in [authors].
  ///
  /// GitHub's search syntax treats whitespace between qualifiers as `AND` and
  /// supports no grouping, so `author:a author:b` matches nothing. Each
  /// account therefore gets its own query and the results are merged here.
  Future<PrSearchResult> fetchAuthored({
    required List<String> authors,
    int limit = 50,
  }) async {
    if (authors.isEmpty) return const PrSearchResult(items: []);
    final results = await Future.wait([
      for (final author in authors)
        _searchPrsViaGraphQL('is:pr is:open author:$author', limit: limit),
    ]);
    return _merge(results);
  }

  /// Fetches open PRs requesting review from any of the accounts in
  /// [reviewers], optionally including PRs assigned to them.
  Future<PrSearchResult> fetchReviewQueue({
    required List<String> reviewers,
    int limit = 50,
    bool includeAssignee = false,
  }) async {
    if (reviewers.isEmpty) return const PrSearchResult(items: []);
    final queries = <String>[
      for (final reviewer in reviewers)
        'is:pr is:open review-requested:$reviewer',
      if (includeAssignee)
        for (final reviewer in reviewers) 'is:pr is:open assignee:$reviewer',
    ];
    final results = await Future.wait([
      for (final query in queries) _searchPrsViaGraphQL(query, limit: limit),
    ]);
    return _merge(results);
  }

  /// Re-reads `mergeable` for PRs whose first read returned `UNKNOWN`.
  ///
  /// GitHub computes mergeability lazily: the first request enqueues a
  /// background job and returns `UNKNOWN`. Roughly a third of a typical run
  /// comes back unknown, which previously disqualified those PRs from the
  /// "ready to merge" and "draft ready for review" tiers entirely.
  Future<List<PrItem>> refreshUnknownMergeable(
    List<PrItem> items, {
    Duration delay = const Duration(seconds: 2),
  }) async {
    final pending = items.where((i) => i.mergeable == 'UNKNOWN').toList();
    if (pending.isEmpty) return items;

    await Future<void>.delayed(delay);

    final resolved = <String, String>{};
    // Batch in groups to stay well inside GraphQL node limits.
    for (var start = 0; start < pending.length; start += 25) {
      final batch = pending.skip(start).take(25).toList();
      resolved.addAll(await _queryMergeable(batch));
    }
    if (resolved.isEmpty) return items;

    return [
      for (final item in items)
        if (resolved['${item.repo}#${item.number}'] case final String state)
          _withMergeable(item, state)
        else
          item,
    ];
  }

  Future<Map<String, String>> _queryMergeable(List<PrItem> batch) async {
    final fields = <String>[];
    for (var i = 0; i < batch.length; i++) {
      final pr = batch[i];
      final parts = pr.repo.split('/');
      if (parts.length != 2) continue;
      fields.add(
        'p$i: repository(owner: "${parts[0]}", name: "${parts[1]}") '
        '{ pullRequest(number: ${pr.number}) { mergeable } }',
      );
    }
    if (fields.isEmpty) return const {};

    try {
      final res = await _runner('gh', [
        'api',
        'graphql',
        '-f',
        'query={${fields.join(' ')}}',
      ]);
      if (res.exitCode != 0) return const {};
      final data = jsonDecode(res.stdout.toString()) as Map<String, dynamic>;
      final payload = data['data'];
      if (payload is! Map) return const {};

      final out = <String, String>{};
      for (var i = 0; i < batch.length; i++) {
        final state = payload['p$i']?['pullRequest']?['mergeable']?.toString();
        if (state != null && state != 'UNKNOWN') {
          out['${batch[i].repo}#${batch[i].number}'] = state.toUpperCase();
        }
      }
      return out;
    } catch (_) {
      return const {};
    }
  }

  static PrItem _withMergeable(PrItem item, String mergeable) => PrItem(
    number: item.number,
    title: item.title,
    url: item.url,
    repo: item.repo,
    author: item.author,
    isDraft: item.isDraft,
    mergeable: mergeable,
    reviewDecision: item.reviewDecision,
    ciStatus: item.ciStatus,
    totalCheckCount: item.totalCheckCount,
    failingChecks: item.failingChecks,
    unresolvedReviewThreads: item.unresolvedReviewThreads,
    totalReviewThreads: item.totalReviewThreads,
    unresolvedThreadsExact: item.unresolvedThreadsExact,
    labels: item.labels,
    updatedAt: item.updatedAt,
    createdAt: item.createdAt,
    headCommitDate: item.headCommitDate,
    lastReviewRequestedAt: item.lastReviewRequestedAt,
    latestReviews: item.latestReviews,
    allReviews: item.allReviews,
    requestedReviewers: item.requestedReviewers,
    requestedTeams: item.requestedTeams,
    assignedReviewers: item.assignedReviewers,
    recentComments: item.recentComments,
  );

  static PrSearchResult _merge(List<PrSearchResult> results) {
    final all = <PrItem>[];
    final seen = <String>{};
    var truncated = false;
    for (final result in results) {
      truncated = truncated || result.truncated;
      for (final pr in result.items) {
        if (seen.add('${pr.repo}#${pr.number}')) all.add(pr);
      }
    }
    return PrSearchResult(items: all, truncated: truncated);
  }

  Future<PrSearchResult> _searchPrsViaGraphQL(
    String searchQuery, {
    int limit = 50,
  }) async {
    final effectiveLimit = limit.clamp(1, kMaxSearchPageSize);
    final res = await _runner('gh', [
      'api',
      'graphql',
      '-f',
      'query=$_searchQueryDocument',
      '-F',
      'searchQuery=$searchQuery',
      '-F',
      'limit=$effectiveLimit',
    ]);

    if (res.exitCode != 0) {
      throw ProcessException(
        'gh',
        ['api', 'graphql'],
        'Failed to query GitHub GraphQL for "$searchQuery": ${res.stderr}',
        res.exitCode,
      );
    }

    final rawJson = res.stdout.toString();
    final Map<String, dynamic> data;
    try {
      data = jsonDecode(rawJson) as Map<String, dynamic>;
    } on FormatException catch (e) {
      throw FormatException(
        'GitHub returned non-JSON for "$searchQuery": ${e.message}',
      );
    }

    // `gh` exits non-zero on GraphQL errors today, but that is undocumented
    // behaviour; checking the payload makes the contract explicit.
    if (data['errors'] case final List errors when errors.isNotEmpty) {
      final messages = errors
          .map((e) => (e is Map ? e['message'] : e).toString())
          .join('; ');
      throw FormatException(
        'GitHub GraphQL errors for "$searchQuery": $messages',
      );
    }

    final search = data['data']?['search'];
    if (search is! Map) {
      throw FormatException(
        'Unexpected GraphQL response shape for "$searchQuery". '
        'Expected data.search, got: ${_preview(rawJson)}',
      );
    }
    final nodes = search['nodes'];
    if (nodes is! List) {
      throw FormatException(
        'Unexpected GraphQL response shape for "$searchQuery". '
        'Expected data.search.nodes to be a list, got: ${_preview(rawJson)}',
      );
    }

    final prs = <PrItem>[];
    for (final node in nodes) {
      if (node is Map<String, dynamic> && node['number'] != null) {
        prs.add(PrItem.fromJson(_enrichGraphQLPrNode(node)));
      }
    }

    final issueCount = (search['issueCount'] as int?) ?? prs.length;
    return PrSearchResult(items: prs, truncated: issueCount > nodes.length);
  }

  static String _preview(String raw) =>
      raw.length <= 200 ? raw : '${raw.substring(0, 200)}...';

  /// Flattens the nested review-thread and commit shapes into the flat keys
  /// [PrItem.fromJson] expects.
  static Map<String, dynamic> _enrichGraphQLPrNode(Map<String, dynamic> node) {
    final enriched = Map<String, dynamic>.from(node);

    final threads = _summarizeThreads(node['reviewThreads']);
    enriched['unresolvedReviewThreads'] = threads.unresolved;
    enriched['totalReviewThreads'] = threads.total;
    enriched['unresolvedThreadsExact'] = threads.exact;

    enriched['headCommitDate'] = _firstMap(
      node['commits'],
    )?['commit']?['committedDate'];
    enriched['lastReviewRequestedAt'] = _lastMap(
      node['timelineItems'],
    )?['createdAt'];

    // `reviews` is selected for the full history; PrItem reads it as
    // allReviews. Without this, reviews hidden from latestReviews were lost.
    enriched['allReviews'] = node['reviews'];

    return enriched;
  }

  /// Counts unresolved review threads and reports whether the count is exact.
  ///
  /// When GitHub has more threads than the page we read, `unresolved` is only
  /// a lower bound, so `== 0` must not be read as "everything is resolved".
  static ({int unresolved, int total, bool exact}) _summarizeThreads(
    dynamic threads,
  ) {
    if (threads is! Map) return (unresolved: 0, total: 0, exact: true);

    final total = (threads['totalCount'] ?? 0) as int;
    final nodes = threads['nodes'];
    if (nodes is! List) return (unresolved: 0, total: total, exact: true);

    var unresolved = 0;
    for (final thread in nodes) {
      if (thread is Map && thread['isResolved'] == false) unresolved++;
    }

    var exact = total <= nodes.length;
    if (threads['pageInfo'] case final Map pageInfo) {
      exact = exact && pageInfo['hasNextPage'] != true;
    }
    return (unresolved: unresolved, total: total, exact: exact);
  }

  static Map<dynamic, dynamic>? _firstMap(dynamic connection) {
    final nodes = connection is Map ? connection['nodes'] : null;
    if (nodes is! List || nodes.isEmpty) return null;
    return nodes.first is Map ? nodes.first as Map : null;
  }

  static Map<dynamic, dynamic>? _lastMap(dynamic connection) {
    final nodes = connection is Map ? connection['nodes'] : null;
    if (nodes is! List || nodes.isEmpty) return null;
    return nodes.last is Map ? nodes.last as Map : null;
  }

  /// Search document.
  ///
  /// Notable selections:
  /// - `reviews(last: 30)` because `latestReviews` omits a reviewer once a
  ///   re-review is requested from them, which is exactly the state we need
  ///   to detect.
  /// - `commits.committedDate` as the only trustworthy "new work exists"
  ///   signal; `updatedAt` moves on label edits and bot comments.
  /// - `timelineItems(REVIEW_REQUESTED_EVENT)` so "days waiting for review"
  ///   measures time since the request rather than time since any activity.
  /// - `contexts.totalCount` so "zero checks ran" is distinguishable from
  ///   "we failed to read the rollup".
  /// - Review bodies are deliberately not selected; nothing consumes them and
  ///   they dominated the output size.
  /// - `comments(last: 15)` bodies are selected so an explicit "@you, please
  ///   review" is visible. Only the parsed mentions and a short excerpt are
  ///   kept, so output size stays bounded.
  static const String _searchQueryDocument = r'''
query($searchQuery: String!, $limit: Int!) {
  search(query: $searchQuery, type: ISSUE, first: $limit) {
    issueCount
    nodes {
      ... on PullRequest {
        number
        title
        url
        isDraft
        mergeable
        reviewDecision
        updatedAt
        createdAt
        repository { nameWithOwner }
        author { login }
        labels(first: 30) { nodes { name } }
        latestReviews(first: 20) {
          nodes { author { login } state submittedAt }
        }
        reviews(last: 30) {
          nodes { author { login } state submittedAt }
        }
        comments(last: 15) {
          nodes { author { login } body createdAt }
        }
        reviewRequests(first: 20) {
          nodes {
            requestedReviewer {
              __typename
              ... on User { login }
              ... on Team { slug }
            }
          }
        }
        assignees(first: 10) { nodes { login } }
        timelineItems(last: 1, itemTypes: [REVIEW_REQUESTED_EVENT]) {
          nodes { ... on ReviewRequestedEvent { createdAt } }
        }
        reviewThreads(first: 100) {
          totalCount
          pageInfo { hasNextPage }
          nodes { isResolved }
        }
        commits(last: 1) {
          nodes {
            commit {
              committedDate
              statusCheckRollup {
                state
                contexts(first: 100) {
                  totalCount
                  nodes {
                    ... on CheckRun { name conclusion status }
                    ... on StatusContext { context state }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}
''';
}
