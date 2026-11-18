import 'dart:convert';

import 'github_fetcher.dart';

/// The `sha` of one `adversarial-review` pass in the review log.
class ReviewedSha {
  const ReviewedSha(this.repo, this.sha);

  /// `owner/name`, lowercase.
  final String repo;

  /// Lowercase hex commit SHA, 7 to 40 characters.
  final String sha;
}

final RegExp _shaPattern = RegExp(r'^[0-9a-f]{7,40}$');

/// Reads `adversarial-review` entries from the text of `review-log.jsonl`.
///
/// Each line is one JSON object. Lines that are blank, not JSON, not a
/// `review` event, from another skill, or without a valid SHA are skipped.
/// The SHA comes from `reviewed_sha`, or from `sha` in older entries.
List<ReviewedSha> parseReviewLog(String text) {
  final out = <ReviewedSha>[];
  for (final line in const LineSplitter().convert(text)) {
    final entry = _decodeLine(line);
    if (entry == null) continue;
    if (entry['event'] != 'review') continue;
    if (entry['skill'] != 'adversarial-review') continue;
    final repo = entry['repo'];
    final sha = entry['reviewed_sha'] ?? entry['sha'];
    if (repo is! String || sha is! String) continue;
    final lowerSha = sha.trim().toLowerCase();
    if (!_shaPattern.hasMatch(lowerSha)) continue;
    out.add(ReviewedSha(repo.trim().toLowerCase(), lowerSha));
  }
  return out;
}

Map<String, dynamic>? _decodeLine(String line) {
  if (line.trim().isEmpty) return null;
  try {
    final value = jsonDecode(line);
    return value is Map<String, dynamic> ? value : null;
  } on FormatException {
    return null;
  }
}

/// One commit on a pull request.
class PrCommit {
  const PrCommit({
    required this.sha,
    required this.parents,
    required this.committedAt,
  });

  final String sha;
  final List<String> parents;
  final DateTime committedAt;

  /// A merge commit brings in another branch, usually the base branch. It is
  /// not counted as code that needs review.
  bool get isMerge => parents.length > 1;
}

/// One comment in a review thread.
class ThreadComment {
  const ThreadComment({
    required this.author,
    required this.isBot,
    required this.createdAt,
    required this.url,
  });

  final String author;
  final bool isBot;
  final DateTime createdAt;
  final String url;
}

/// One inline review thread.
class ReviewThread {
  const ReviewThread({required this.resolved, required this.comments});

  final bool resolved;
  final List<ThreadComment> comments;
}

/// What `--ready` needs to know about one pull request.
class PrSnapshot {
  const PrSnapshot({
    required this.author,
    required this.headSha,
    required this.commits,
    required this.threads,
  });

  final String author;
  final String headSha;

  /// Every commit on the PR, as listed by GitHub.
  final List<PrCommit> commits;
  final List<ReviewThread> threads;
}

/// The GitHub REST API lists at most this many commits for a pull request.
const int maxListedCommits = 250;

/// Whether a GitHub account is a bot. GraphQL reports bots with the
/// `Bot` type and no `[bot]` suffix; REST adds the suffix.
bool isBotAccount(String login, String? typeName) =>
    typeName == 'Bot' || login.endsWith('[bot]');

/// Returns the non-merge commits on [pr] that no logged review covers.
///
/// A review of commit R covers R and every ancestor of R on the PR. Ancestry
/// comes from the parent links GitHub returns for the PR's commits, so no
/// local clone is needed. Returns null when no logged SHA matches any commit
/// on the PR. A logged SHA may be a prefix of the full SHA.
List<PrCommit>? unreviewedCommits(PrSnapshot pr, Iterable<String> loggedShas) {
  final bySha = {for (final c in pr.commits) c.sha.toLowerCase(): c};
  final reviewed = [
    for (final sha in bySha.keys)
      if (loggedShas.any(sha.startsWith)) sha,
  ];
  if (reviewed.isEmpty) return null;
  final covered = <String>{};
  final pending = [...reviewed];
  while (pending.isNotEmpty) {
    final sha = pending.removeLast();
    final commit = bySha[sha];
    if (commit == null || !covered.add(sha)) continue;
    pending.addAll(commit.parents.map((p) => p.toLowerCase()));
  }
  return [
    for (final c in pr.commits)
      if (!c.isMerge && !covered.contains(c.sha.toLowerCase())) c,
  ];
}

/// Returns the last human comment of each unresolved thread that still waits
/// for an answer.
///
/// A thread waits when its last non-bot comment is by someone other than the
/// PR author or [answerers], and no non-merge commit on the PR was made after
/// that comment. Threads with only bot comments are skipped.
List<ThreadComment> unansweredThreads(PrSnapshot pr, Set<String> answerers) {
  final answering = {
    pr.author.toLowerCase(),
    for (final a in answerers) a.toLowerCase(),
  };
  final lastCode = _lastCodeCommitTime(pr.commits);
  final out = <ThreadComment>[];
  for (final thread in pr.threads) {
    if (thread.resolved) continue;
    final humans = thread.comments.where((c) => !c.isBot);
    if (humans.isEmpty) continue;
    final last = humans.last;
    if (answering.contains(last.author.toLowerCase())) continue;
    if (lastCode != null && lastCode.isAfter(last.createdAt)) continue;
    out.add(last);
  }
  return out;
}

DateTime? _lastCodeCommitTime(List<PrCommit> commits) {
  DateTime? last;
  for (final c in commits) {
    if (c.isMerge) continue;
    if (last == null || c.committedAt.isAfter(last)) last = c.committedAt;
  }
  return last;
}

/// The result of `--ready` for one pull request.
class ReadyVerdict {
  const ReadyVerdict(this.spec, this.reasons);

  final PrSpec spec;

  /// Why the PR is not ready. Empty when it is ready.
  final List<String> reasons;

  bool get ready => reasons.isEmpty;

  /// `READY <pr>` or `NOT READY <pr>: <reasons>`.
  String get line =>
      ready ? 'READY $spec' : 'NOT READY $spec: ${reasons.join('; ')}';
}

/// Decides whether [pr] is ready, given every review in the log.
ReadyVerdict judgeReadiness(
  PrSpec spec,
  PrSnapshot pr,
  List<ReviewedSha> log, {
  Set<String> answerers = const {},
}) {
  final reasons = <String>[];
  final reviewReason = _reviewReason(spec, pr, log);
  if (reviewReason != null) reasons.add(reviewReason);
  final waiting = unansweredThreads(pr, answerers);
  if (waiting.isNotEmpty) {
    final noun = waiting.length == 1 ? 'thread' : 'threads';
    reasons.add(
      '${waiting.length} human review $noun without a reply or later '
      'commit: ${_listSome([for (final c in waiting) c.url])}',
    );
  }
  return ReadyVerdict(spec, reasons);
}

String? _reviewReason(PrSpec spec, PrSnapshot pr, List<ReviewedSha> log) {
  if (pr.commits.length >= maxListedCommits) {
    return 'PR has $maxListedCommits or more commits, more than GitHub '
        'lists; check the review log by hand';
  }
  final repo = spec.repo.toLowerCase();
  final shas = [
    for (final entry in log)
      if (entry.repo == repo) entry.sha,
  ];
  final missing = unreviewedCommits(pr, shas);
  if (missing == null) {
    return 'no adversarial-review entry in the review log for any commit '
        'on this PR (head ${_short(pr.headSha)})';
  }
  if (missing.isEmpty) return null;
  return 'commits no adversarial-review entry covers: '
      '${_listSome([for (final c in missing) _short(c.sha)])}';
}

String _short(String sha) => sha.length > 10 ? sha.substring(0, 10) : sha;

String _listSome(List<String> items, {int max = 5}) {
  if (items.length <= max) return items.join(', ');
  return '${items.take(max).join(', ')} and ${items.length - max} more';
}

/// Fetches pull request data for `--ready` with the `gh` CLI.
class ReadyChecker {
  ReadyChecker({CommandRunner? runner})
    : _runner = runner ?? defaultCommandRunner;

  final CommandRunner _runner;

  /// Fetches [spec] and judges it. A fetch failure is a not-ready verdict.
  Future<ReadyVerdict> check(
    PrSpec spec,
    List<ReviewedSha> log, {
    Set<String> answerers = const {},
  }) async {
    final PrSnapshot pr;
    try {
      pr = await fetch(spec);
    } on Object catch (e) {
      return ReadyVerdict(spec, ['fetch failed: $e']);
    }
    return judgeReadiness(spec, pr, log, answerers: answerers);
  }

  /// Reads the commits (REST) and review threads (GraphQL) of [spec].
  Future<PrSnapshot> fetch(PrSpec spec) async {
    final commitPages = await _ghJson([
      'api',
      'repos/${spec.repo}/pulls/${spec.number}/commits',
      '--paginate',
      '--slurp',
    ]);
    final commits = parseCommitPages(commitPages);
    final threads = <ReviewThread>[];
    String? author;
    String? head;
    String? cursor;
    do {
      final page = parseThreadPage(await _threadPage(spec, cursor));
      author ??= page.author;
      head ??= page.headSha;
      threads.addAll(page.threads);
      cursor = page.nextCursor;
    } while (cursor != null);
    return PrSnapshot(
      author: author ?? '',
      headSha: head ?? '',
      commits: commits,
      threads: threads,
    );
  }

  Future<Object?> _threadPage(PrSpec spec, String? cursor) {
    final [owner, name] = spec.repo.split('/');
    return _ghJson([
      'api', 'graphql', '-f', 'query=$_threadsQuery', //
      '-F', 'owner=$owner', '-F', 'name=$name', '-F', 'number=${spec.number}',
      if (cursor != null) ...['-F', 'cursor=$cursor'],
    ]);
  }

  Future<Object?> _ghJson(List<String> args) async {
    final res = await _runner('gh', args);
    if (res.exitCode != 0) {
      throw FormatException(
        '`gh ${args.take(3).join(' ')}` exited ${res.exitCode}: '
        '${res.stderr.toString().trim()}',
      );
    }
    return jsonDecode(res.stdout.toString());
  }

  static const String _threadsQuery = r'''
query($owner: String!, $name: String!, $number: Int!, $cursor: String) {
  repository(owner: $owner, name: $name) {
    pullRequest(number: $number) {
      author { login }
      headRefOid
      reviewThreads(first: 100, after: $cursor) {
        pageInfo { hasNextPage endCursor }
        nodes {
          isResolved
          comments(last: 100) {
            nodes { author { login __typename } createdAt url }
          }
        }
      }
    }
  }
}''';
}

/// Parses `gh api repos/<o>/<r>/pulls/<n>/commits --paginate --slurp`
/// output, which wraps each page in an outer array.
List<PrCommit> parseCommitPages(Object? pages) => [
  for (final page in pages as List)
    for (final row in page as List)
      PrCommit(
        sha: row['sha'] as String,
        parents: [for (final p in row['parents'] as List) p['sha'] as String],
        committedAt: DateTime.parse(
          row['commit']['committer']['date'] as String,
        ),
      ),
];

/// One page of the review-thread GraphQL query.
typedef ThreadPage = ({
  String? author,
  String? headSha,
  List<ReviewThread> threads,
  String? nextCursor,
});

/// Parses one page of the review-thread GraphQL query.
ThreadPage parseThreadPage(Object? data) {
  if (data case {'errors': final List errors} when errors.isNotEmpty) {
    throw FormatException('GraphQL errors: $errors');
  }
  final pr = (data as Map)['data']?['repository']?['pullRequest'];
  if (pr is! Map) throw const FormatException('missing pullRequest');
  final conn = pr['reviewThreads'] as Map;
  final info = conn['pageInfo'] as Map?;
  return (
    author: pr['author']?['login'] as String?,
    headSha: pr['headRefOid'] as String?,
    threads: [for (final t in conn['nodes'] as List) _parseThread(t as Map)],
    nextCursor: info?['hasNextPage'] == true
        ? info!['endCursor'] as String?
        : null,
  );
}

ReviewThread _parseThread(Map t) => ReviewThread(
  resolved: t['isResolved'] == true,
  comments: [
    for (final c in (t['comments']?['nodes'] as List?) ?? const [])
      _parseComment(c as Map),
  ],
);

ThreadComment _parseComment(Map c) {
  final user = c['author'];
  final login = (user is Map ? user['login'] as String? : null) ?? 'ghost';
  final typeName = user is Map ? user['__typename'] as String? : null;
  return ThreadComment(
    author: login,
    isBot: isBotAccount(login, typeName),
    createdAt: DateTime.parse(c['createdAt'] as String),
    url: c['url'] as String? ?? '',
  );
}
