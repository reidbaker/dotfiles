import 'dart:convert';

import 'github_fetcher.dart';

/// Kind of reviewer feedback reported by [ThreadScanner].
enum FeedbackKind {
  /// An unresolved inline review thread.
  thread,

  /// The body of a submitted review.
  review,

  /// A PR-level (issue) comment.
  comment,
}

/// One piece of reviewer feedback that may still need an answer.
class FeedbackItem {
  const FeedbackItem({
    required this.kind,
    required this.id,
    required this.author,
    required this.createdAt,
    required this.body,
    required this.url,
    this.path,
    this.line,
    this.outdated = false,
    this.state,
    this.reason,
  });

  final FeedbackKind kind;

  /// REST database id of the comment or review. For threads, the id of the
  /// last comment.
  final int id;
  final String author;
  final DateTime createdAt;
  final String body;
  final String url;
  final String? path;
  final int? line;
  final bool outdated;

  /// Review state, for [FeedbackKind.review].
  final String? state;

  /// Why a thread the agent replied to on a merged PR is still listed.
  final String? reason;

  /// `path:line`, or null for feedback not attached to a file.
  String? get location =>
      path == null ? null : '$path:${line?.toString() ?? '?'}';

  Map<String, Object?> toJson() => {
    'kind': kind.name,
    'id': id,
    'author': author,
    'createdAt': createdAt.toUtc().toIso8601String(),
    'path': ?path,
    'line': ?line,
    if (kind == FeedbackKind.thread) 'outdated': outdated,
    'state': ?state,
    'reason': ?reason,
    'body': body,
    'url': url,
  };
}

/// Feedback found on one pull request.
class PrFeedback {
  const PrFeedback({
    required this.number,
    required this.title,
    required this.url,
    required this.items,
    this.mergedAt,
  });

  final int number;
  final String title;
  final String url;
  final DateTime? mergedAt;
  final List<FeedbackItem> items;

  bool get merged => mergedAt != null;

  Map<String, Object?> toJson() => {
    'number': number,
    'title': title,
    'url': url,
    'merged': merged,
    if (mergedAt != null) 'mergedAt': mergedAt!.toUtc().toIso8601String(),
    'items': [for (final i in items) i.toJson()],
  };
}

/// Outcome of a scan. [failures] is non-empty when any fetch failed; callers
/// must treat that as a failed run, because the report is incomplete.
class ThreadScanResult {
  const ThreadScanResult({required this.prs, required this.failures});

  final List<PrFeedback> prs;
  final List<String> failures;

  List<PrFeedback> get open => [
    for (final p in prs)
      if (!p.merged) p,
  ];
  List<PrFeedback> get merged => [
    for (final p in prs)
      if (p.merged) p,
  ];

  Map<String, Object?> toJson() => {
    'open': [for (final p in open) p.toJson()],
    'merged': [for (final p in merged) p.toJson()],
    'failures': failures,
  };
}

/// Options that decide which feedback counts as unaddressed.
class ThreadScanOptions {
  const ThreadScanOptions({
    required this.repo,
    this.agent,
    this.since,
    this.includeMerged = const Duration(days: 2),
    this.ignoreAuthors = const {},
  });

  /// `owner/name`.
  final String repo;

  /// Login whose last word on a thread marks it addressed. Its own reviews
  /// and comments are never reported.
  final String? agent;

  /// Drop feedback older than this.
  final DateTime? since;

  /// How far back to include merged PRs.
  final Duration includeMerged;

  /// Logins treated like the agent: their comments are hidden and, as the
  /// last comment of a thread, do not count as an answer by a reviewer.
  final Set<String> ignoreAuthors;
}

/// Lists reviewer feedback that has not been answered by the agent, across
/// every open PR in a repository and every PR merged within a window.
class ThreadScanner {
  ThreadScanner({CommandRunner? runner, DateTime Function()? clock})
    : _runner = runner ?? defaultCommandRunner,
      _clock = clock ?? DateTime.now;

  final CommandRunner _runner;
  final DateTime Function() _clock;

  static const int _maxBody = 300;

  /// Linked PR/issue status lookups, keyed by `owner/repo#N`, shared by
  /// every PR in a run so each item is fetched once.
  final Map<String, Future<_LinkStatus>> _linkCache = {};

  Future<ThreadScanResult> scan(ThreadScanOptions options) async {
    final failures = <String>[];
    final List<_PrRef> refs;
    try {
      refs = await _listPrs(options);
    } on Object catch (e) {
      return ThreadScanResult(prs: const [], failures: ['listing PRs: $e']);
    }
    final results = await Future.wait([
      for (final ref in refs) _scanPrSafely(ref, options, failures),
    ]);
    return ThreadScanResult(prs: [...results.nonNulls], failures: failures);
  }

  Future<PrFeedback?> _scanPrSafely(
    _PrRef ref,
    ThreadScanOptions options,
    List<String> failures,
  ) async {
    try {
      return await _scanPr(ref, options);
    } on Object catch (e) {
      failures.add('PR #${ref.number}: $e');
      return null;
    }
  }

  Future<List<_PrRef>> _listPrs(ThreadScanOptions options) async {
    final open = await _ghJson([
      'pr', 'list', '-R', options.repo, '--state', 'open', //
      '--limit', '1000', '--json', 'number,title,url,mergedAt',
    ]);
    final cutoff = _clock().toUtc().subtract(options.includeMerged);
    final refs = <int, _PrRef>{
      for (final raw in open as List)
        (raw as Map<String, dynamic>)['number'] as int: _PrRef.fromJson(raw),
    };
    for (final ref in await _mergedSince(options.repo, cutoff)) {
      refs[ref.number] = ref;
    }
    return refs.values.toList()..sort((a, b) => a.number - b.number);
  }

  /// Lists PRs merged at or after [cutoff].
  ///
  /// Walks `pullRequests` ordered by `updatedAt`, not the search API: search
  /// is eventually consistent and was seen returning no results at all.
  /// Merging updates a PR, so once a page reaches `updatedAt < cutoff` no
  /// later page can hold a PR merged after it.
  Future<List<_PrRef>> _mergedSince(String repo, DateTime cutoff) async {
    final [owner, name] = repo.split('/');
    final refs = <_PrRef>[];
    String? cursor;
    var more = true;
    while (more) {
      final data = await _ghJson([
        'api', 'graphql', '-f', 'query=$_mergedQuery', //
        '-F', 'owner=$owner', '-F', 'name=$name',
        if (cursor != null) ...['-F', 'cursor=$cursor'],
      ]);
      final conn = (data as Map)['data']?['repository']?['pullRequests'];
      if (conn is! Map) {
        throw const FormatException('missing pullRequests in response');
      }
      final nodes = (conn['nodes'] as List).cast<Map<String, dynamic>>();
      refs.addAll([
        for (final n in nodes)
          if (!DateTime.parse(n['mergedAt'] as String).isBefore(cutoff))
            _PrRef.fromJson(n),
      ]);
      final info = conn['pageInfo'] as Map;
      more =
          info['hasNextPage'] == true &&
          nodes.isNotEmpty &&
          !DateTime.parse(nodes.last['updatedAt'] as String).isBefore(cutoff);
      cursor = info['endCursor'] as String?;
    }
    return refs;
  }

  static const String _mergedQuery = r'''
query($owner: String!, $name: String!, $cursor: String) {
  repository(owner: $owner, name: $name) {
    pullRequests(states: MERGED, first: 100, after: $cursor,
        orderBy: {field: UPDATED_AT, direction: DESC}) {
      pageInfo { hasNextPage endCursor }
      nodes { number title url mergedAt updatedAt }
    }
  }
}''';

  Future<PrFeedback> _scanPr(_PrRef ref, ThreadScanOptions options) async {
    final [threads, reviews, comments] = await Future.wait([
      _threads(ref, options),
      _reviews(ref, options),
      _comments(ref, options),
    ]);
    final items = [...threads, ...reviews, ...comments]
        .where(
          (i) => options.since == null || !i.createdAt.isBefore(options.since!),
        )
        .toList();
    return PrFeedback(
      number: ref.number,
      title: ref.title,
      url: ref.url,
      mergedAt: ref.mergedAt,
      items: items,
    );
  }

  bool _hidden(String login, ThreadScanOptions o) =>
      login == o.agent || o.ignoreAuthors.contains(login);

  Future<List<FeedbackItem>> _threads(
    _PrRef ref,
    ThreadScanOptions options,
  ) async {
    final items = <FeedbackItem>[];
    String? cursor;
    do {
      final page = await _threadPage(options.repo, ref.number, cursor);
      final resolved = await Future.wait([
        for (final node in page.nodes)
          _threadItem(node as Map<String, dynamic>, ref, options),
      ]);
      items.addAll(resolved.nonNulls);
      cursor = page.nextCursor;
    } while (cursor != null);
    return items;
  }

  Future<FeedbackItem?> _threadItem(
    Map<String, dynamic> t,
    _PrRef ref,
    ThreadScanOptions o,
  ) async {
    if (t['isResolved'] == true) return null;
    final comments = (t['comments']?['nodes'] as List?) ?? const [];
    if (comments.isEmpty) return null;
    final last = comments.last as Map<String, dynamic>;
    final author = _login(last['author']);
    final body = last['body'] as String? ?? '';
    String? reason;
    if (o.ignoreAuthors.contains(author)) return null;
    final createdAt = DateTime.parse(last['createdAt'] as String);
    if (author == o.agent) {
      // Only a reply after the merge can be a promise with no commit behind
      // it; earlier replies were answered on the PR itself.
      final merged = ref.mergedAt;
      if (merged == null || !createdAt.isAfter(merged)) return null;
      reason = await _followUpGap(body, o.repo);
      if (reason == null) return null;
    }
    return FeedbackItem(
      kind: FeedbackKind.thread,
      id: last['databaseId'] as int,
      author: author,
      createdAt: createdAt,
      body: _short(body),
      url: last['url'] as String? ?? '',
      path: t['path'] as String?,
      line: (t['line'] ?? t['originalLine']) as int?,
      outdated: t['isOutdated'] == true,
      reason: reason,
    );
  }

  static final RegExp _linkPattern = RegExp(
    r'https://github\.com/([\w.-]+/[\w.-]+)/(?:pull|issues)/(\d+)'
    r'|(?<![\w/&])#(\d+)\b',
  );

  /// Returns why the agent's reply on a merged PR does not close the thread,
  /// or null when every linked follow-up is merged (PR) or closed (issue).
  Future<String?> _followUpGap(String body, String repo) async {
    final keys = <String>{
      for (final m in _linkPattern.allMatches(body))
        m.group(3) != null
            ? '$repo#${m.group(3)}'
            : '${m.group(1)}#${m.group(2)}',
    };
    if (keys.isEmpty) return 'no follow-up linked';
    final statuses = await Future.wait([
      for (final k in keys)
        _linkCache.putIfAbsent(
          k,
          () =>
              _lookup(k, k.startsWith('$repo#') ? k.substring(repo.length) : k),
        ),
    ]);
    final pending = [
      for (final s in statuses)
        if (!s.done) '${s.label} ${s.state}',
    ];
    return pending.isEmpty ? null : 'follow-up ${pending.join(', ')}';
  }

  Future<_LinkStatus> _lookup(String key, String label) async {
    final [repo, number] = key.split('#');
    final data = await _ghJson(['api', 'repos/$repo/issues/$number']);
    if (data is! Map) throw FormatException('bad response for $key');
    final closed = data['state'] == 'closed';
    if (data['pull_request'] case final Map pr) {
      final merged = pr['merged_at'] != null;
      final state = merged
          ? 'merged'
          : (closed ? 'closed without merging' : 'still open');
      return _LinkStatus(label, merged, state);
    }
    return _LinkStatus(label, closed, closed ? 'closed' : 'still open');
  }

  Future<({List<dynamic> nodes, String? nextCursor})> _threadPage(
    String repo,
    int number,
    String? cursor,
  ) async {
    final [owner, name] = repo.split('/');
    final data = await _ghJson([
      'api', 'graphql', '-f', 'query=$_threadsQuery', //
      '-F', 'owner=$owner', '-F', 'name=$name', '-F', 'number=$number',
      if (cursor != null) ...['-F', 'cursor=$cursor'],
    ]);
    if (data case {'errors': final List errors} when errors.isNotEmpty) {
      throw FormatException('GraphQL errors: $errors');
    }
    final conn =
        (data as Map)['data']?['repository']?['pullRequest']?['reviewThreads'];
    if (conn is! Map) {
      throw const FormatException('missing reviewThreads in response');
    }
    final info = conn['pageInfo'] as Map?;
    return (
      nodes: conn['nodes'] as List,
      nextCursor: info?['hasNextPage'] == true
          ? info!['endCursor'] as String?
          : null,
    );
  }

  Future<List<FeedbackItem>> _reviews(_PrRef ref, ThreadScanOptions o) async {
    final rows = await _restPages(
      'repos/${o.repo}/pulls/${ref.number}/reviews',
    );
    return [
      for (final r in rows)
        if (_isHuman(r['user'], o) && (r['body'] as String? ?? '').isNotEmpty)
          FeedbackItem(
            kind: FeedbackKind.review,
            id: r['id'] as int,
            author: _login(r['user']),
            createdAt: DateTime.parse(r['submitted_at'] as String),
            body: _short(r['body'] as String),
            url: r['html_url'] as String? ?? '',
            state: r['state'] as String?,
          ),
    ];
  }

  Future<List<FeedbackItem>> _comments(_PrRef ref, ThreadScanOptions o) async {
    final rows = await _restPages(
      'repos/${o.repo}/issues/${ref.number}/comments',
    );
    return [
      for (final c in rows)
        if (_isHuman(c['user'], o))
          FeedbackItem(
            kind: FeedbackKind.comment,
            id: c['id'] as int,
            author: _login(c['user']),
            createdAt: DateTime.parse(c['created_at'] as String),
            body: _short(c['body'] as String? ?? ''),
            url: c['html_url'] as String? ?? '',
          ),
    ];
  }

  bool _isHuman(Object? user, ThreadScanOptions o) {
    if (user is! Map) return false;
    final login = _login(user);
    return user['type'] != 'Bot' &&
        !login.endsWith('[bot]') &&
        !_hidden(login, o);
  }

  /// Reads every page of a REST list endpoint. `--slurp` wraps the pages in
  /// an outer array, so the result is flattened here.
  Future<List<Map<String, dynamic>>> _restPages(String endpoint) async {
    final pages = await _ghJson(['api', endpoint, '--paginate', '--slurp']);
    return [
      for (final page in pages as List)
        for (final row in page as List) row as Map<String, dynamic>,
    ];
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

  static String _login(Object? user) =>
      (user is Map ? user['login'] as String? : null) ?? 'ghost';

  static String _short(String body) {
    final flat = body.replaceAll(RegExp(r'\s+'), ' ').trim();
    return flat.length <= _maxBody ? flat : '${flat.substring(0, _maxBody)}…';
  }

  static const String _threadsQuery = r'''
query($owner: String!, $name: String!, $number: Int!, $cursor: String) {
  repository(owner: $owner, name: $name) {
    pullRequest(number: $number) {
      reviewThreads(first: 100, after: $cursor) {
        pageInfo { hasNextPage endCursor }
        nodes {
          isResolved isOutdated path line originalLine
          comments(last: 100) {
            nodes { databaseId author { login } createdAt body url }
          }
        }
      }
    }
  }
}''';
}

class _LinkStatus {
  const _LinkStatus(this.label, this.done, this.state);

  final String label;
  final bool done;
  final String state;
}

class _PrRef {
  const _PrRef(this.number, this.title, this.url, this.mergedAt);

  factory _PrRef.fromJson(Map<String, dynamic> j) {
    final merged = j['mergedAt'] as String?;
    return _PrRef(
      j['number'] as int,
      j['title'] as String? ?? '',
      j['url'] as String? ?? '',
      merged == null || merged.isEmpty || merged.startsWith('0001')
          ? null
          : DateTime.parse(merged),
    );
  }

  final int number;
  final String title;
  final String url;
  final DateTime? mergedAt;
}

/// Parses `2d`, `36h`, `90m` or `45s`.
Duration? parseDuration(String raw) {
  final m = RegExp(r'^(\d+)([dhms])$').firstMatch(raw.trim());
  if (m == null) return null;
  final n = int.parse(m.group(1)!);
  return switch (m.group(2)) {
    'd' => Duration(days: n),
    'h' => Duration(hours: n),
    'm' => Duration(minutes: n),
    _ => Duration(seconds: n),
  };
}

/// Renders [result] as plain text for a terminal or a chat paste.
String formatThreadReport(ThreadScanResult result) {
  final out = StringBuffer();
  _writeSection(out, 'Open PRs', result.open);
  _writeSection(
    out,
    'Merged PRs (merged — needs a follow-up PR)',
    result.merged,
  );
  for (final f in result.failures) {
    out.writeln('FETCH FAILURE: $f');
  }
  return out.toString();
}

void _writeSection(StringBuffer out, String title, List<PrFeedback> prs) {
  out.writeln('== $title');
  final withItems = [
    for (final p in prs)
      if (p.items.isNotEmpty) p,
  ];
  if (withItems.isEmpty) out.writeln('  (nothing unaddressed)');
  for (final pr in withItems) {
    final tag = pr.merged ? ' [merged — needs a follow-up PR]' : '';
    out.writeln('-- PR #${pr.number}$tag: ${pr.title}');
    for (final i in pr.items) {
      _writeItem(out, i);
    }
  }
  out.writeln();
}

void _writeItem(StringBuffer out, FeedbackItem i) {
  final where = switch (i.kind) {
    FeedbackKind.thread =>
      '${i.location}${i.outdated ? ' (outdated)' : ''}'
          '${i.reason == null ? '' : ' [${i.reason}]'}',
    FeedbackKind.review => 'review ${i.state ?? ''}'.trim(),
    FeedbackKind.comment => 'PR comment',
  };
  out
    ..writeln(
      '   [${i.id}] $where — ${i.author} '
      '${i.createdAt.toUtc().toIso8601String()}',
    )
    ..writeln('     ${i.body}')
    ..writeln('     ${i.url}');
}
