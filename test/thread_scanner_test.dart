import 'dart:convert';
import 'dart:io';

import 'package:dotfiles/src/pr_triage/thread_scanner.dart';
import 'package:test/test.dart';

final DateTime kNow = DateTime.utc(2026, 10, 22, 15);

const String kRepo = 'google/skills_lint.dart';

Map<String, Object?> comment(int id, String login, String at) => {
  'databaseId': id,
  'author': {'login': login},
  'createdAt': at,
  'body': 'comment $id',
  'url': 'https://example.test/$id',
};

Map<String, Object?> thread(
  List<Map<String, Object?>> comments, {
  bool resolved = false,
  bool outdated = false,
}) => {
  'isResolved': resolved,
  'isOutdated': outdated,
  'path': 'a.md',
  'line': 3,
  'originalLine': 3,
  'comments': {'nodes': comments},
};

String threadPage(List<Object?> nodes, {String? next}) => jsonEncode({
  'data': {
    'repository': {
      'pullRequest': {
        'reviewThreads': {
          'pageInfo': {'hasNextPage': next != null, 'endCursor': next},
          'nodes': nodes,
        },
      },
    },
  },
});

/// Answers `gh` invocations from canned responses keyed by PR number.
class FakeGh {
  FakeGh({
    this.open = const [],
    this.merged = const [],
    this.threadPages = const {},
    this.reviews = const {},
    this.comments = const {},
    this.failPr,
    this.links = const {},
    this.mergedPages = const {},
  });

  /// Cursor (`''` first) -> raw merged-PR listing; overrides [merged].
  final Map<String, String> mergedPages;

  /// `owner/repo#N` -> issue JSON; a missing key makes the lookup fail.
  final Map<String, Map<String, Object?>> links;

  final List<Map<String, Object?>> open;
  final List<Map<String, Object?>> merged;

  /// PR number -> cursor (`''` for the first page) -> response body.
  final Map<int, Map<String, String>> threadPages;
  final Map<int, List<Object?>> reviews;
  final Map<int, List<Object?>> comments;
  final int? failPr;
  final List<List<String>> calls = [];

  Future<ProcessResult> call(String exe, List<String> args) async {
    calls.add(args);
    if (args.first == 'pr') {
      return _ok(jsonEncode(open));
    }
    if (args.any((a) => a.contains('pullRequests('))) {
      if (mergedPages.isEmpty) return _merged();
      final cursor = args.firstWhere(
        (a) => a.startsWith('cursor='),
        orElse: () => 'cursor=',
      );
      return _ok(mergedPages[cursor.substring(7)]!);
    }
    if (args[1] == 'graphql') return _graphql(args);
    final endpoint = args[1];
    if (!args.contains('--paginate')) return _link(endpoint);
    final pr = int.parse(endpoint.split('/')[4]);
    final rows = endpoint.endsWith('/reviews') ? reviews[pr] : comments[pr];
    return _ok(jsonEncode([rows ?? const []]));
  }

  ProcessResult _graphql(List<String> args) {
    String field(String name) => args
        .firstWhere((a) => a.startsWith('$name='), orElse: () => '$name=')
        .substring(name.length + 1);
    final pr = int.parse(field('number'));
    if (pr == failPr) return ProcessResult(0, 1, '', 'HTTP 502');
    final page = threadPages[pr]?[field('cursor')] ?? threadPage(const []);
    return _ok(page);
  }

  ProcessResult _merged() => _ok(
    jsonEncode({
      'data': {
        'repository': {
          'pullRequests': {
            'pageInfo': {'hasNextPage': false},
            'nodes': [
              for (final m in merged) {...m, 'updatedAt': m['mergedAt']},
            ],
          },
        },
      },
    }),
  );

  ProcessResult _link(String endpoint) {
    final parts = endpoint.split('/');
    final link = links['${parts[1]}/${parts[2]}#${parts[4]}'];
    if (link == null) return ProcessResult(0, 1, '', 'HTTP 404');
    return _ok(jsonEncode(link));
  }

  ProcessResult _ok(String out) => ProcessResult(0, 0, out, '');
}

Map<String, Object?> prRef(int n, {String? mergedAt}) => {
  'number': n,
  'title': 'PR $n',
  'url': 'https://github.com/$kRepo/pull/$n',
  'mergedAt': mergedAt,
};

Future<ThreadScanResult> scan(
  FakeGh gh, {
  Set<String> ignore = const {},
  DateTime? since,
}) => ThreadScanner(runner: gh.call, clock: () => kNow).scan(
  ThreadScanOptions(
    repo: kRepo,
    agent: 'reidbaker-agent',
    ignoreAuthors: ignore,
    since: since,
  ),
);

Map<String, Object?> prLink(String state, {bool merged = false}) => {
  'state': state,
  'pull_request': {'merged_at': merged ? '2026-10-22T14:53:00Z' : null},
};

const Map<String, Object?> kClosedIssue = {'state': 'closed'};

/// A merged PR #120 whose only thread ends with an agent reply of [reply].
FakeGh mergedWithReply(
  String reply, {
  Map<String, Map<String, Object?>> links = const {},
  bool open = false,
  String mergedAt = '2026-10-22T14:36:58Z',
}) {
  final ref = prRef(120, mergedAt: open ? null : mergedAt);
  return FakeGh(
    open: open ? [ref] : const [],
    merged: open ? const [] : [ref],
    links: links,
    threadPages: {
      120: {
        '': threadPage([
          thread([
            comment(4349658228, 'reidbaker', '2026-10-22T14:34:51Z'),
            {
              ...comment(4349844032, 'reidbaker-agent', '2026-10-22T14:51:54Z'),
              'body': reply,
            },
          ]),
        ]),
      },
    },
  );
}

void mergedFollowUpTests() {
  group('merged PR thread answered by the agent', () {
    Future<List<String?>> reasons(FakeGh gh) async {
      final result = await scan(gh);
      expect(result.failures, isEmpty);
      return [
        for (final p in result.prs)
          for (final i in p.items) i.reason,
      ];
    }

    test('is hidden when the linked PR merged', () async {
      final gh = mergedWithReply(
        'Fixed in #130.',
        links: {'$kRepo#130': prLink('closed', merged: true)},
      );
      expect(await reasons(gh), isEmpty);
    });

    test('is shown when the linked PR is still open', () async {
      final gh = mergedWithReply(
        'Fixed in #130.',
        links: {'$kRepo#130': prLink('open')},
      );
      expect(await reasons(gh), ['follow-up #130 still open']);
      expect(
        formatThreadReport(await scan(gh)),
        contains('[follow-up #130 still open]'),
      );
    });

    test('is shown when the linked PR closed without merging', () async {
      final gh = mergedWithReply(
        'https://github.com/$kRepo/pull/130',
        links: {'$kRepo#130': prLink('closed')},
      );
      expect(await reasons(gh), ['follow-up #130 closed without merging']);
    });

    test('is shown when nothing is linked', () async {
      expect(await reasons(mergedWithReply('Will fix later.')), [
        'no follow-up linked',
      ]);
    });

    test('is shown when one of two links is still open', () async {
      final gh = mergedWithReply(
        'See #130 and https://github.com/other/repo/issues/7.',
        links: {
          '$kRepo#130': prLink('closed', merged: true),
          'other/repo#7': {'state': 'open'},
        },
      );
      expect(await reasons(gh), ['follow-up other/repo#7 still open']);
    });

    test('is hidden when the linked issue is closed', () async {
      final gh = mergedWithReply(
        'Tracked in https://github.com/$kRepo/issues/131.',
        links: {'$kRepo#131': kClosedIssue},
      );
      expect(await reasons(gh), isEmpty);
    });

    test('reports a lookup failure', () async {
      final result = await scan(mergedWithReply('Fixed in #130.'));
      expect(result.failures.single, contains('404'));
    });

    test('looks each link up once per run', () async {
      final gh = mergedWithReply(
        'Fixed in #130, see #130.',
        links: {'$kRepo#130': prLink('closed', merged: true)},
      );
      await scan(gh);
      expect(
        gh.calls.where((c) => c.contains('repos/$kRepo/issues/130')),
        hasLength(1),
      );
    });

    test('treats an agent reply before the merge as answered', () async {
      final gh = mergedWithReply('Done.', mergedAt: '2026-10-22T15:00:00Z');
      expect(await reasons(gh), isEmpty);
      expect(
        gh.calls.where((c) => c.contains('repos/$kRepo/issues/130')),
        isEmpty,
      );
    });

    test('does not change the rule for open PRs', () async {
      final gh = mergedWithReply('Will fix later.', open: true);
      expect(await reasons(gh), isEmpty);
      expect(
        gh.calls.where((c) => c.contains('repos/$kRepo/issues/130')),
        isEmpty,
      );
    });
  });
}

String mergedPage(List<(int, String, String)> prs, {String? next}) =>
    jsonEncode({
      'data': {
        'repository': {
          'pullRequests': {
            'pageInfo': {'hasNextPage': next != null, 'endCursor': next},
            'nodes': [
              for (final (n, merged, updated) in prs)
                {...prRef(n, mergedAt: merged), 'updatedAt': updated},
            ],
          },
        },
      },
    });

void main() {
  mergedFollowUpTests();

  test('pages merged PRs until updatedAt passes the cutoff', () async {
    // kNow is 2026-10-22T15:00Z, so the default 2d cutoff is 10-20T15:00Z.
    final gh = FakeGh(
      mergedPages: {
        '': mergedPage([
          (1, '2026-10-22T10:00:00Z', '2026-10-22T11:00:00Z'),
          // Merged long ago but recently updated: skipped, paging continues.
          (2, '2026-10-01T10:00:00Z', '2026-10-21T10:00:00Z'),
        ], next: 'm1'),
        'm1': mergedPage([
          (3, '2026-10-20T16:00:00Z', '2026-10-20T16:00:00Z'),
          (4, '2026-10-19T10:00:00Z', '2026-10-19T10:00:00Z'),
        ], next: 'm2'),
        // Never requested: page m1 already ends before the cutoff.
      },
    );
    final result = await scan(gh);
    expect(result.failures, isEmpty);
    expect([for (final p in result.merged) p.number], [1, 3]);
    expect(gh.calls.where((c) => c.contains('cursor=m1')), hasLength(1));
    expect(gh.calls.where((c) => c.contains('cursor=m2')), isEmpty);
  });

  test('excludes a thread whose last comment is from the agent', () async {
    final gh = FakeGh(
      open: [prRef(1)],
      threadPages: {
        1: {
          '': threadPage([
            thread([
              comment(10, 'lrhn', '2026-10-22T10:00:00Z'),
              comment(11, 'reidbaker-agent', '2026-10-22T11:00:00Z'),
            ]),
            thread([comment(20, 'lrhn', '2026-10-22T12:00:00Z')]),
            thread([
              comment(30, 'lrhn', '2026-10-22T12:00:00Z'),
            ], resolved: true),
          ]),
        },
      },
    );
    final result = await scan(gh);
    expect(result.failures, isEmpty);
    expect([for (final i in result.open.single.items) i.id], [20]);
  });

  test('--ignore-author hides threads answered by that login', () async {
    final gh = FakeGh(
      open: [prRef(1)],
      threadPages: {
        1: {
          '': threadPage([
            thread([
              comment(10, 'lrhn', '2026-10-22T10:00:00Z'),
              comment(11, 'reidbaker', '2026-10-22T11:00:00Z'),
            ]),
          ]),
        },
      },
    );
    expect((await scan(gh)).open.single.items, hasLength(1));
    expect((await scan(gh, ignore: {'reidbaker'})).open.single.items, isEmpty);
  });

  test('includes a merged PR inside the window and drops older ones', () async {
    final gh = FakeGh(
      merged: [
        prRef(120, mergedAt: '2026-10-22T14:36:58Z'),
        prRef(90, mergedAt: '2026-10-19T00:00:00Z'),
      ],
      threadPages: {
        120: {
          '': threadPage([
            thread([
              comment(4349658228, 'reidbaker', '2026-10-22T14:34:51Z'),
            ], outdated: true),
          ]),
        },
      },
    );
    final result = await scan(gh);
    expect(result.open, isEmpty);
    final pr = result.merged.single;
    expect(pr.number, 120);
    expect(pr.items.single.id, 4349658228);
    expect(pr.items.single.outdated, isTrue);
    final text = formatThreadReport(result);
    expect(text, contains('PR #120 [merged — needs a follow-up PR]'));
    expect(text, contains('a.md:3 (outdated)'));
  });

  test('reports a fetch failure instead of skipping the PR', () async {
    final gh = FakeGh(open: [prRef(1), prRef(2)], failPr: 2);
    final result = await scan(gh);
    expect(result.failures.single, allOf(contains('#2'), contains('502')));
    expect(formatThreadReport(result), contains('FETCH FAILURE'));
  });

  test('follows reviewThreads pagination', () async {
    final gh = FakeGh(
      open: [prRef(1)],
      threadPages: {
        1: {
          '': threadPage([
            thread([comment(1, 'lrhn', '2026-10-22T10:00:00Z')]),
          ], next: 'c1'),
          'c1': threadPage([
            thread([comment(2, 'lrhn', '2026-10-22T10:00:00Z')]),
          ]),
        },
      },
    );
    final result = await scan(gh);
    expect([for (final i in result.open.single.items) i.id], [1, 2]);
    expect(gh.calls.where((c) => c.contains('cursor=c1')), hasLength(1));
  });

  test('reads all REST pages and filters bots, agent and --since', () async {
    Map<String, Object?> user(String login, [String type = 'User']) => {
      'login': login,
      'type': type,
    };
    final gh = FakeGh(
      open: [prRef(1)],
      reviews: {
        1: [
          {
            'id': 7,
            'user': user('lrhn'),
            'body': 'nits',
            'state': 'COMMENTED',
            'submitted_at': '2026-10-22T09:00:00Z',
          },
          {
            'id': 8,
            'user': user('lrhn'),
            'body': '',
            'state': 'APPROVED',
            'submitted_at': '2026-10-22T09:00:00Z',
          },
        ],
      },
      comments: {
        1: [
          {
            'id': 5,
            'user': user('github-actions[bot]', 'Bot'),
            'body': 'x',
            'created_at': '2026-10-22T09:00:00Z',
          },
          {
            'id': 6,
            'user': user('reidbaker-agent'),
            'body': 'x',
            'created_at': '2026-10-22T09:00:00Z',
          },
          {
            'id': 9,
            'user': user('kevmoo'),
            'body': 'old',
            'created_at': '2026-10-01T09:00:00Z',
          },
        ],
      },
    );
    final all = await scan(gh);
    expect([for (final i in all.open.single.items) i.id], [7, 9]);
    expect(
      gh.calls.where((c) => c.contains('--paginate') && c.contains('--slurp')),
      hasLength(2),
    );
    final recent = await scan(gh, since: DateTime.utc(2026, 10, 20));
    expect([for (final i in recent.open.single.items) i.id], [7]);
  });

  test('parseDuration', () {
    expect(parseDuration('2d'), const Duration(days: 2));
    expect(parseDuration('36h'), const Duration(hours: 36));
    expect(parseDuration('2x'), isNull);
  });
}
