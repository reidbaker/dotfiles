import 'dart:convert';
import 'dart:io';

import 'package:dotfiles/src/pr_triage/github_fetcher.dart';
import 'package:dotfiles/src/pr_triage/ready_check.dart';
import 'package:test/test.dart';

const _spec = PrSpec('o/r', 7);

// Full 40-character SHAs, so prefix matching is exercised for real.
final _a = 'a' * 40;
final _b = 'b' * 40;
final _c = 'c' * 40;
final _m = 'd' * 40;
final _base = 'e' * 40;

final _t0 = DateTime.utc(2026, 11, 1);
DateTime _at(int hours) => _t0.add(Duration(hours: hours));

PrCommit _commit(String sha, List<String> parents, int hour) =>
    PrCommit(sha: sha, parents: parents, committedAt: _at(hour));

/// base <- A <- B <- C, all on the PR.
List<PrCommit> _linear() => [
  _commit(_a, [_base], 0),
  _commit(_b, [_a], 1),
  _commit(_c, [_b], 2),
];

ThreadComment _comment(
  String author,
  int hour, {
  bool bot = false,
  bool outdated = false,
}) => ThreadComment(
  author: author,
  isBot: bot,
  createdAt: _at(hour),
  url: 'https://github.com/o/r/pull/7#c-$author-$hour',
  outdated: outdated,
);

PrSnapshot _pr({
  List<PrCommit>? commits,
  List<ReviewThread> threads = const [],
  List<ReviewState> opinionatedReviews = const [],
  String author = 'agent',
}) {
  final list = commits ?? _linear();
  return PrSnapshot(
    author: author,
    headSha: list.last.sha,
    commits: list,
    threads: threads,
    opinionatedReviews: opinionatedReviews,
  );
}

String _logLine(Map<String, Object?> fields) => jsonEncode({
  'event': 'review',
  'skill': 'adversarial-review',
  'repo': 'o/r',
  ...fields,
});

void main() {
  group('parseReviewLog', _parseReviewLogTests);
  group('unreviewedCommits', _unreviewedCommitsTests);
  group('isBotAccount', _isBotAccountTests);
  group('unansweredThreads', _unansweredThreadsTests);
  group('judgeReadiness', _judgeReadinessTests);
  group('ReadyChecker', _readyCheckerTests);
}

void _parseReviewLogTests() {
  test('keeps adversarial-review entries with a valid SHA', () {
    final log = parseReviewLog(
      [
        _logLine({'reviewed_sha': 'ABCDEF1'}),
        _logLine({'repo': 'O/R', 'reviewed_sha': _b}),
      ].join('\n'),
    );
    expect(log.map((e) => e.sha), ['abcdef1', _b]);
    expect(log.map((e) => e.repo), ['o/r', 'o/r']);
  });

  test('falls back to the older sha field', () {
    final log = parseReviewLog(_logLine({'sha': 'f02ef43027'}));
    expect(log.single.sha, 'f02ef43027');
  });

  test('skips lines that are not adversarial-review entries', () {
    final text = [
      '',
      'not json',
      '[1, 2]',
      _logLine({'skill': 'persona-review', 'reviewed_sha': _a}),
      _logLine({'event': 'ready', 'reviewed_sha': _a}),
      _logLine({'reviewed_sha': 'uncommitted-diff-on-76fca44'}),
      _logLine({'reviewed_sha': 'abc12'}),
      _logLine({}),
      jsonEncode({
        'event': 'review',
        'skill': 'adversarial-review',
        'reviewed_sha': _a,
      }),
    ].join('\n');
    expect(parseReviewLog(text), isEmpty);
  });
}

void _unreviewedCommitsTests() {
  test('returns nothing when the head is reviewed', () {
    expect(unreviewedCommits(_pr(), [_c]), isEmpty);
  });

  test('a short logged SHA matches as a prefix', () {
    expect(unreviewedCommits(_pr(), ['ccccccc']), isEmpty);
  });

  test('lists code commits after a reviewed ancestor', () {
    final missing = unreviewedCommits(_pr(), [_a]);
    expect(missing!.map((c) => c.sha), [_b, _c]);
  });

  test('later commits reviewed by later entries are covered', () {
    // Each fix commit has its own entry, as after rounds of fixes.
    expect(unreviewedCommits(_pr(), [_a, _b, _c]), isEmpty);
  });

  test('a merge commit after the reviewed commit needs no review', () {
    final commits = [
      ..._linear(),
      _commit(_m, [_c, _base], 3),
    ];
    expect(unreviewedCommits(_pr(commits: commits), [_c]), isEmpty);
  });

  test('a code commit after a merge still needs review', () {
    final commits = [
      ..._linear(),
      _commit(_m, [_c, _base], 3),
      _commit('f' * 40, [_m], 4),
    ];
    final missing = unreviewedCommits(_pr(commits: commits), [_c]);
    expect(missing!.map((c) => c.sha), ['f' * 40]);
  });

  test('a side branch is not covered by a review of the other side', () {
    // base <- A <- B, and base <- C, merged at M.
    final commits = [
      _commit(_a, [_base], 0),
      _commit(_b, [_a], 1),
      _commit(_c, [_base], 2),
      _commit(_m, [_b, _c], 3),
    ];
    final missing = unreviewedCommits(_pr(commits: commits), [_b]);
    expect(missing!.map((c) => c.sha), [_c]);
  });

  test('returns null when no logged SHA is on the PR', () {
    // A SHA from before a rebase, and the base commit itself.
    expect(unreviewedCommits(_pr(), ['1234567', _base]), isNull);
  });
}

void _isBotAccountTests() {
  test('detects bots by GraphQL type or REST suffix', () {
    expect(isBotAccount('gemini-code-assist', 'Bot'), isTrue);
    expect(isBotAccount('dependabot[bot]', null), isTrue);
    expect(isBotAccount('matt561', 'User'), isFalse);
    expect(isBotAccount('reidbaker-agent', 'User'), isFalse);
  });
}

void _unansweredThreadsTests() {
  ReviewThread thread(List<ThreadComment> comments, {bool resolved = false}) =>
      ReviewThread(resolved: resolved, comments: comments);

  // The linear PR's last code commit is at hour 2.
  test('a reviewer comment after the last commit waits', () {
    final waiting = unansweredThreads(
      _pr(
        threads: [
          thread([_comment('matt561', 5)]),
        ],
      ),
      const {},
    );
    expect(waiting.single.author, 'matt561');
  });

  test('a reply from the PR author answers it', () {
    final pr = _pr(
      threads: [
        thread([_comment('matt561', 5), _comment('agent', 6)]),
      ],
    );
    expect(unansweredThreads(pr, const {}), isEmpty);
  });

  test('a reply from an answerer answers it, ignoring case', () {
    final pr = _pr(
      author: 'reidbaker',
      threads: [
        thread([_comment('matt561', 5), _comment('ReidBaker-Agent', 6)]),
      ],
    );
    expect(unansweredThreads(pr, {'reidbaker-agent'}), isEmpty);
    expect(unansweredThreads(pr, const {}), hasLength(1));
  });

  test('with answerers given, the PR author is a reviewer', () {
    // The human and the agent can share the PR author's account.
    final pr = _pr(
      author: 'reidbaker',
      threads: [
        thread([_comment('reidbaker', 5)]),
      ],
    );
    expect(unansweredThreads(pr, {'reidbaker-agent'}), hasLength(1));
    expect(unansweredThreads(pr, const {}), isEmpty);
  });

  test('an outdated comment with a later commit is answered by code', () {
    final pr = _pr(
      threads: [
        thread([_comment('matt561', 1, outdated: true)]),
      ],
    );
    expect(unansweredThreads(pr, const {}), isEmpty);
  });

  test('a later commit that left the lines alone does not answer it', () {
    // A rebase re-dates every commit, so dates alone prove nothing.
    final pr = _pr(
      threads: [
        thread([_comment('matt561', 1)]),
      ],
    );
    expect(unansweredThreads(pr, const {}), hasLength(1));
  });

  test('a follow-up in an outdated thread needs a reply', () {
    // GitHub marks the whole thread outdated, follow-ups included.
    final pr = _pr(
      threads: [
        thread([
          _comment('matt561', 0, outdated: true),
          _comment('agent', 0, outdated: true),
          _comment('matt561', 1, outdated: true),
        ]),
      ],
    );
    expect(unansweredThreads(pr, const {}).single.createdAt, _at(1));
  });

  test('an outdated comment with no later commit waits', () {
    final pr = _pr(
      threads: [
        thread([_comment('matt561', 5, outdated: true)]),
      ],
    );
    expect(unansweredThreads(pr, const {}), hasLength(1));
  });

  test('a merge commit after the comment does not answer it', () {
    final commits = [
      ..._linear(),
      _commit(_m, [_c, _base], 9),
    ];
    final pr = _pr(
      commits: commits,
      threads: [
        thread([_comment('matt561', 5, outdated: true)]),
      ],
    );
    expect(unansweredThreads(pr, const {}), hasLength(1));
  });

  test('a reviewer reply after the author reply waits again', () {
    final pr = _pr(
      threads: [
        thread([
          _comment('matt561', 5),
          _comment('agent', 6),
          _comment('matt561', 7),
        ]),
      ],
    );
    expect(unansweredThreads(pr, const {}).single.createdAt, _at(7));
  });

  test('a bot reply does not answer a human comment', () {
    final pr = _pr(
      threads: [
        thread([_comment('matt561', 5), _comment('ci', 6, bot: true)]),
      ],
    );
    expect(unansweredThreads(pr, const {}).single.author, 'matt561');
  });

  test('resolved threads and bot-only threads are skipped', () {
    final pr = _pr(
      threads: [
        thread([_comment('matt561', 5)], resolved: true),
        thread([_comment('gemini-code-assist', 5, bot: true)]),
        thread(const []),
      ],
    );
    expect(unansweredThreads(pr, const {}), isEmpty);
  });
}

void _judgeReadinessTests() {
  List<ReviewedSha> log(List<String> shas, {String repo = 'o/r'}) => [
    for (final s in shas) ReviewedSha(repo, s),
  ];

  test('READY when the head is reviewed and no thread waits', () {
    final verdict = judgeReadiness(_spec, _pr(), log([_c]));
    expect(verdict.ready, isTrue);
    expect(verdict.line, 'READY o/r#7');
  });

  test('NOT READY when nothing in the log matches', () {
    final verdict = judgeReadiness(_spec, _pr(), log([]));
    expect(
      verdict.line,
      'NOT READY o/r#7: no adversarial-review entry in the review log '
      'for any commit on this PR (head cccccccccc)',
    );
  });

  test('entries for another repo do not count', () {
    final verdict = judgeReadiness(_spec, _pr(), log([_c], repo: 'x/r'));
    expect(verdict.ready, isFalse);
  });

  test('names the uncovered commits', () {
    final verdict = judgeReadiness(_spec, _pr(), log([_a]));
    expect(
      verdict.line,
      'NOT READY o/r#7: commits no adversarial-review entry covers: '
      'bbbbbbbbbb, cccccccccc',
    );
  });

  test('names the waiting threads and joins reasons', () {
    final pr = _pr(
      threads: [
        ReviewThread(resolved: false, comments: [_comment('matt561', 5)]),
      ],
    );
    final verdict = judgeReadiness(_spec, pr, log([_a]));
    expect(
      verdict.line,
      'NOT READY o/r#7: commits no adversarial-review entry covers: '
      'bbbbbbbbbb, cccccccccc; 1 human review thread without a reply or '
      'code change: https://github.com/o/r/pull/7#c-matt561-5',
    );
  });

  test('NOT READY when a human reviewer requests changes', () {
    final pr = _pr(
      opinionatedReviews: const [
        ReviewState(
          author: 'matt561',
          isBot: false,
          state: 'CHANGES_REQUESTED',
        ),
        ReviewState(author: 'lint', isBot: true, state: 'CHANGES_REQUESTED'),
        ReviewState(author: 'reidbaker', isBot: false, state: 'APPROVED'),
      ],
    );
    expect(
      judgeReadiness(_spec, pr, log([_c])).line,
      'NOT READY o/r#7: changes requested by matt561',
    );
  });

  test('NOT READY when the head is missing from the commit list', () {
    // GitHub's head moved after the commit list was fetched.
    final pr = PrSnapshot(
      author: 'agent',
      headSha: 'f' * 40,
      commits: _linear(),
      threads: const [],
    );
    expect(
      judgeReadiness(_spec, pr, log([_c])).line,
      'NOT READY o/r#7: head ffffffffff is not in the commit list GitHub '
      'returned; run again',
    );
  });

  test('shortens long lists', () {
    final commits = [
      for (var i = 0; i < 8; i++)
        _commit('$i' * 40, [if (i > 0) '${i - 1}' * 40], i),
    ];
    final verdict = judgeReadiness(
      _spec,
      _pr(commits: commits),
      log(['0' * 40]),
    );
    expect(verdict.line, endsWith('and 2 more'));
  });

  test('refuses to judge a PR with too many commits to list', () {
    final commits = [
      for (var i = 0; i < maxListedCommits; i++)
        _commit(i.toRadixString(16).padLeft(40, '0'), const [], 0),
    ];
    final verdict = judgeReadiness(
      _spec,
      _pr(commits: commits),
      log([commits.last.sha]),
    );
    expect(verdict.line, contains('250 or more commits'));
  });
}

void _readyCheckerTests() {
  Map<String, Object?> restCommit(String sha, List<String> parents, int h) => {
    'sha': sha,
    'parents': [
      for (final p in parents) {'sha': p},
    ],
    'commit': {
      'committer': {'date': _at(h).toIso8601String()},
    },
  };

  Map<String, Object?> threadPage(
    List<Map<String, Object?>> nodes, {
    String? next,
  }) => {
    'data': {
      'repository': {
        'pullRequest': {
          'author': {'login': 'agent'},
          'headRefOid': _c,
          'latestOpinionatedReviews': {
            'nodes': [
              {
                'state': 'CHANGES_REQUESTED',
                'author': {'login': 'matt561', '__typename': 'User'},
              },
            ],
          },
          'reviewThreads': {
            'pageInfo': {'hasNextPage': next != null, 'endCursor': next},
            'nodes': nodes,
          },
        },
      },
    },
  };

  Map<String, Object?> threadNode(String login, String type, int h) => {
    'isResolved': false,
    'comments': {
      'nodes': [
        {
          'author': {'login': login, '__typename': type},
          'createdAt': _at(h).toIso8601String(),
          'url': 'u-$login',
          'outdated': type == 'Bot',
        },
      ],
    },
  };

  test('fetches commits and every page of threads', () async {
    final calls = <List<String>>[];
    Future<ProcessResult> runner(String exe, List<String> args) async {
      calls.add(args);
      Object? body;
      if (args[1].endsWith('/commits')) {
        body = [
          [
            restCommit(_a, [_base], 0),
            restCommit(_b, [_a], 1),
          ],
          [
            restCommit(_c, [_b], 2),
          ],
        ];
      } else if (!args.contains('cursor=p2')) {
        body = threadPage([threadNode('matt561', 'User', 5)], next: 'p2');
      } else {
        body = threadPage([threadNode('gemini-code-assist', 'Bot', 5)]);
      }
      return ProcessResult(0, 0, jsonEncode(body), '');
    }

    final pr = await ReadyChecker(runner: runner).fetch(_spec);
    expect(pr.author, 'agent');
    expect(pr.headSha, _c);
    expect(pr.commits.map((c) => c.sha), [_a, _b, _c]);
    expect(pr.commits.first.parents, [_base]);
    expect(pr.threads, hasLength(2));
    expect(pr.threads.first.comments.single.outdated, isFalse);
    expect(pr.threads.last.comments.single.isBot, isTrue);
    expect(pr.threads.last.comments.single.outdated, isTrue);
    expect(pr.opinionatedReviews.single.author, 'matt561');
    expect(pr.opinionatedReviews.single.state, 'CHANGES_REQUESTED');
    expect(calls.first, contains('repos/o/r/pulls/7/commits'));
    expect(calls, hasLength(3));

    final verdict = judgeReadiness(_spec, pr, [ReviewedSha('o/r', _c)]);
    expect(verdict.line, contains('1 human review thread'));
    expect(verdict.line, contains('u-matt561'));
    expect(verdict.line, contains('changes requested by matt561'));
  });

  test('a failed fetch is NOT READY with the error', () async {
    Future<ProcessResult> runner(String exe, List<String> args) async =>
        ProcessResult(0, 1, '', 'HTTP 502');

    final verdict = await ReadyChecker(runner: runner).check(_spec, const []);
    expect(verdict.ready, isFalse);
    expect(verdict.line, startsWith('NOT READY o/r#7: fetch or parse failed:'));
    expect(verdict.line, contains('HTTP 502'));
  });

  test('GraphQL errors are a failed fetch', () {
    expect(
      () => parseThreadPage({
        'errors': [
          {'message': 'Could not resolve to a PullRequest'},
        ],
      }),
      throwsFormatException,
    );
  });
}
