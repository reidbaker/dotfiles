@TestOn('posix')
library;

import 'dart:convert';
import 'dart:io';

import 'package:test/test.dart';

/// End-to-end checks of `--ready` exit codes and output. A fake `gh` on PATH
/// answers from fixture files, so no network is touched.
void main() {
  final head = 'c' * 40;
  final commits = jsonEncode([
    [
      {
        'sha': head,
        'parents': [
          {'sha': 'e' * 40},
        ],
        'commit': {
          'committer': {'date': '2026-11-01T00:00:00Z'},
        },
      },
    ],
  ]);
  final threads = jsonEncode({
    'data': {
      'repository': {
        'pullRequest': {
          'author': {'login': 'agent'},
          'headRefOid': head,
          'reviewThreads': {
            'pageInfo': {'hasNextPage': false},
            'nodes': <Object>[],
          },
        },
      },
    },
  });

  late Directory dir;
  late Map<String, String> env;

  setUp(() async {
    dir = await Directory.systemTemp.createTemp('fake_gh_ready');
    File('${dir.path}/commits.json').writeAsStringSync(commits);
    File('${dir.path}/threads.json').writeAsStringSync(threads);
    final gh = File('${dir.path}/gh')
      ..writeAsStringSync(
        '#!/bin/sh\n'
        'case "\$2" in\n'
        '  graphql) cat "${dir.path}/threads.json" ;;\n'
        '  *) cat "${dir.path}/commits.json" ;;\n'
        'esac\n',
      );
    await Process.run('chmod', ['+x', gh.path]);
    env = {'PATH': '${dir.path}:${Platform.environment['PATH']}'};
  });
  tearDown(() => dir.delete(recursive: true));

  Future<ProcessResult> run(List<String> args) => Process.run(
    Platform.resolvedExecutable,
    ['run', 'bin/pr_triage.dart', '--ready', ...args],
    environment: env,
  );

  String writeLog(String sha) {
    final path = '${dir.path}/review-log.jsonl';
    File(path).writeAsStringSync(
      '${jsonEncode({'event': 'review', 'skill': 'adversarial-review', 'repo': 'o/r', 'reviewed_sha': sha})}\n',
    );
    return path;
  }

  test('prints READY and exits 0 when the head is reviewed', () async {
    final res = await run(['--pr', 'o/r#7', '--review-log', writeLog(head)]);
    expect(res.exitCode, 0, reason: '${res.stdout}\n${res.stderr}');
    expect(res.stdout, 'READY o/r#7\n');
  });

  test('prints one line per PR and exits 1 when any is not ready', () async {
    final res = await run([
      '--pr',
      'o/r#7,x/y#8',
      '--review-log',
      writeLog(head),
    ]);
    expect(res.exitCode, 1, reason: '${res.stdout}\n${res.stderr}');
    expect(const LineSplitter().convert(res.stdout as String), [
      'READY o/r#7',
      startsWith('NOT READY x/y#8: no adversarial-review entry'),
    ]);
  });

  test('warns and is NOT READY when the log is missing', () async {
    final res = await run([
      '--pr',
      'o/r#7',
      '--review-log',
      '${dir.path}/missing.jsonl',
    ]);
    expect(res.exitCode, 1);
    expect(res.stderr, contains('no review log at'));
    expect(res.stdout, startsWith('NOT READY o/r#7'));
  });

  test('exits 64 without --pr', () async {
    final res = await run([]);
    expect(res.exitCode, 64);
    expect(res.stderr, contains('--ready requires --pr'));
  });
}
