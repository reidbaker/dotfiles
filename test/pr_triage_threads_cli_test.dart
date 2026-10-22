import 'dart:convert';
import 'dart:io';

import 'package:test/test.dart';

/// End-to-end check that `--threads` exits non-zero when a fetch fails. A
/// fake `gh` on PATH fails every call, so no network is touched.
void main() {
  test('--threads exits non-zero when gh fails', () async {
    final dir = await Directory.systemTemp.createTemp('fake_gh');
    addTearDown(() => dir.delete(recursive: true));
    final gh = File('${dir.path}/gh')
      ..writeAsStringSync('#!/bin/sh\necho "HTTP 502" >&2\nexit 1\n');
    await Process.run('chmod', ['+x', gh.path]);

    final res = await Process.run(
      Platform.resolvedExecutable,
      ['run', 'bin/pr_triage.dart', '--threads', '--repo', 'o/r'],
      environment: {'PATH': '${dir.path}:${Platform.environment['PATH']}'},
    );
    expect(res.exitCode, 1, reason: '${res.stdout}\n${res.stderr}');
    expect(res.stderr, contains('fetch failed'));
  }, testOn: 'posix');

  test('--threads exits non-zero when a follow-up lookup fails', () async {
    final dir = await Directory.systemTemp.createTemp('fake_gh');
    addTearDown(() => dir.delete(recursive: true));
    final now = DateTime.now().toUtc();
    final mergedAt = now.subtract(const Duration(hours: 1)).toIso8601String();
    final repliedAt = now.toIso8601String();
    final page = jsonEncode({
      'data': {
        'repository': {
          'pullRequest': {
            'reviewThreads': {
              'pageInfo': {'hasNextPage': false},
              'nodes': [
                {
                  'isResolved': false,
                  'path': 'a.md',
                  'line': 1,
                  'comments': {
                    'nodes': [
                      {
                        'databaseId': 1,
                        'author': {'login': 'bot'},
                        'createdAt': repliedAt,
                        'body': 'see #9',
                      },
                    ],
                  },
                },
              ],
            },
          },
        },
      },
    });
    final merged = jsonEncode({
      'data': {
        'repository': {
          'pullRequests': {
            'pageInfo': {'hasNextPage': false},
            'nodes': [
              {
                'number': 1,
                'title': 't',
                'url': 'u',
                'mergedAt': mergedAt,
                'updatedAt': mergedAt,
              },
            ],
          },
        },
      },
    });
    File('${dir.path}/merged.json').writeAsStringSync(merged);
    File('${dir.path}/page.json').writeAsStringSync(page);
    final gh = File('${dir.path}/gh')
      ..writeAsStringSync("""#!/bin/sh
d=${dir.path}
case "\$*" in
  *"--state open"*) echo '[]' ;;
  *pullRequests*) cat "\$d/merged.json" ;;
  *graphql*) cat "\$d/page.json" ;;
  *--paginate*) echo '[[]]' ;;
  *) echo "HTTP 502" >&2; exit 1 ;;
esac
""");
    await Process.run('chmod', ['+x', gh.path]);

    final res = await Process.run(
      Platform.resolvedExecutable,
      [
        'run',
        'bin/pr_triage.dart',
        '--threads',
        '--repo',
        'o/r',
        '--agent',
        'bot',
      ],
      environment: {'PATH': '${dir.path}:${Platform.environment['PATH']}'},
    );
    expect(res.exitCode, 1, reason: '${res.stdout}\n${res.stderr}');
    expect(res.stderr, contains('repos/o/r/issues/9'));
  }, testOn: 'posix');

  test('--threads without --repo is a usage error', () async {
    final res = await Process.run(Platform.resolvedExecutable, [
      'run',
      'bin/pr_triage.dart',
      '--threads',
    ]);
    expect(res.exitCode, 64);
  });
}
