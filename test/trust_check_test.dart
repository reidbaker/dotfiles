import 'dart:convert';
import 'dart:io';

import 'package:dotfiles/src/trust_check/trust_checker.dart';
import 'package:dotfiles/src/trust_check/trust_config.dart';
import 'package:test/test.dart';

/// Fake `gh` that answers from a small in-memory model of GitHub.
class FakeGitHub {
  FakeGitHub({
    required this.prAuthor,
    this.commits = const [],
    this.orgMembers = const {},
    this.teamMembers = const {},
    this.failPr = false,
  });

  final String prAuthor;
  final List<Map<String, dynamic>> commits;
  final Map<String, Set<String>> orgMembers;
  final Map<String, Set<String>> teamMembers;
  final bool failPr;
  final calls = <List<String>>[];

  Future<ProcessResult> run(String exe, List<String> args) async {
    calls.add(args);
    ProcessResult ok(String out) => ProcessResult(0, 0, out, '');
    final fail = ProcessResult(0, 1, '', 'HTTP 404');

    if (args.first == 'pr') return failPr ? fail : ok('$prAuthor\n');
    final path = args.firstWhere((a) => a.contains('/'));
    if (path.endsWith('/commits')) return ok(jsonEncode([commits]));

    final org = RegExp(r'^orgs/([^/]+)/members/(.+)$').firstMatch(path);
    if (org != null) {
      return (orgMembers[org[1]] ?? {}).contains(org[2]) ? ok('') : fail;
    }
    final team = RegExp(r'^orgs/([^/]+)/teams/([^/]+)/memberships/(.+)$')
        .firstMatch(path);
    if (team != null) {
      final members = teamMembers['${team[1]}/${team[2]}'] ?? {};
      return members.contains(team[3]) ? ok('active\n') : fail;
    }
    throw StateError('unexpected gh call: $args');
  }
}

Map<String, dynamic> commit(String? author, {String? committer = 'web-flow'}) =>
    {
      'author': author == null ? null : {'login': author},
      'committer': committer == null ? null : {'login': committer},
      'commit': {
        'author': {'email': '${author ?? 'ghost'}@example.com'},
        'committer': {'email': 'noreply@github.com'},
      },
    };

Future<TrustReport> check(FakeGitHub gh, [TrustConfig? config]) =>
    TrustChecker(runner: gh.run).check(
      repo: 'flutter/flutter',
      number: 1,
      config: config ?? const TrustConfig(),
    );

void main() {
  group('TrustConfig', () {
    test('an empty file gives the defaults', () {
      final c = TrustConfig.fromYamlString('');
      expect(c.trustedOrgs, ['flutter', 'dart-lang']);
      expect(c.trustedTeams, ['flutter/flutter-hackers']);
      expect(c.ignoredLogins, ['web-flow']);
    });

    test('an explicit empty list is honored, not defaulted', () {
      final c = TrustConfig.fromYamlString('trusted_orgs: []');
      expect(c.trustedOrgs, isEmpty);
      expect(c.trustedTeams, ['flutter/flutter-hackers']);
    });

    test('reads every key', () {
      final c = TrustConfig.fromYamlString('''
trusted_orgs: [google]
trusted_teams: [google/reviewers]
trusted_users: [Alice]
ignored_logins: [bot]
''');
      expect(c.trustedOrgs, ['google']);
      expect(c.trustedTeams, ['google/reviewers']);
      expect(c.isTrustedUser('alice'), isTrue);
      expect(c.isIgnored('BOT'), isTrue);
    });

    test('rejects a team without an org', () {
      expect(
        () => TrustConfig.fromYamlString('trusted_teams: [flutter-hackers]'),
        throwsFormatException,
      );
    });

    test('rejects a scalar where a list is expected', () {
      expect(
        () => TrustConfig.fromYamlString('trusted_orgs: flutter'),
        throwsFormatException,
      );
    });
  });

  group('TrustChecker', () {
    test('an org member with their own commits is trusted', () async {
      final report = await check(
        FakeGitHub(
          prAuthor: 'LouiseHsu',
          commits: [commit('LouiseHsu')],
          orgMembers: {
            'flutter': {'LouiseHsu'},
          },
        ),
      );
      expect(report.allTrusted, isTrue);
      expect(report.verdicts.single.via, 'org:flutter');
    });

    test('web-flow is skipped', () async {
      final gh = FakeGitHub(
        prAuthor: 'a',
        commits: [commit('a')],
        orgMembers: {
          'flutter': {'a'},
        },
      );
      final report = await check(gh);
      expect(report.verdicts.map((v) => v.login), ['a']);
      expect(gh.calls.any((c) => c.join(' ').contains('web-flow')), isFalse);
    });

    test('a team member is trusted through the team', () async {
      final report = await check(
        FakeGitHub(
          prAuthor: 'hacker',
          commits: [commit('hacker')],
          teamMembers: {
            'flutter/flutter-hackers': {'hacker'},
          },
        ),
      );
      expect(report.allTrusted, isTrue);
      expect(report.verdicts.single.via, 'team:flutter/flutter-hackers');
    });

    test('an outsider author is untrusted', () async {
      final report = await check(
        FakeGitHub(prAuthor: 'stranger', commits: [commit('stranger')]),
      );
      expect(report.allTrusted, isFalse);
      expect(report.untrusted, ['stranger']);
    });

    test('a trusted author with an untrusted committer is untrusted', () async {
      // The last editor matters: someone else can push to the PR branch.
      final report = await check(
        FakeGitHub(
          prAuthor: 'member',
          commits: [
            commit('member'),
            commit('member', committer: 'stranger'),
          ],
          orgMembers: {
            'flutter': {'member'},
          },
        ),
      );
      expect(report.untrusted, ['stranger']);
    });

    test('a commit with no linked account is untrusted', () async {
      final report = await check(
        FakeGitHub(
          prAuthor: 'member',
          commits: [commit(null)],
          orgMembers: {
            'flutter': {'member'},
          },
        ),
      );
      expect(report.allTrusted, isFalse);
      expect(report.untrusted.single, startsWith('<unlinked:'));
    });

    test('trusted_users needs no API call', () async {
      final gh = FakeGitHub(prAuthor: 'friend', commits: [commit('friend')]);
      final report = await check(
        gh,
        const TrustConfig(trustedUsers: ['friend']),
      );
      expect(report.allTrusted, isTrue);
      expect(gh.calls.where((c) => c.join(' ').contains('members')), isEmpty);
    });

    test('configured orgs replace the defaults', () async {
      final report = await check(
        FakeGitHub(
          prAuthor: 'm',
          commits: [commit('m')],
          orgMembers: {
            'flutter': {'m'},
          },
        ),
        const TrustConfig(trustedOrgs: ['google'], trustedTeams: []),
      );
      expect(report.allTrusted, isFalse);
    });

    test('a login is checked once despite case differences', () async {
      final gh = FakeGitHub(
        prAuthor: 'Alice',
        commits: [commit('alice', committer: 'ALICE')],
        orgMembers: {
          'flutter': {'Alice'},
        },
      );
      final report = await check(gh);
      expect(report.verdicts, hasLength(1));
    });

    test('an unreadable PR throws instead of reporting trusted', () async {
      expect(
        check(FakeGitHub(prAuthor: 'x', failPr: true)),
        throwsA(isA<TrustCheckException>()),
      );
    });

    test('nothing checked is not "all trusted"', () {
      const report = TrustReport(repo: 'r/r', number: 1, verdicts: []);
      expect(report.allTrusted, isFalse);
    });
  });
}
