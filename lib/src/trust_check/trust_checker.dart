import 'dart:convert';

import '../pr_triage/github_fetcher.dart'
    show CommandRunner, defaultCommandRunner;
import 'trust_config.dart';

/// Why a login was or was not trusted.
class LoginVerdict {
  const LoginVerdict({
    required this.login,
    required this.trusted,
    required this.via,
  });

  /// The GitHub login, or `<unlinked:email>` for a commit with no linked
  /// account.
  final String login;
  final bool trusted;

  /// How trust was granted (`user`, `org:flutter`, `team:flutter/x`), or why
  /// it was not (`not a member`, `no linked GitHub account`).
  final String via;

  Map<String, dynamic> toJson() => {
    'login': login,
    'trusted': trusted,
    'via': via,
  };
}

/// Result of checking every person behind a pull request.
class TrustReport {
  const TrustReport({
    required this.repo,
    required this.number,
    required this.verdicts,
  });

  final String repo;
  final int number;
  final List<LoginVerdict> verdicts;

  /// True only when at least one login was checked and all were trusted.
  bool get allTrusted =>
      verdicts.isNotEmpty && verdicts.every((v) => v.trusted);

  List<String> get untrusted => [
    for (final v in verdicts)
      if (!v.trusted) v.login,
  ];

  Map<String, dynamic> toJson() => {
    'repo': repo,
    'number': number,
    'all_trusted': allTrusted,
    'untrusted': untrusted,
    'verdicts': [for (final v in verdicts) v.toJson()],
  };
}

/// Thrown when GitHub cannot be queried for the PR itself. Callers must treat
/// it as untrusted.
class TrustCheckException implements Exception {
  TrustCheckException(this.message);
  final String message;
  @override
  String toString() => message;
}

/// Decides whether the PR author and every commit author and committer are
/// trusted under a [TrustConfig].
///
/// Fails closed. GitHub answers 404 to an org membership query both for
/// non-members and for private members this token cannot see, so a missing
/// `read:org` scope makes a member look untrusted, never the reverse.
class TrustChecker {
  TrustChecker({CommandRunner? runner})
    : _runner = runner ?? defaultCommandRunner;

  final CommandRunner _runner;

  Future<TrustReport> check({
    required String repo,
    required int number,
    required TrustConfig config,
  }) async {
    final logins = await _collectLogins(repo, number);
    final verdicts = <LoginVerdict>[];
    for (final login in logins) {
      if (config.isIgnored(login)) continue;
      verdicts.add(await _verdictFor(login, config));
    }
    return TrustReport(repo: repo, number: number, verdicts: verdicts);
  }

  /// The PR author plus every commit author and committer, de-duplicated
  /// case-insensitively and sorted for stable output.
  Future<List<String>> _collectLogins(String repo, int number) async {
    final prRes = await _runner('gh', [
      'pr', 'view', '$number', '-R', repo, '--json', 'author', //
      '--jq', '.author.login',
    ]);
    if (prRes.exitCode != 0) {
      throw TrustCheckException(
        'could not read PR $repo#$number: ${prRes.stderr}'.trim(),
      );
    }

    final commitsRes = await _runner('gh', [
      'api',
      '--paginate',
      '--slurp',
      'repos/$repo/pulls/$number/commits',
    ]);
    if (commitsRes.exitCode != 0) {
      throw TrustCheckException(
        'could not list commits for $repo#$number: ${commitsRes.stderr}'.trim(),
      );
    }

    final seen = <String, String>{};
    void add(String login) =>
        seen.putIfAbsent(login.toLowerCase(), () => login);

    final author = (prRes.stdout as String).trim();
    if (author.isNotEmpty) add(author);

    for (final commit in _flattenPages(
      jsonDecode(commitsRes.stdout as String),
    )) {
      for (final role in const ['author', 'committer']) {
        final user = commit[role];
        final login = user is Map ? user['login'] as String? : null;
        if (login != null && login.isNotEmpty) {
          add(login);
        } else {
          final email = (commit['commit']?[role]?['email'] ?? 'unknown')
              .toString();
          add('<unlinked:$email>');
        }
      }
    }
    return seen.values.toList()..sort();
  }

  /// `--slurp` wraps each page in an outer list.
  static Iterable<Map<String, dynamic>> _flattenPages(dynamic decoded) sync* {
    if (decoded is! List) return;
    for (final page in decoded) {
      if (page is List) {
        yield* page.whereType<Map<String, dynamic>>();
      } else if (page is Map<String, dynamic>) {
        yield page;
      }
    }
  }

  Future<LoginVerdict> _verdictFor(String login, TrustConfig config) async {
    if (login.startsWith('<unlinked:')) {
      return LoginVerdict(
        login: login,
        trusted: false,
        via: 'no linked GitHub account',
      );
    }
    if (config.isTrustedUser(login)) {
      return LoginVerdict(login: login, trusted: true, via: 'user');
    }
    for (final org in config.trustedOrgs) {
      // 204 means member; 404 and 302 mean not visible as a member.
      final res = await _runner('gh', [
        'api',
        '--silent',
        'orgs/$org/members/$login',
      ]);
      if (res.exitCode == 0) {
        return LoginVerdict(login: login, trusted: true, via: 'org:$org');
      }
    }
    for (final team in config.trustedTeams) {
      final slash = team.indexOf('/');
      final res = await _runner('gh', [
        'api',
        'orgs/${team.substring(0, slash)}/teams/${team.substring(slash + 1)}'
            '/memberships/$login',
        '--jq',
        '.state',
      ]);
      if (res.exitCode == 0 && (res.stdout as String).trim() == 'active') {
        return LoginVerdict(login: login, trusted: true, via: 'team:$team');
      }
    }
    return LoginVerdict(login: login, trusted: false, via: 'not a member');
  }
}
