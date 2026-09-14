import 'dart:convert';

import 'package:file/file.dart';

import 'github_fetcher.dart';

/// Paginated org membership query. `membersWithRole` requires `read:org`.
const String _membersQuery = r'''
query($org: String!, $cursor: String) {
  organization(login: $org) {
    membersWithRole(first: 100, after: $cursor) {
      nodes { login }
      pageInfo { hasNextPage endCursor }
    }
  }
}
''';

/// Resolves GitHub organisation membership so that colleagues are recognised
/// as teammates rather than treated as external contributors.
///
/// Configuring an organisation name in `team_members` never worked: the value
/// was compared for exact equality against a PR author's login, so
/// `team_members: [flutter]` matched the string `flutter` and no human being.
/// This class expands each entry in `team_orgs` into the real member list.
///
/// Results are cached on disk because org membership changes slowly and the
/// query costs a round trip on every run.
class TeamResolver {
  TeamResolver({
    required this.fs,
    CommandRunner? runner,
    this.cacheTtl = const Duration(hours: 24),
    DateTime Function()? clock,
  })  : _runner = runner ?? defaultCommandRunner,
        _clock = clock ?? DateTime.now;

  final FileSystem fs;
  final CommandRunner _runner;
  final Duration cacheTtl;
  final DateTime Function() _clock;

  /// Returns the union of member logins across [orgs], lowercased.
  ///
  /// Never throws: membership lookup requires the `read:org` scope, which many
  /// tokens lack. On failure the caller falls back to the static
  /// `team_members` allowlist, so triage degrades rather than dies.
  Future<Set<String>> resolveMembers(
    List<String> orgs, {
    String? cachePath,
  }) async {
    if (orgs.isEmpty) return const {};

    final cached = _readCache(cachePath, orgs);
    if (cached != null) return cached;

    final members = <String>{};
    for (final org in orgs) {
      members.addAll(await _fetchOrgMembers(org));
    }

    if (members.isNotEmpty) {
      _writeCache(cachePath, orgs, members);
    }
    return members;
  }

  Future<Set<String>> _fetchOrgMembers(String org) async {
    final members = <String>{};
    String? cursor;
    // Bound the loop so a pagination bug cannot spin forever.
    for (var page = 0; page < 20; page++) {
      final connection = await _fetchMemberPage(org, cursor);
      if (connection == null) break;

      members.addAll(_loginsOf(connection['nodes']));

      cursor = _nextCursor(connection['pageInfo']);
      if (cursor == null) break;
    }
    return members;
  }

  /// Returns one `membersWithRole` page, or null on any failure.
  ///
  /// Failures are swallowed on purpose: membership lookup needs the `read:org`
  /// scope, and a token without it must degrade to the static allowlist rather
  /// than abort the whole triage run.
  Future<Map<dynamic, dynamic>?> _fetchMemberPage(
    String org,
    String? cursor,
  ) async {
    final args = <String>[
      'api',
      'graphql',
      '-f',
      'query=$_membersQuery',
      '-F',
      'org=$org',
      if (cursor != null) ...['-F', 'cursor=$cursor'],
    ];

    try {
      final res = await _runner('gh', args);
      if (res.exitCode != 0) return null;
      final data = jsonDecode(res.stdout.toString()) as Map<String, dynamic>;
      final connection = data['data']?['organization']?['membersWithRole'];
      return connection is Map ? connection : null;
    } catch (_) {
      return null;
    }
  }

  static Set<String> _loginsOf(dynamic nodes) {
    if (nodes is! List) return const {};
    return {
      for (final node in nodes)
        if (node is Map && node['login'] != null)
          if (node['login'].toString().trim().toLowerCase()
              case final String login when login.isNotEmpty)
            login,
    };
  }

  /// Returns the cursor for the next page, or null when this was the last.
  static String? _nextCursor(dynamic pageInfo) {
    if (pageInfo is! Map || pageInfo['hasNextPage'] != true) return null;
    final cursor = pageInfo['endCursor']?.toString();
    return (cursor == null || cursor.isEmpty) ? null : cursor;
  }


  Set<String>? _readCache(String? cachePath, List<String> orgs) {
    if (cachePath == null) return null;
    final file = fs.file(cachePath);
    if (!file.existsSync()) return null;

    try {
      final decoded = jsonDecode(file.readAsStringSync());
      if (decoded is! Map) return null;
      if (_cacheKey(orgs) != decoded['orgs']) return null;

      final fetchedAt = DateTime.tryParse(decoded['fetched_at']?.toString() ?? '');
      if (fetchedAt == null) return null;
      if (_clock().difference(fetchedAt) > cacheTtl) return null;

      final members = decoded['members'];
      if (members is! List) return null;
      return members.map((m) => m.toString().toLowerCase()).toSet();
    } catch (_) {
      return null;
    }
  }

  void _writeCache(String? cachePath, List<String> orgs, Set<String> members) {
    if (cachePath == null) return;
    try {
      final file = fs.file(cachePath);
      file.parent.createSync(recursive: true);
      file.writeAsStringSync(jsonEncode({
        'orgs': _cacheKey(orgs),
        'fetched_at': _clock().toIso8601String(),
        'members': members.toList()..sort(),
      }));
    } catch (_) {
      // A non-writable cache directory must not fail the run.
    }
  }

  static String _cacheKey(List<String> orgs) {
    final sorted = orgs.map((o) => o.toLowerCase()).toList()..sort();
    return sorted.join(',');
  }
}
