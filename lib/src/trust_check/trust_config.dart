import 'package:yaml/yaml.dart';

/// Who counts as trusted when deciding whether a review may run a change's
/// code without a security pass first.
///
/// Loaded from `.agents/skills/adversarial-review/resources/config.yaml`.
/// That file is gitignored; `config.example.yaml` next to it documents the
/// keys.
class TrustConfig {
  const TrustConfig({
    this.trustedOrgs = kDefaultTrustedOrgs,
    this.trustedTeams = kDefaultTrustedTeams,
    this.trustedUsers = const [],
    this.ignoredLogins = kDefaultIgnoredLogins,
  });

  /// Parses configuration from a YAML string.
  ///
  /// An empty document gives the defaults. A key that is present but empty
  /// (`trusted_orgs: []`) is honored as empty, so a config can deliberately
  /// trust nobody by org.
  factory TrustConfig.fromYamlString(String content) {
    final doc = loadYaml(content);
    if (doc == null) return const TrustConfig();
    if (doc is! YamlMap) {
      throw const FormatException('Expected a YAML map at the root of config');
    }
    final teams = _list(doc, 'trusted_teams') ?? kDefaultTrustedTeams;
    for (final team in teams) {
      if (!_teamPattern.hasMatch(team)) {
        throw FormatException(
          'trusted_teams entries must look like "org/team-slug", got "$team"',
        );
      }
    }
    return TrustConfig(
      trustedOrgs: _list(doc, 'trusted_orgs') ?? kDefaultTrustedOrgs,
      trustedTeams: teams,
      trustedUsers: _list(doc, 'trusted_users') ?? const [],
      ignoredLogins: _list(doc, 'ignored_logins') ?? kDefaultIgnoredLogins,
    );
  }

  static const kDefaultTrustedOrgs = ['flutter', 'dart-lang'];
  static const kDefaultTrustedTeams = ['flutter/flutter-hackers'];

  /// GitHub's own committer for web edits and merges. It is not a person, and
  /// the real author of such a commit is checked separately.
  static const kDefaultIgnoredLogins = ['web-flow'];

  static final _teamPattern = RegExp(r'^[^/\s]+/[^/\s]+$');

  /// Members of these GitHub organizations are trusted.
  final List<String> trustedOrgs;

  /// Active members of these teams are trusted. Each entry is `org/team-slug`.
  final List<String> trustedTeams;

  /// Individual logins that are trusted. Compared case-insensitively.
  final List<String> trustedUsers;

  /// Logins skipped entirely, such as GitHub's `web-flow` committer.
  final List<String> ignoredLogins;

  bool isTrustedUser(String login) =>
      trustedUsers.any((u) => u.toLowerCase() == login.toLowerCase());

  bool isIgnored(String login) =>
      ignoredLogins.any((u) => u.toLowerCase() == login.toLowerCase());

  Map<String, dynamic> toJson() => {
    'trusted_orgs': trustedOrgs,
    'trusted_teams': trustedTeams,
    'trusted_users': trustedUsers,
    'ignored_logins': ignoredLogins,
  };

  static List<String>? _list(YamlMap map, String key) {
    if (!map.containsKey(key)) return null;
    final value = map[key];
    if (value == null) return const [];
    if (value is! YamlList) {
      throw FormatException('"$key" must be a list');
    }
    return [
      for (final v in value)
        if (v != null && v.toString().trim().isNotEmpty) v.toString().trim(),
    ];
  }
}
