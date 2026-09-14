import 'package:file/file.dart';
import 'package:yaml/yaml.dart';

/// Default labels that mean "CI cannot proceed until something external
/// resolves". Sourced from the actual label registries of `flutter/flutter`
/// and `flutter/packages`.
const List<String> kDefaultCicdBlockedLabels = [
  'waiting for tree to go green',
  'waiting for code freeze',
  'waiting for tree status',
];

/// Default labels that mean "the ball is in the author's court".
const List<String> kDefaultWaitingLabels = [
  'waiting for author',
  'waiting for pr author',
  'waiting for response',
  'work in progress (do not review)',
];

/// Default labels that mean "the author has not signed the CLA".
///
/// Kept separate from [kDefaultWaitingLabels] because an unsigned CLA blocks
/// every downstream action, which is a stronger signal than a generic
/// "waiting on author" state.
const List<String> kDefaultClaMissingLabels = [
  'cla: no',
  'needs cla',
  'cla missing',
];

/// Default check-name substrings treated as infrastructure flakes.
const List<String> kDefaultFlakyKeywords = [
  'flaky',
  'tree-status',
  'tree status',
  'mac_android',
  'flakiness',
];

/// Repositories that gate presubmit CI behind an explicit trigger label,
/// mapped to the label that starts the run.
///
/// Scoped per repository on purpose: applying a global rule caused every
/// `dart-lang` PR to be told to add a `CICD` label that does not exist there.
const Map<String, String> kDefaultPresubmitTriggers = {
  'flutter/flutter': 'CICD',
  'flutter/packages': 'CICD',
};

/// GitHub's hard cap on the `search` connection's `first` argument.
/// Exceeding it returns `EXCESSIVE_PAGINATION` and fails the whole query.
const int kMaxSearchPageSize = 100;

/// A per-PR annotation that adjusts how a specific pull request is triaged.
class PrOverride {
  const PrOverride({
    this.demote = false,
    this.tier,
    this.action,
    this.note,
  });

  /// Parses an override entry.
  ///
  /// A bare string is treated as a note only. Burying a PR requires an
  /// explicit `demote: true`, so a passing comment cannot silently drop a
  /// pull request to the bottom of the queue.
  factory PrOverride.fromYaml(dynamic value) {
    if (value is String) {
      return PrOverride(note: value);
    }
    if (value is Map) {
      final demote = value['demote'];
      if (demote != null && demote is! bool) {
        throw FormatException(
            'pr_overrides: "demote" must be true or false, got "$demote".');
      }
      return PrOverride(
        demote: (demote as bool?) ?? false,
        tier: value['tier']?.toString(),
        action: value['action']?.toString(),
        note: (value['note'] ?? value['reason'])?.toString(),
      );
    }
    return const PrOverride();
  }

  /// Forces the PR to the lowest tier of its queue.
  final bool demote;

  /// Pins the PR to a named tier, e.g. `readyToMerge`. Takes precedence over
  /// [demote]. Must match a `MyWorkTier` or `ReviewQueueTier` enum name.
  final String? tier;

  /// Replaces the tier's default action prompt.
  final String? action;

  /// Free-form annotation surfaced as the classification reason.
  final String? note;

  /// True when this override changes placement rather than only annotating.
  bool get changesPlacement => demote || tier != null;

  Map<String, dynamic> toJson() => {
        'demote': demote,
        if (tier != null) 'tier': tier,
        if (action != null) 'action': action,
        if (note != null) 'note': note,
      };
}

/// Configuration for the PR triage skill.
class TriageConfig {
  const TriageConfig({
    required this.accounts,
    this.teamMembers = const [],
    this.teamOrgs = const [],
    this.primaryOrgs = const ['flutter', 'dart-lang'],
    this.primaryRepos = const [],
    this.staleReviewBusinessDays = 3,
    this.flakyTestKeywords = kDefaultFlakyKeywords,
    this.cicdBlockedLabels = kDefaultCicdBlockedLabels,
    this.presubmitTriggers = kDefaultPresubmitTriggers,
    this.waitingLabels = kDefaultWaitingLabels,
    this.claMissingLabels = kDefaultClaMissingLabels,
    this.holidays = const [],
    this.prOverrides = const {},
    this.queryLimit = 50,
    this.searchAssignee = false,
  });

  /// Factory that builds a default config around the current GitHub user.
  factory TriageConfig.defaultForUser(String username) {
    return TriageConfig(accounts: [username]);
  }

  /// Parses configuration from a YAML string.
  factory TriageConfig.fromYamlString(String content) {
    final doc = loadYaml(content);
    if (doc is! YamlMap) {
      throw const FormatException('Expected YAML map at root of config');
    }
    return TriageConfig.fromYamlMap(doc);
  }

  /// Parses configuration from a [YamlMap].
  factory TriageConfig.fromYamlMap(YamlMap map) {
    final accounts = _stringList(map, ['accounts']);
    final primaryOrgs = _stringList(map, ['primary_orgs', 'primaryOrgs']);
    final flaky =
        _stringList(map, ['flaky_test_keywords', 'flakyTestKeywords']);
    final blocked = _stringList(
        map, ['cicd_blocked_labels', 'cicdBlockedLabels', 'cicd_action_labels']);
    final waiting = _stringList(map, ['waiting_labels', 'waitingLabels']);
    final cla = _stringList(map, ['cla_missing_labels', 'claMissingLabels']);
    final limit = _int(map, ['query_limit', 'queryLimit'], 50);

    return TriageConfig(
      accounts: accounts,
      teamMembers: _stringList(map, ['team_members', 'teamMembers']),
      teamOrgs: _stringList(map, ['team_orgs', 'teamOrgs']),
      primaryOrgs:
          primaryOrgs.isEmpty ? const ['flutter', 'dart-lang'] : primaryOrgs,
      primaryRepos: _stringList(map, ['primary_repos', 'primaryRepos']),
      staleReviewBusinessDays: _int(
          map, ['stale_review_business_days', 'staleReviewBusinessDays'], 3),
      flakyTestKeywords: flaky.isEmpty ? kDefaultFlakyKeywords : flaky,
      cicdBlockedLabels: blocked.isEmpty ? kDefaultCicdBlockedLabels : blocked,
      presubmitTriggers: _extractPresubmitTriggers(map),
      waitingLabels: waiting.isEmpty ? kDefaultWaitingLabels : waiting,
      claMissingLabels: cla.isEmpty ? kDefaultClaMissingLabels : cla,
      holidays: _extractHolidays(map),
      prOverrides:
          _extractOverrides(map['pr_overrides'] ?? map['prOverrides']),
      queryLimit: limit.clamp(1, kMaxSearchPageSize),
      searchAssignee:
          _bool(map, ['search_assignee', 'searchAssignee'], false),
    );
  }

  final List<String> accounts;

  /// Explicit teammate logins. Combined with logins resolved from [teamOrgs].
  final List<String> teamMembers;

  /// GitHub organisations whose members count as teammates. Membership is
  /// resolved at runtime and cached; see `TeamResolver`.
  final List<String> teamOrgs;
  final List<String> primaryOrgs;
  final List<String> primaryRepos;
  final int staleReviewBusinessDays;
  final List<String> flakyTestKeywords;
  final List<String> cicdBlockedLabels;

  /// Repository (or owner) to presubmit trigger label.
  final Map<String, String> presubmitTriggers;
  final List<String> waitingLabels;

  /// Labels indicating the contributor has not signed the CLA, which blocks
  /// all downstream progress until the author acts.
  final List<String> claMissingLabels;

  /// Dates excluded from business-day arithmetic alongside weekends.
  final List<DateTime> holidays;
  final Map<String, PrOverride> prOverrides;
  final int queryLimit;

  /// Whether to also search `assignee:` in addition to `review-requested:`.
  /// Off by default because it doubles query count for typically zero results.
  final bool searchAssignee;

  /// Returns a copy with [logins] merged into [teamMembers].
  TriageConfig withTeamMembers(Iterable<String> logins) {
    final merged = {
      ...teamMembers.map((m) => m.toLowerCase()),
      ...logins.map((m) => m.toLowerCase()),
    }.toList()
      ..sort();
    return TriageConfig(
      accounts: accounts,
      teamMembers: merged,
      teamOrgs: teamOrgs,
      primaryOrgs: primaryOrgs,
      primaryRepos: primaryRepos,
      staleReviewBusinessDays: staleReviewBusinessDays,
      flakyTestKeywords: flakyTestKeywords,
      cicdBlockedLabels: cicdBlockedLabels,
      presubmitTriggers: presubmitTriggers,
      waitingLabels: waitingLabels,
      claMissingLabels: claMissingLabels,
      holidays: holidays,
      prOverrides: prOverrides,
      queryLimit: queryLimit,
      searchAssignee: searchAssignee,
    );
  }

  /// Returns custom override if configured for [repo] and [number].
  PrOverride? getOverrideFor(String repo, int number) {
    final fullKey = '$repo#$number'.toLowerCase();
    final shortKey = '#$number';
    final numKey = '$number';

    for (final entry in prOverrides.entries) {
      final k = entry.key.toLowerCase();
      if (k == fullKey || k == shortKey || k == numKey) {
        return entry.value;
      }
    }
    return null;
  }

  /// Returns true if [username] is one of the configured user accounts.
  bool isMyAccount(String username) {
    return accounts.any((a) => a.toLowerCase() == username.toLowerCase());
  }

  /// Returns true if [username] is a known teammate.
  ///
  /// Deliberately does *not* treat the configured accounts as teammates:
  /// authored PRs are filtered out of the review queue upstream, so the only
  /// effect would be to mislabel the cross-account case.
  bool isTeamMember(String username) {
    final lower = username.toLowerCase();
    return teamMembers.any((t) => t.toLowerCase() == lower);
  }

  /// Returns the presubmit trigger label for [repo], or null when the
  /// repository does not gate CI behind a label.
  String? presubmitTriggerLabelFor(String repo) {
    final lower = repo.toLowerCase();
    for (final entry in presubmitTriggers.entries) {
      if (entry.key.toLowerCase() == lower) return entry.value;
    }
    final owner = _owner(lower);
    if (owner == null) return null;
    for (final entry in presubmitTriggers.entries) {
      if (entry.key.toLowerCase() == owner) return entry.value;
    }
    return null;
  }

  /// True when [repo] is owned by one of the configured accounts, i.e. it is
  /// a personal fork or scratch repository.
  bool isFork(String repo) {
    final owner = _owner(repo.toLowerCase());
    return owner != null && isMyAccount(owner);
  }

  /// True when [repo] is an explicitly configured primary repository or lives
  /// under a configured primary organisation.
  ///
  /// Repositories that are neither forks nor primary are *not* forks; callers
  /// must distinguish the two rather than lumping them together.
  bool isPrimaryRepo(String repo) {
    final lower = repo.toLowerCase();
    if (primaryRepos.any((r) => r.toLowerCase() == lower)) return true;
    final owner = _owner(lower);
    if (owner == null) return false;
    if (isMyAccount(owner)) return false;
    return primaryOrgs.any((org) => org.toLowerCase() == owner);
  }

  /// Returns the owner segment of `owner/name`, or null when [repo] is not a
  /// fully qualified repository reference.
  static String? _owner(String repo) {
    final idx = repo.indexOf('/');
    if (idx <= 0) return null;
    return repo.substring(0, idx);
  }

  static List<String> _stringList(YamlMap map, List<String> keys) {
    final value = _firstNonNull(map, keys);
    return _toStringList(value, keys.first);
  }

  static dynamic _firstNonNull(YamlMap map, List<String> keys) {
    for (final key in keys) {
      final value = map[key];
      if (value != null) return value;
    }
    return null;
  }

  /// Converts a YAML value to a list of strings, refusing to guess.
  ///
  /// An unquoted `- cla: no` parses as a *map*, not a string. Stringifying it
  /// silently produced `"{cla: no}"`, which matched no real label. Failing
  /// loudly is the only safe behaviour.
  static List<String> _toStringList(dynamic value, String key) {
    if (value == null) return const [];
    final items = value is List ? value : [value];
    return items
        .map((e) {
          if (e is! String && e is! num && e is! bool) {
            throw FormatException(
              '$key: expected a list of strings but found ${e.runtimeType} '
              '($e). Values containing ":" must be quoted, e.g. "cla: no".',
            );
          }
          return e.toString().trim();
        })
        .where((s) => s.isNotEmpty)
        .toList();
  }

  static int _int(YamlMap map, List<String> keys, int fallback) {
    final value = _firstNonNull(map, keys);
    if (value == null) return fallback;
    if (value is int) return value;
    throw FormatException(
        '${keys.first}: expected an integer, got "$value" (${value.runtimeType}).');
  }

  static bool _bool(YamlMap map, List<String> keys, bool fallback) {
    final value = _firstNonNull(map, keys);
    if (value == null) return fallback;
    if (value is bool) return value;
    throw FormatException(
        '${keys.first}: expected true or false, got "$value".');
  }

  static Map<String, String> _extractPresubmitTriggers(YamlMap map) {
    final scalar = map['presubmit_trigger_label'] ??
        map['presubmitTriggerLabel'];
    final mapping = map['presubmit_triggers'] ?? map['presubmitTriggers'];

    if (mapping is Map) {
      final result = <String, String>{};
      for (final entry in mapping.entries) {
        final key = entry.key.toString().trim();
        final label = entry.value?.toString().trim() ?? '';
        if (key.isNotEmpty && label.isNotEmpty) result[key] = label;
      }
      return result;
    }

    // Legacy scalar form: apply the label to the repos known to use it.
    if (scalar is String && scalar.trim().isNotEmpty) {
      return {
        for (final repo in kDefaultPresubmitTriggers.keys) repo: scalar.trim(),
      };
    }
    if (scalar != null) return const {};
    return kDefaultPresubmitTriggers;
  }

  static List<DateTime> _extractHolidays(YamlMap map) {
    final raw = _stringList(map, ['holidays']);
    final result = <DateTime>[];
    for (final entry in raw) {
      final parsed = DateTime.tryParse(entry);
      if (parsed == null) {
        throw FormatException(
            'holidays: "$entry" is not a valid YYYY-MM-DD date.');
      }
      result.add(DateTime(parsed.year, parsed.month, parsed.day));
    }
    return result;
  }

  static Map<String, PrOverride> _extractOverrides(dynamic map) {
    if (map is! Map) return const {};
    final result = <String, PrOverride>{};
    for (final entry in map.entries) {
      final key = entry.key.toString().trim();
      if (key.isNotEmpty) {
        result[key] = PrOverride.fromYaml(entry.value);
      }
    }
    return result;
  }

  Map<String, dynamic> toJson() => {
        'accounts': accounts,
        'team_members': teamMembers,
        'team_orgs': teamOrgs,
        'primary_orgs': primaryOrgs,
        'primary_repos': primaryRepos,
        'stale_review_business_days': staleReviewBusinessDays,
        'flaky_test_keywords': flakyTestKeywords,
        'cicd_blocked_labels': cicdBlockedLabels,
        'presubmit_triggers': presubmitTriggers,
        'waiting_labels': waitingLabels,
        'cla_missing_labels': claMissingLabels,
        'holidays': holidays.map((d) => d.toIso8601String()).toList(),
        'pr_overrides': prOverrides.map((k, v) => MapEntry(k, v.toJson())),
        'query_limit': queryLimit,
        'search_assignee': searchAssignee,
      };
}

/// Outcome of loading configuration, including any non-fatal warnings that
/// the caller should surface to the user.
class ConfigLoadResult {
  const ConfigLoadResult({required this.config, this.warnings = const []});

  final TriageConfig config;
  final List<String> warnings;
}

/// Helper that loads [TriageConfig] from disk or queries the default user.
class ConfigLoader {
  const ConfigLoader({required this.fs});

  final FileSystem fs;

  /// Loads config from [configPath], falling back to [getCurrentUser].
  Future<TriageConfig> loadConfig({
    String? configPath,
    Future<String?> Function()? getCurrentUser,
  }) async {
    final result =
        await load(configPath: configPath, getCurrentUser: getCurrentUser);
    return result.config;
  }

  /// Loads config and reports why any fallback was taken.
  ///
  /// A config file that exists but yields no accounts is a likely typo
  /// (`account:` for `accounts:`), so it produces a warning rather than
  /// silently degrading to a single-account run.
  Future<ConfigLoadResult> load({
    String? configPath,
    Future<String?> Function()? getCurrentUser,
  }) async {
    final warnings = <String>[];

    if (configPath != null && fs.file(configPath).existsSync()) {
      final content = fs.file(configPath).readAsStringSync();
      final config = TriageConfig.fromYamlString(content);
      if (config.accounts.isNotEmpty) {
        return ConfigLoadResult(config: config, warnings: warnings);
      }
      warnings.add(
        'Config at $configPath declares no "accounts:"; falling back to the '
        'authenticated gh user. Check for a typo in the accounts key.',
      );
    } else if (configPath != null) {
      warnings.add(
        'No config found at $configPath. Copy config.example.yaml to '
        'config.yaml to configure accounts, team orgs, and overrides.',
      );
    }

    if (getCurrentUser != null) {
      final user = await getCurrentUser();
      if (user != null && user.trim().isNotEmpty) {
        return ConfigLoadResult(
          config: TriageConfig.defaultForUser(user.trim()),
          warnings: warnings,
        );
      }
    }

    return ConfigLoadResult(
      config: const TriageConfig(accounts: []),
      warnings: warnings,
    );
  }
}
