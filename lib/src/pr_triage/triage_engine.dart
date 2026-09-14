import 'package:file/file.dart';
import 'package:file/local.dart';
import 'package:path/path.dart' as p;

import 'github_fetcher.dart';
import 'models/pr_item.dart';
import 'models/triage_config.dart';
import 'models/triaged_item.dart';
import 'pr_classifier.dart';
import 'team_resolver.dart';

/// Everything a triage run produces, including the diagnostics needed to
/// judge whether the run was complete.
class TriageResult {
  const TriageResult({
    required this.config,
    required this.myWork,
    required this.reviewQueue,
    required this.topMyWork,
    required this.topReviewQueue,
    this.warnings = const [],
    this.truncated = false,
  });

  final TriageConfig config;
  final List<TriagedItem> myWork;
  final List<TriagedItem> reviewQueue;
  final List<TriagedItem> topMyWork;
  final List<TriagedItem> topReviewQueue;

  /// Non-fatal problems, e.g. a missing config file or an org membership
  /// lookup that the token was not scoped for.
  final List<String> warnings;

  /// True when GitHub reported more matching pull requests than `query_limit`
  /// allowed us to read, so the queues below are incomplete.
  final bool truncated;

  /// Full detail for the two queues, plus compact references for the
  /// highlights so the top-of-report payload stays readable.
  Map<String, dynamic> toJson() => {
        'config': config.toJson(),
        'truncated': truncated,
        'warnings': warnings,
        'top_my_work': topMyWork.map((i) => i.toRefJson()).toList(),
        'top_review_queue': topReviewQueue.map((i) => i.toRefJson()).toList(),
        'my_work': myWork.map((i) => i.toJson()).toList(),
        'review_queue': reviewQueue.map((i) => i.toJson()).toList(),
      };
}

/// Orchestrates config loading, fetching, and classification.
class TriageEngine {
  TriageEngine({
    FileSystem? fs,
    GitHubPrFetcher? fetcher,
    PrClassifier? classifier,
    TeamResolver? teamResolver,
  })  : fs = fs ?? const LocalFileSystem(),
        fetcher = fetcher ?? GitHubPrFetcher(),
        classifier = classifier ?? const PrClassifier(),
        teamResolver = teamResolver ?? TeamResolver(fs: fs ?? const LocalFileSystem());

  final FileSystem fs;
  final GitHubPrFetcher fetcher;
  final PrClassifier classifier;
  final TeamResolver teamResolver;

  /// Runs a full triage pass.
  ///
  /// [topCount] controls how many highlights each queue reports. Set
  /// [refreshMergeable] to false in tests to skip the second network round
  /// trip that resolves GitHub's lazily computed `mergeable` field.
  Future<TriageResult> runTriage({
    String? configPath,
    int topCount = 3,
    int? limitOverride,
    bool refreshMergeable = true,
  }) async {
    final loaded = await ConfigLoader(fs: fs).load(
      configPath: configPath,
      getCurrentUser: fetcher.getCurrentUser,
    );
    final warnings = <String>[...loaded.warnings];
    var config = loaded.config;

    if (config.accounts.isEmpty) {
      throw StateError(
        'No GitHub accounts configured and `gh api user` did not return one. '
        'Copy config.example.yaml to config.yaml and set "accounts:", or run '
        '`gh auth login`.',
      );
    }

    config = await _withResolvedTeam(config, configPath, warnings);

    final limit = limitOverride ?? config.queryLimit;
    final (authored, review) = await (
      fetcher.fetchAuthored(authors: config.accounts, limit: limit),
      fetcher.fetchReviewQueue(
        reviewers: config.accounts,
        limit: limit,
        includeAssignee: config.searchAssignee,
      ),
    ).wait;

    // Cross-account collaboration means an authored PR can also come back in
    // the review queue; keep it in exactly one place.
    final reviewItems =
        review.items.where((pr) => !config.isMyAccount(pr.author)).toList();

    final (myPrs, reviewPrs) = await _resolveMergeable(
      authored.items,
      reviewItems,
      enabled: refreshMergeable,
    );

    final (myWork, reviewQueue) = classifier.classifyAll(
      myPrs: myPrs,
      reviewPrs: reviewPrs,
      config: config,
    );

    return TriageResult(
      config: config,
      myWork: myWork,
      reviewQueue: reviewQueue,
      topMyWork: myWork.take(topCount).toList(),
      topReviewQueue: reviewQueue.take(topCount).toList(),
      warnings: warnings,
      truncated: authored.truncated || review.truncated,
    );
  }

  /// Expands `team_orgs` into real member logins, recording a warning when the
  /// lookup yields nothing so the static allowlist fallback is visible.
  Future<TriageConfig> _withResolvedTeam(
    TriageConfig config,
    String? configPath,
    List<String> warnings,
  ) async {
    if (config.teamOrgs.isEmpty) return config;

    final members = await teamResolver.resolveMembers(
      config.teamOrgs,
      cachePath: _cachePathFor(configPath),
    );
    if (members.isEmpty) {
      warnings.add(
        'Could not resolve members of ${config.teamOrgs.join(', ')}. The gh '
        'token likely lacks the read:org scope; falling back to the '
        '"team_members" list (${config.teamMembers.length} entries).',
      );
      return config;
    }
    return config.withTeamMembers(members);
  }

  Future<(List<PrItem>, List<PrItem>)> _resolveMergeable(
    List<PrItem> authored,
    List<PrItem> review, {
    required bool enabled,
  }) async {
    if (!enabled) return (authored, review);
    // Only authored PRs are gated on mergeability by the classifier.
    return (await fetcher.refreshUnknownMergeable(authored), review);
  }

  /// Stores the org membership cache next to the config so a per-user config
  /// directory keeps its own cache.
  String? _cachePathFor(String? configPath) {
    if (configPath == null) return null;
    return p.join(p.dirname(configPath), '.team_members_cache.json');
  }
}
