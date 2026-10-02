import 'dart:io';

import 'package:dotfiles/src/pr_triage/github_fetcher.dart';
import 'package:dotfiles/src/pr_triage/models/pr_item.dart';
import 'package:dotfiles/src/pr_triage/models/triage_config.dart';
import 'package:dotfiles/src/pr_triage/models/triaged_item.dart';
import 'package:dotfiles/src/pr_triage/pr_classifier.dart';
import 'package:dotfiles/src/pr_triage/team_resolver.dart';
import 'package:dotfiles/src/pr_triage/triage_engine.dart';
import 'package:file/file.dart';
import 'package:file/memory.dart';
import 'package:test/test.dart';

/// Fixed "now" so business-day assertions do not drift.
final DateTime kNow = DateTime(2026, 1, 15, 10);

const TriageConfig kConfig = TriageConfig(
  accounts: ['reidbaker', 'reidbaker-agent'],
  teamMembers: ['gmackall', 'camsim99'],
);

PrClassifier classifier() => PrClassifier(clock: () => kNow);

PrItem pr({
  int number = 1,
  String repo = 'flutter/flutter',
  String author = 'reidbaker',
  bool isDraft = false,
  String mergeable = 'MERGEABLE',
  String reviewDecision = 'NONE',
  CiStatus ciStatus = CiStatus.none,
  int totalCheckCount = 0,
  List<String> failingChecks = const [],
  int unresolvedReviewThreads = 0,
  bool unresolvedThreadsExact = true,
  List<String> labels = const [],
  DateTime? createdAt,
  DateTime? headCommitDate,
  DateTime? lastReviewRequestedAt,
  List<PrReview> latestReviews = const [],
  List<PrReview> allReviews = const [],
  List<String> requestedReviewers = const [],
  List<String> requestedTeams = const [],
  List<String> assignedReviewers = const [],
}) {
  final created = createdAt ?? kNow.subtract(const Duration(days: 1));
  return PrItem(
    number: number,
    title: 'PR $number',
    url: 'https://github.com/$repo/pull/$number',
    repo: repo,
    author: author,
    isDraft: isDraft,
    mergeable: mergeable,
    reviewDecision: reviewDecision,
    ciStatus: ciStatus,
    totalCheckCount: totalCheckCount,
    failingChecks: failingChecks,
    unresolvedReviewThreads: unresolvedReviewThreads,
    unresolvedThreadsExact: unresolvedThreadsExact,
    labels: labels,
    updatedAt: created,
    createdAt: created,
    headCommitDate: headCommitDate,
    lastReviewRequestedAt: lastReviewRequestedAt,
    latestReviews: latestReviews,
    allReviews: allReviews,
    requestedReviewers: requestedReviewers,
    requestedTeams: requestedTeams,
    assignedReviewers: assignedReviewers,
  );
}

PrReview review(String author, String state, DateTime at) =>
    PrReview(author: author, state: state, submittedAt: at);

void main() {
  group('TriageConfig parsing', () {
    test('reads the documented schema', () {
      final config = TriageConfig.fromYamlString('''
accounts:
  - reidbaker
  - reidbaker-agent
primary_orgs:
  - flutter
team_orgs:
  - flutter
presubmit_triggers:
  flutter/flutter: CICD
  flutter/packages: CICD
cla_missing_labels:
  - "cla: no"
search_assignee: true
''');

      expect(config.accounts, ['reidbaker', 'reidbaker-agent']);
      expect(config.teamOrgs, ['flutter']);
      expect(config.presubmitTriggers['flutter/packages'], 'CICD');
      expect(config.claMissingLabels, ['cla: no']);
      expect(config.searchAssignee, isTrue);
    });

    test('rejects an unquoted "cla: no" instead of stringifying the map', () {
      // Unquoted, YAML parses this as {cla: null}. Stringifying produced
      // "{cla: null}", which silently matched no label at all.
      expect(
        () => TriageConfig.fromYamlString('''
accounts: [me]
cla_missing_labels:
  - cla: no
'''),
        throwsA(isA<FormatException>()),
      );
    });

    test('rejects a non-integer query_limit', () {
      expect(
        () => TriageConfig.fromYamlString('accounts: [me]\nquery_limit: lots'),
        throwsA(isA<FormatException>()),
      );
    });

    test('clamps query_limit to GitHub\'s search page cap', () {
      final config = TriageConfig.fromYamlString(
        'accounts: [me]\nquery_limit: 500',
      );
      expect(config.queryLimit, kMaxSearchPageSize);
    });

    test('parses holidays and rejects malformed dates', () {
      final config = TriageConfig.fromYamlString(
        'accounts: [me]\nholidays: ["2026-01-07"]',
      );
      expect(config.holidays.single, DateTime(2026, 1, 7));

      expect(
        () => TriageConfig.fromYamlString(
          'accounts: [me]\nholidays: ["next tuesday"]',
        ),
        throwsA(isA<FormatException>()),
      );
    });

    test('CLA labels are no longer duplicated into waiting labels', () {
      expect(kDefaultWaitingLabels, isNot(contains('cla: no')));
      expect(kDefaultClaMissingLabels, contains('cla: no'));
    });

    test('reads the review-queue thresholds, with defaults', () {
      final defaults = TriageConfig.fromYamlString('accounts: [me]');
      expect(defaults.crowdedReviewThreshold, 4);
      expect(defaults.staleCoReviewerBusinessDays, 10);

      final custom = TriageConfig.fromYamlString(
        'accounts: [me]\n'
        'crowded_review_threshold: 6\n'
        'stale_co_reviewer_business_days: 7',
      );
      expect(custom.crowdedReviewThreshold, 6);
      expect(custom.staleCoReviewerBusinessDays, 7);
      expect(custom.withTeamMembers(['x']).staleCoReviewerBusinessDays, 7);
    });
  });

  group('TriageConfig repository scoping', () {
    test('distinguishes a personal fork from a merely untracked repo', () {
      expect(kConfig.isFork('reidbaker/dotfiles'), isTrue);
      expect(kConfig.isFork('google/some-repo'), isFalse);
      expect(kConfig.isPrimaryRepo('google/some-repo'), isFalse);
      expect(kConfig.isPrimaryRepo('flutter/flutter'), isTrue);
    });

    test('scopes the presubmit trigger label per repository', () {
      expect(kConfig.presubmitTriggerLabelFor('flutter/flutter'), 'CICD');
      // dart-lang has no such label; asking for it returns a 404.
      expect(kConfig.presubmitTriggerLabelFor('dart-lang/sdk'), isNull);
    });

    test('withTeamMembers merges and lowercases', () {
      final merged = kConfig.withTeamMembers({'Jesswrd', 'gmackall'});
      expect(merged.teamMembers, containsAll(['gmackall', 'jesswrd']));
      expect(merged.isTeamMember('JESSWRD'), isTrue);
    });
  });

  group('PrOverride', () {
    test('a bare string annotates without changing placement', () {
      final override = PrOverride.fromYaml('just a note');
      expect(override.note, 'just a note');
      expect(override.demote, isFalse);
      expect(override.changesPlacement, isFalse);
    });

    test('rejects a non-boolean demote', () {
      expect(
        () => PrOverride.fromYaml({'demote': 'yes please'}),
        throwsA(isA<FormatException>()),
      );
    });
  });

  group('ConfigLoader', () {
    test('warns when the config file is absent', () async {
      final fs = MemoryFileSystem();
      final result = await ConfigLoader(fs: fs).load(
        configPath: '/nowhere/config.yaml',
        getCurrentUser: () async => 'reidbaker',
      );
      expect(result.config.accounts, ['reidbaker']);
      expect(result.warnings.single, contains('config.example.yaml'));
    });

    test('warns when the file exists but declares no accounts', () async {
      final fs = MemoryFileSystem();
      fs.file('/c.yaml')
        ..createSync(recursive: true)
        ..writeAsStringSync('account: reidbaker\n');

      final result = await ConfigLoader(fs: fs).load(
        configPath: '/c.yaml',
        getCurrentUser: () async => 'fallback-user',
      );
      expect(result.config.accounts, ['fallback-user']);
      expect(result.warnings.single, contains('accounts'));
    });
  });

  group('Business day arithmetic', () {
    test('counts weekdays between two dates', () {
      // Mon 2026-01-05 to Fri 2026-01-09.
      expect(
        classifier().calculateBusinessDays(
          DateTime(2026, 1, 5),
          DateTime(2026, 1, 9),
        ),
        4,
      );
    });

    test('skips weekends', () {
      // Fri to Mon is one business day, not three.
      expect(
        classifier().calculateBusinessDays(
          DateTime(2026, 1, 9),
          DateTime(2026, 1, 12),
        ),
        1,
      );
    });

    test('skips configured holidays', () {
      expect(
        classifier().calculateBusinessDays(
          DateTime(2026, 1, 5),
          DateTime(2026, 1, 9),
          holidays: [DateTime(2026, 1, 7)],
        ),
        3,
      );
    });

    test('is non-zero for an aged PR', () {
      // Regression: every item once reported business_days_elapsed: 0 because
      // the elapsed count was computed but never passed to the item.
      final item = classifier().classifyMyWork(
        pr(createdAt: DateTime(2025, 12, 1)),
        kConfig,
      );
      expect(item.businessDaysElapsed, greaterThan(20));
    });

    test('measures from the last review request, not creation', () {
      final item = classifier().classifyMyWork(
        pr(
          createdAt: DateTime(2025, 1, 1),
          lastReviewRequestedAt: kNow.subtract(const Duration(days: 1)),
          ciStatus: CiStatus.passing,
          totalCheckCount: 3,
        ),
        kConfig,
      );
      expect(item.businessDaysElapsed, lessThan(3));
    });
  });

  group('My Work: ready to merge', () {
    test('fires when approved with green CI', () {
      final item = classifier().classifyMyWork(
        pr(
          reviewDecision: 'APPROVED',
          ciStatus: CiStatus.passing,
          totalCheckCount: 12,
        ),
        kConfig,
      );
      expect(item.tier, MyWorkTier.readyToMerge);
      expect(item.actionPrompt, '[Action: Merge]');
    });

    test('still fires when mergeable is UNKNOWN', () {
      // GitHub computes mergeability lazily; the first read is UNKNOWN for
      // roughly a third of a run. Treating that as "not mergeable" hid every
      // one of those PRs from the merge tier.
      final item = classifier().classifyMyWork(
        pr(
          reviewDecision: 'APPROVED',
          mergeable: 'UNKNOWN',
          ciStatus: CiStatus.passing,
          totalCheckCount: 12,
        ),
        kConfig,
      );
      expect(item.tier, MyWorkTier.readyToMerge);
    });

    test('does not fire when the branch genuinely conflicts', () {
      final item = classifier().classifyMyWork(
        pr(
          reviewDecision: 'APPROVED',
          mergeable: 'CONFLICTING',
          ciStatus: CiStatus.passing,
          totalCheckCount: 12,
        ),
        kConfig,
      );
      expect(item.tier, isNot(MyWorkTier.readyToMerge));
    });

    test('does not fire when the thread count is only a lower bound', () {
      final item = classifier().classifyMyWork(
        pr(
          reviewDecision: 'APPROVED',
          ciStatus: CiStatus.passing,
          totalCheckCount: 12,
          unresolvedThreadsExact: false,
        ),
        kConfig,
      );
      expect(item.tier, isNot(MyWorkTier.readyToMerge));
    });

    test('does not fire on an unreadable CI rollup', () {
      // ciStatus none with checks present means we failed to read the rollup,
      // which is not the same as "this repo runs no CI".
      final item = classifier().classifyMyWork(
        pr(
          reviewDecision: 'APPROVED',
          ciStatus: CiStatus.none,
          totalCheckCount: 7,
        ),
        kConfig,
      );
      expect(item.tier, isNot(MyWorkTier.readyToMerge));
    });
  });

  group('My Work: CI/CD trigger', () {
    test('asks for the trigger label when no checks have run', () {
      final item = classifier().classifyMyWork(
        pr(repo: 'flutter/packages'),
        kConfig,
      );
      expect(item.tier, MyWorkTier.waitingOnCicdTask);
      expect(item.actionPrompt, contains('CICD'));
    });

    test('does not fire once the trigger label is applied', () {
      // flutter/packages#12663 carried CICD and was still reported as needing
      // it, because the rule matched the label instead of its absence.
      final item = classifier().classifyMyWork(
        pr(repo: 'flutter/packages', labels: ['CICD']),
        kConfig,
      );
      expect(item.tier, isNot(MyWorkTier.waitingOnCicdTask));
    });

    test('never asks a dart-lang PR for a label that repo does not define', () {
      final item = classifier().classifyMyWork(
        pr(repo: 'dart-lang/sdk'),
        kConfig,
      );
      expect(item.tier, isNot(MyWorkTier.waitingOnCicdTask));
    });

    test('reports a tree-status block as unactionable waiting', () {
      final item = classifier().classifyMyWork(
        pr(labels: ['waiting for tree to go green']),
        kConfig,
      );
      expect(item.tier, MyWorkTier.waitingOnCicdTask);
      expect(item.actionPrompt, contains('tree to go green'));
    });
  });

  group('My Work: drafts', () {
    test('a clean draft is promoted to draft-ready-for-review', () {
      final item = classifier().classifyMyWork(
        pr(isDraft: true, ciStatus: CiStatus.passing, totalCheckCount: 8),
        kConfig,
      );
      expect(item.tier, MyWorkTier.draftReadyForReview);
    });

    test('a draft with running CI stays a draft and says why', () {
      final item = classifier().classifyMyWork(
        pr(isDraft: true, ciStatus: CiStatus.pending, totalCheckCount: 8),
        kConfig,
      );
      expect(item.tier, MyWorkTier.draft);
      expect(item.reason, contains('CI still running'));
    });

    test('a draft with open threads names them', () {
      final item = classifier().classifyMyWork(
        pr(
          isDraft: true,
          ciStatus: CiStatus.passing,
          totalCheckCount: 8,
          unresolvedReviewThreads: 20,
        ),
        kConfig,
      );
      expect(item.tier, MyWorkTier.draft);
      expect(item.reason, contains('20 unresolved review threads'));
    });

    test('a draft is never told to trigger CI', () {
      // Drafts have no checks by design; the CI/CD tier used to claim every
      // draft needed a trigger label.
      final item = classifier().classifyMyWork(
        pr(repo: 'flutter/packages', isDraft: true),
        kConfig,
      );
      expect(item.tier, isNot(MyWorkTier.waitingOnCicdTask));
      expect(item.tier, MyWorkTier.draft);
    });
  });

  group('My Work: CI failures and feedback', () {
    test('classifies an all-flaky failure separately', () {
      final item = classifier().classifyMyWork(
        pr(
          ciStatus: CiStatus.failing,
          totalCheckCount: 4,
          failingChecks: ['Mac_android flaky_integration'],
        ),
        kConfig,
      );
      expect(item.tier, MyWorkTier.flakyCiFailure);
    });

    test('a mixed failure needs a code fix', () {
      final item = classifier().classifyMyWork(
        pr(
          ciStatus: CiStatus.failing,
          totalCheckCount: 4,
          failingChecks: ['Mac_android flaky', 'analyze'],
        ),
        kConfig,
      );
      expect(item.tier, MyWorkTier.failingCiWorkRelated);
    });

    test('approved with open threads is minor feedback', () {
      final item = classifier().classifyMyWork(
        pr(
          reviewDecision: 'APPROVED',
          ciStatus: CiStatus.passing,
          totalCheckCount: 4,
          unresolvedReviewThreads: 2,
        ),
        kConfig,
      );
      expect(item.tier, MyWorkTier.minorFeedbackWithApproval);
    });

    test('a human blocking review is substantial feedback', () {
      final item = classifier().classifyMyWork(
        pr(
          ciStatus: CiStatus.passing,
          totalCheckCount: 4,
          headCommitDate: kNow.subtract(const Duration(days: 3)),
          latestReviews: [
            review(
              'cbracken',
              'CHANGES_REQUESTED',
              kNow.subtract(const Duration(days: 1)),
            ),
          ],
        ),
        kConfig,
      );
      expect(item.tier, MyWorkTier.substantialFeedback);
      expect(item.reason, contains('cbracken'));
    });

    test('a bot blocking review does not bury the PR', () {
      // gemini-code-assist left CHANGES_REQUESTED on 9 of 10 live items,
      // which buried the entire queue behind an automated opinion.
      final item = classifier().classifyMyWork(
        pr(
          ciStatus: CiStatus.passing,
          totalCheckCount: 4,
          headCommitDate: kNow.subtract(const Duration(days: 3)),
          latestReviews: [
            review(
              'gemini-code-assist',
              'CHANGES_REQUESTED',
              kNow.subtract(const Duration(days: 1)),
            ),
          ],
        ),
        kConfig,
      );
      expect(item.tier, isNot(MyWorkTier.substantialFeedback));
    });

    test('a review predating the head commit is already addressed', () {
      final item = classifier().classifyMyWork(
        pr(
          ciStatus: CiStatus.passing,
          totalCheckCount: 4,
          headCommitDate: kNow.subtract(const Duration(days: 1)),
          latestReviews: [
            review(
              'cbracken',
              'CHANGES_REQUESTED',
              kNow.subtract(const Duration(days: 5)),
            ),
          ],
        ),
        kConfig,
      );
      expect(item.tier, isNot(MyWorkTier.substantialFeedback));
    });
  });

  group('My Work: review age', () {
    test('past the threshold is stalled', () {
      final item = classifier().classifyMyWork(
        pr(
          ciStatus: CiStatus.passing,
          totalCheckCount: 4,
          createdAt: DateTime(2025, 12, 1),
        ),
        kConfig,
      );
      expect(item.tier, MyWorkTier.stalledInReview);
    });

    test('inside the threshold is fresh', () {
      final item = classifier().classifyMyWork(
        pr(
          ciStatus: CiStatus.passing,
          totalCheckCount: 4,
          createdAt: kNow.subtract(const Duration(days: 1)),
        ),
        kConfig,
      );
      expect(item.tier, MyWorkTier.freshInReview);
    });
  });

  group('My Work: repository scoping', () {
    test('an untracked org is not called a personal fork', () {
      final item = classifier().classifyMyWork(pr(repo: 'google/x'), kConfig);
      expect(item.tier, MyWorkTier.nonPrimaryRepo);
    });

    test('a repo owned by one of my accounts is a fork', () {
      final item = classifier().classifyMyWork(
        pr(repo: 'reidbaker/dotfiles'),
        kConfig,
      );
      expect(item.tier, MyWorkTier.forkOrPoc);
    });
  });

  group('My Work: overrides', () {
    const demoted = TriageConfig(
      accounts: ['reidbaker'],
      prOverrides: {
        'flutter/flutter#189918': PrOverride(
          demote: true,
          action: '[Action: Delete when DSL migration bug is closed]',
          note: 'Exploratory POC.',
        ),
      },
    );

    test('demote buries the PR and keeps the custom action', () {
      final item = classifier().classifyMyWork(pr(number: 189918), demoted);
      expect(item.tier, MyWorkTier.forkOrPoc);
      expect(item.actionPrompt, contains('DSL migration'));
      expect(item.reason, contains('Exploratory POC.'));
    });

    test('a note-only override annotates without demoting', () {
      const annotated = TriageConfig(
        accounts: ['reidbaker'],
        prOverrides: {
          'flutter/flutter#7': PrOverride(note: 'blocked on infra'),
        },
      );
      final item = classifier().classifyMyWork(
        pr(
          number: 7,
          reviewDecision: 'APPROVED',
          ciStatus: CiStatus.passing,
          totalCheckCount: 4,
        ),
        annotated,
      );
      expect(item.tier, MyWorkTier.readyToMerge);
      expect(item.reason, contains('blocked on infra'));
    });

    test('an unknown pinned tier fails loudly', () {
      const bad = TriageConfig(
        accounts: ['reidbaker'],
        prOverrides: {'flutter/flutter#9': PrOverride(tier: 'urgentish')},
      );
      expect(
        () => classifier().classifyMyWork(pr(number: 9), bad),
        throwsA(isA<FormatException>()),
      );
    });
  });

  group('Review Queue', () {
    test('a push after my review is re-review ready', () {
      final item = classifier().classifyReviewQueue(
        pr(
          author: 'gmackall',
          allReviews: [
            review(
              'reidbaker',
              'APPROVED',
              kNow.subtract(const Duration(days: 5)),
            ),
          ],
          headCommitDate: kNow.subtract(const Duration(days: 1)),
        ),
        kConfig,
      );
      expect(item.tier, ReviewQueueTier.reReviewReady);
    });

    test('a re-request finds my review even when GitHub hides it', () {
      // GitHub drops a reviewer from latestReviews once a re-review is
      // requested from them, which is exactly this state.
      final item = classifier().classifyReviewQueue(
        pr(
          author: 'gmackall',
          allReviews: [
            review(
              'reidbaker',
              'CHANGES_REQUESTED',
              kNow.subtract(const Duration(days: 5)),
            ),
          ],
          requestedReviewers: ['reidbaker'],
        ),
        kConfig,
      );
      expect(item.tier, ReviewQueueTier.reReviewReady);
    });

    test('a teammate PR outranks an external one', () {
      final team = classifier().classifyReviewQueue(
        pr(author: 'gmackall', ciStatus: CiStatus.passing, totalCheckCount: 3),
        kConfig,
      );
      final external = classifier().classifyReviewQueue(
        pr(author: 'stranger', ciStatus: CiStatus.passing, totalCheckCount: 3),
        kConfig,
      );
      expect(team.tier, ReviewQueueTier.teamReviewRequest);
      expect(external.tier, ReviewQueueTier.cleanExternalPr);
      expect(team.tierRank, lessThan(external.tierRank));
    });

    test('a waiting label defers to the author', () {
      final item = classifier().classifyReviewQueue(
        pr(author: 'stranger', labels: ['waiting for response']),
        kConfig,
      );
      expect(item.tier, ReviewQueueTier.waitingOnAuthor);
    });

    test('an incoming draft is deprioritized', () {
      final item = classifier().classifyReviewQueue(
        pr(author: 'stranger', isDraft: true),
        kConfig,
      );
      expect(item.tier, ReviewQueueTier.draftReview);
    });

    test('an unsigned CLA blocks, and says so honestly', () {
      final item = classifier().classifyReviewQueue(
        pr(author: 'stranger', labels: ['cla: no']),
        kConfig,
      );
      expect(item.tier, ReviewQueueTier.blockedExternalPr);
      expect(item.reason, contains('CLA'));
    });

    test('blocking reviews are reported as such, not as a CLA problem', () {
      final item = classifier().classifyReviewQueue(
        pr(
          author: 'stranger',
          headCommitDate: kNow.subtract(const Duration(days: 3)),
          latestReviews: [
            review(
              'cbracken',
              'CHANGES_REQUESTED',
              kNow.subtract(const Duration(days: 1)),
            ),
          ],
        ),
        kConfig,
      );
      expect(item.tier, ReviewQueueTier.blockedExternalPr);
      expect(item.reason, isNot(contains('CLA')));
      expect(item.reason, contains('cbracken'));
    });

    test('the backlog tier is reachable', () {
      final item = classifier().classifyReviewQueue(
        pr(author: 'stranger', ciStatus: CiStatus.failing, totalCheckCount: 3),
        kConfig,
      );
      expect(item.tier, ReviewQueueTier.other);
    });

    test('untracked and fork repos get their own review tiers', () {
      expect(
        classifier().classifyReviewQueue(pr(repo: 'google/x'), kConfig).tier,
        ReviewQueueTier.nonPrimaryRepoReview,
      );
      expect(
        classifier()
            .classifyReviewQueue(pr(repo: 'reidbaker/dotfiles'), kConfig)
            .tier,
        ReviewQueueTier.forkOrPocReview,
      );
    });
  });

  group('Review Queue: team-only requests', () {
    test('a team-only draft goes to the bottom, not the draft tier', () {
      // The shape of flutter/flutter#178551: teams and other people asked,
      // not you; draft; conflicting.
      final item = classifier().classifyReviewQueue(
        pr(
          author: 'stranger',
          isDraft: true,
          mergeable: 'CONFLICTING',
          requestedTeams: ['ios-reviewers', 'android-reviewers'],
          requestedReviewers: ['justinmc', 'jtmcdole', 'loic-sharma'],
          latestReviews: [
            review(
              'Renzo-Olivares',
              'COMMENTED',
              kNow.subtract(const Duration(days: 30)),
            ),
          ],
        ),
        kConfig,
      );
      expect(item.tier, ReviewQueueTier.teamOnlyRequest);
      expect(item.reason, contains('ios-reviewers, android-reviewers'));
      expect(item.reason, contains('4 people already on it (crowded)'));
      expect(item.reason, contains('merge conflicts'));
      expect(item.reason, contains('draft'));
    });

    test('a request to you and a team is not team-only', () {
      final item = classifier().classifyReviewQueue(
        pr(
          author: 'gmackall',
          ciStatus: CiStatus.passing,
          totalCheckCount: 3,
          requestedTeams: ['android-reviewers'],
          requestedReviewers: ['reidbaker'],
        ),
        kConfig,
      );
      expect(item.tier, ReviewQueueTier.teamReviewRequest);
    });

    test('an assignment is not team-only', () {
      final item = classifier().classifyReviewQueue(
        pr(
          author: 'gmackall',
          ciStatus: CiStatus.passing,
          totalCheckCount: 3,
          requestedTeams: ['android-reviewers'],
          assignedReviewers: ['reidbaker-agent'],
        ),
        kConfig,
      );
      expect(item.tier, ReviewQueueTier.teamReviewRequest);
    });

    test(
      'a push after my review stays re-review ready under a team request',
      () {
        final item = classifier().classifyReviewQueue(
          pr(
            author: 'gmackall',
            requestedTeams: ['android-reviewers'],
            allReviews: [
              review(
                'reidbaker',
                'COMMENTED',
                kNow.subtract(const Duration(days: 5)),
              ),
            ],
            headCommitDate: kNow.subtract(const Duration(days: 1)),
          ),
          kConfig,
        );
        expect(item.tier, ReviewQueueTier.reReviewReady);
      },
    );

    test('team-only ranks below the backlog tier', () {
      expect(
        ReviewQueueTier.teamOnlyRequest.rank,
        greaterThan(ReviewQueueTier.other.rank),
      );
    });

    test('crowded team-only requests sort after uncrowded ones', () {
      final crowded = pr(
        number: 1,
        author: 'stranger',
        requestedTeams: ['android-reviewers'],
        requestedReviewers: ['a1', 'a2', 'a3', 'a4'],
        lastReviewRequestedAt: kNow.subtract(const Duration(days: 40)),
      );
      final small = pr(
        number: 2,
        author: 'stranger',
        requestedTeams: ['android-reviewers'],
        requestedReviewers: ['a1'],
        lastReviewRequestedAt: kNow.subtract(const Duration(days: 2)),
      );
      final (_, queue) = classifier().classifyAll(
        myPrs: const [],
        reviewPrs: [crowded, small],
        config: kConfig,
      );
      expect(queue.map((i) => i.pr.number), [2, 1]);
    });
  });

  group('Review Queue: merge conflicts', () {
    test('a conflicting teammate PR falls to the backlog with the reason', () {
      final item = classifier().classifyReviewQueue(
        pr(
          author: 'gmackall',
          mergeable: 'CONFLICTING',
          ciStatus: CiStatus.passing,
          totalCheckCount: 3,
          requestedReviewers: ['reidbaker'],
        ),
        kConfig,
      );
      expect(item.tier, ReviewQueueTier.other);
      expect(item.reason, contains('Merge conflicts'));
    });

    test('a conflicting draft asked of you falls to the backlog', () {
      final item = classifier().classifyReviewQueue(
        pr(
          author: 'stranger',
          isDraft: true,
          mergeable: 'CONFLICTING',
          requestedReviewers: ['reidbaker'],
        ),
        kConfig,
      );
      expect(item.tier, ReviewQueueTier.other);
      expect(item.reason, contains('merge conflicts'));
    });
  });

  group('Review Queue: co-reviewer stalled', () {
    // kNow is Thursday 2026-01-15; 2026-01-01 is 10 business days earlier
    // and 2026-01-02 is 9.
    PrItem stalledShape(
      DateTime requestedAt, {
      String mergeable = 'MERGEABLE',
    }) => pr(
      author: 'stranger',
      mergeable: mergeable,
      ciStatus: CiStatus.failing,
      totalCheckCount: 3,
      requestedReviewers: ['reidbaker', 'jesswrd'],
      lastReviewRequestedAt: requestedAt,
    );

    test('fires at the threshold and names the co-reviewer', () {
      final item = classifier().classifyReviewQueue(
        stalledShape(DateTime(2026, 1, 1, 10)),
        kConfig,
      );
      expect(item.tier, ReviewQueueTier.coReviewerStalled);
      expect(item.reason, contains('jesswrd'));
      expect(item.businessDaysElapsed, 10);
    });

    test('does not fire one day before the threshold', () {
      final item = classifier().classifyReviewQueue(
        stalledShape(DateTime(2026, 1, 2, 10)),
        kConfig,
      );
      expect(item.tier, ReviewQueueTier.other);
    });

    test('does not fire when the PR has merge conflicts', () {
      final item = classifier().classifyReviewQueue(
        stalledShape(DateTime(2026, 1, 1, 10), mergeable: 'CONFLICTING'),
        kConfig,
      );
      expect(item.tier, ReviewQueueTier.other);
    });

    test('ignores bot reviewers', () {
      final item = classifier().classifyReviewQueue(
        pr(
          author: 'stranger',
          ciStatus: CiStatus.failing,
          totalCheckCount: 3,
          requestedReviewers: [
            'reidbaker',
            'copilot-pull-request-reviewer[bot]',
          ],
          lastReviewRequestedAt: DateTime(2026, 1, 1, 10),
        ),
        kConfig,
      );
      expect(item.tier, ReviewQueueTier.other);
    });

    test('a passing teammate PR stays in the teammate tier', () {
      final item = classifier().classifyReviewQueue(
        pr(
          author: 'gmackall',
          ciStatus: CiStatus.passing,
          totalCheckCount: 3,
          requestedReviewers: ['reidbaker', 'jesswrd'],
          lastReviewRequestedAt: DateTime(2026, 1, 1, 10),
        ),
        kConfig,
      );
      expect(item.tier, ReviewQueueTier.teamReviewRequest);
    });
  });

  group('TriageEngine', () {
    late MemoryFileSystem fs;

    setUp(() {
      fs = MemoryFileSystem();
      fs.file('/config.yaml')
        ..createSync(recursive: true)
        ..writeAsStringSync('accounts:\n  - reidbaker\n');
    });

    Future<TriageResult> run({
      List<PrItem> authored = const [],
      List<PrItem> reviews = const [],
      bool truncated = false,
      int topCount = 3,
      String? configPath = '/config.yaml',
    }) {
      return TriageEngine(
        fs: fs,
        fetcher: _FakeFetcher(
          authored: authored,
          reviews: reviews,
          truncated: truncated,
        ),
        classifier: classifier(),
        teamResolver: _FakeResolver(fs, const {}),
      ).runTriage(
        configPath: configPath,
        topCount: topCount,
        refreshMergeable: false,
      );
    }

    test('caps each highlight list at topCount', () async {
      final result = await run(
        authored: [for (var i = 0; i < 6; i++) pr(number: i)],
        reviews: [
          for (var i = 0; i < 6; i++) pr(number: 100 + i, author: 'stranger'),
        ],
      );
      expect(result.myWork, hasLength(6));
      expect(result.topMyWork, hasLength(3));
      expect(result.topReviewQueue, hasLength(3));
    });

    test('drops self-authored PRs from the review queue', () async {
      final result = await run(
        reviews: [
          pr(number: 1, author: 'reidbaker'),
          pr(number: 2, author: 'stranger'),
        ],
      );
      expect(result.reviewQueue.map((i) => i.pr.number), [2]);
    });

    test(
      'reports truncation so a partial queue is not read as complete',
      () async {
        final result = await run(authored: [pr()], truncated: true);
        expect(result.truncated, isTrue);
        expect(result.toJson()['truncated'], isTrue);
      },
    );

    test('surfaces config warnings', () async {
      final result = await run(configPath: '/missing.yaml');
      expect(result.warnings, isNotEmpty);
    });

    test('warns when org membership cannot be resolved', () async {
      fs
          .file('/config.yaml')
          .writeAsStringSync(
            'accounts:\n  - reidbaker\nteam_orgs:\n  - flutter\n',
          );
      final result = await run();
      expect(result.warnings.any((w) => w.contains('read:org')), isTrue);
    });

    test('promotes teammates once org membership resolves', () async {
      fs
          .file('/config.yaml')
          .writeAsStringSync(
            'accounts:\n  - reidbaker\nteam_orgs:\n  - flutter\n',
          );
      final result = await TriageEngine(
        fs: fs,
        fetcher: _FakeFetcher(
          reviews: [
            pr(
              author: 'gmackall',
              ciStatus: CiStatus.passing,
              totalCheckCount: 3,
            ),
          ],
        ),
        classifier: classifier(),
        teamResolver: _FakeResolver(fs, const {'gmackall'}),
      ).runTriage(configPath: '/config.yaml', refreshMergeable: false);

      expect(result.reviewQueue.single.tier, ReviewQueueTier.teamReviewRequest);
      expect(result.warnings, isEmpty);
    });

    test('throws when no account can be determined', () {
      expect(
        () => TriageEngine(
          fs: MemoryFileSystem(),
          fetcher: _FakeFetcher(currentUser: null),
          classifier: classifier(),
          teamResolver: _FakeResolver(fs, const {}),
        ).runTriage(configPath: null, refreshMergeable: false),
        throwsA(isA<StateError>()),
      );
    });

    test('top lists serialize as compact references', () async {
      final result = await run(authored: [pr(number: 42)]);
      final json = result.toJson();
      final top = (json['top_my_work'] as List).single as Map;
      expect(top['number'], 42);
      expect(top.containsKey('action_prompt'), isTrue);
      // Full PR payloads belong in my_work, not in the highlights.
      expect(top.containsKey('pr'), isFalse);
    });
  });

  group('GitHub search query construction', () {
    test('emits one query per account', () async {
      // Search qualifiers are ANDed with no grouping syntax, so
      // "author:a author:b" is an intersection and matches nothing.
      final runner = _RecordingRunner();
      await GitHubPrFetcher(runner: runner.call)
          .fetchAuthored(authors: ['reidbaker', 'reidbaker-agent'], limit: 10);
      final queries = runner.queries;
      expect(queries, hasLength(2));
      expect(queries[0], endsWith('author:reidbaker'));
      expect(queries[1], endsWith('author:reidbaker-agent'));
      expect(
        queries.any((q) => q.contains('author:reidbaker author:')),
        isFalse,
      );
    });

    test('clamps the page size to GitHub\'s cap', () async {
      // first: 101 returns EXCESSIVE_PAGINATION and kills the whole run.
      final runner = _RecordingRunner();
      await GitHubPrFetcher(runner: runner.call)
          .fetchAuthored(authors: ['reidbaker'], limit: 500);
      expect(runner.lastArgs, contains('limit=$kMaxSearchPageSize'));
    });

    test(
      'surfaces GraphQL errors instead of returning an empty queue',
      () async {
        final runner = _RecordingRunner(
          response: '{"errors":[{"message":"Bad credentials"}]}',
        );
        expect(
          () =>
              GitHubPrFetcher(runner: runner.call)
                  .fetchAuthored(authors: ['reidbaker'], limit: 10),
          throwsA(isA<Exception>()),
        );
      },
    );

    test(
      'reports truncation when GitHub has more matches than requested',
      () async {
        final runner = _RecordingRunner(
          response: '{"data":{"search":{"issueCount":137,"nodes":[]}}}',
        );
        final result = await GitHubPrFetcher(runner: runner.call)
            .fetchAuthored(authors: ['reidbaker'], limit: 10);
        expect(result.truncated, isTrue);
      },
    );

    test('includes assignee only when asked', () async {
      final plain = _RecordingRunner();
      await GitHubPrFetcher(runner: plain.call)
          .fetchReviewQueue(reviewers: ['reidbaker'], limit: 10);
      expect(plain.queries, hasLength(1));

      final both = _RecordingRunner();
      await GitHubPrFetcher(runner: both.call).fetchReviewQueue(
        reviewers: ['reidbaker'],
        limit: 10,
        includeAssignee: true,
      );
      expect(both.queries, hasLength(2));
      expect(both.queries.any((q) => q.contains('assignee:')), isTrue);
    });
  });
}

/// Fetcher stub that returns canned queues without touching the network.
class _FakeFetcher extends GitHubPrFetcher {
  _FakeFetcher({
    this.authored = const [],
    this.reviews = const [],
    this.truncated = false,
    this.currentUser = 'reidbaker',
  }) : super(runner: _unreachableRunner);

  final List<PrItem> authored;
  final List<PrItem> reviews;
  final bool truncated;
  final String? currentUser;

  @override
  Future<String?> getCurrentUser() async => currentUser;

  @override
  Future<PrSearchResult> fetchAuthored({
    required List<String> authors,
    int limit = 50,
  }) async => PrSearchResult(items: authored, truncated: truncated);

  @override
  Future<PrSearchResult> fetchReviewQueue({
    required List<String> reviewers,
    int limit = 50,
    bool includeAssignee = false,
  }) async => PrSearchResult(items: reviews);

  @override
  Future<List<PrItem>> refreshUnknownMergeable(
    List<PrItem> items, {
    Duration delay = Duration.zero,
  }) async => items;
}

/// Fails the test rather than shelling out if a stub misses an override.
Future<ProcessResult> _unreachableRunner(String _, List<String> args) async {
  throw StateError('Unexpected process call: gh ${args.join(' ')}');
}

/// Team resolver stub; an empty set stands in for a token without read:org.
class _FakeResolver extends TeamResolver {
  _FakeResolver(FileSystem fs, this.members)
    : super(fs: fs, runner: _unreachableRunner);

  final Set<String> members;

  @override
  Future<Set<String>> resolveMembers(
    List<String> orgs, {
    String? cachePath,
  }) async => members;
}

/// Captures the arguments passed to `gh` so query construction can be asserted
/// without a network round trip.
class _RecordingRunner {
  _RecordingRunner({this.response = '{"data":{"search":{"nodes":[]}}}'});

  final String response;
  final List<List<String>> calls = [];

  List<String> get lastArgs => calls.last;

  /// Every `searchQuery=` payload seen, in call order.
  List<String> get queries => [
    for (final args in calls)
      args.firstWhere(
        (a) => a.startsWith('searchQuery='),
        orElse: () => args.join(' '),
      ),
  ];

  Future<ProcessResult> call(String executable, List<String> args) async {
    calls.add(args);
    return ProcessResult(0, 0, response, '');
  }
}
