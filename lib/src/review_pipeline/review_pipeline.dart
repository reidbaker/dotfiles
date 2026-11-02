import 'dart:convert';
import 'dart:io';
import 'dart:math' as math;

import 'package:path/path.dart' as p;

/// Supported engineering review personas and their default comment caps `k`.
enum ReviewPersona {
  reid('reid', 'reid-baker', 5),
  stuart('stuart', 'stuart-morgan', 5),
  lasse('lasse', 'lasse-nielsen', 8),
  jake('jake', 'jake-macdonald', 5),
  david('david', 'david-morgan', 5),
  codefu('codefu', 'john-mcdole', 5),
  loic('loic', 'loic-sharma', 5),
  chris('chris', 'chris-bracken', 5),
  paul('paul', 'paul-berry', 5),
  jenn('jenn', 'jenn-magder', 5),
  flutterToolsGhost('flutter-tools-ghost', 'flutter-tools-ghost', 6);

  const ReviewPersona(this.shortName, this.slug, this.defaultCap);

  final String shortName;
  final String slug;
  final int defaultCap;

  static ReviewPersona parse(String input) {
    final normalized = input.trim().toLowerCase();
    if (normalized == 'jmagman') return ReviewPersona.jenn;
    if (normalized == 'tools' || normalized == 'flutter_tools') {
      return ReviewPersona.flutterToolsGhost;
    }
    for (final persona in values) {
      if (persona.shortName == normalized || persona.slug == normalized) {
        return persona;
      }
    }
    final allowed = [for (final v in values) v.shortName].join(', ');
    throw ArgumentError('Unknown persona "$input". Expected one of: $allowed');
  }
}

/// Canonical Stage 1 -> Stage 2 persona-aligned lens categories.
const stage1LensCategories = [
  'architecture-placement',
  'script-vs-dart-tooling',
  'scope-hygiene',
  'test-assertion-fidelity',
  'api-naming-ergonomics',
  'ci-workflow-safety',
  'config-sdk-bounds',
  'bug-lifecycle',
];

/// Strips any prescriptive `why` hypothesis from a Stage 1 locus record so
/// Stage 2 receives only a neutral pointer (`what_is_here` + `lens`).
Map<String, Object?> sanitizeLocusForStage2(Map<String, dynamic> locus) {
  final whatIsHere = '${locus['what_is_here'] ?? ''}'.trim();
  return {
    'path': '${locus['path'] ?? ''}',
    'line': (locus['line'] as num?)?.toInt() ?? 1,
    if (locus['line_range'] is List) 'line_range': locus['line_range'],
    'lens': '${locus['lens'] ?? 'architecture-placement'}',
    'what_is_here': whatIsHere.isNotEmpty
        ? whatIsHere
        : fileDomain('${locus['path'] ?? ''}'),
    if (locus['prior_thread'] != null &&
        '${locus['prior_thread']}'.trim().isNotEmpty)
      'prior_thread': '${locus['prior_thread']}',
  };
}

/// One `@@` hunk of a unified diff.
final class DiffHunk {
  final String path;
  final int index;
  final int newStart;
  final int newCount;
  final String text;
  final bool deletedFile;

  const DiffHunk({
    required this.path,
    required this.index,
    required this.newStart,
    required this.newCount,
    required this.text,
    this.deletedFile = false,
  });

  /// The last new-file line the hunk covers.
  int get newEnd => newCount == 0 ? newStart : newStart + newCount - 1;

  String get id => '$path#$index';

  Map<String, Object?> toJson({String? context}) => {
    'id': id,
    'path': path,
    'domain': fileDomain(path),
    'new_start': newStart,
    'new_end': newEnd,
    if (deletedFile) 'deleted_file': true,
    'hunk': text,
    'context': ?context,
  };
}

final _hunkHeader = RegExp(r'^@@ -\d+(?:,\d+)? \+(\d+)(?:,(\d+))? @@');
final _diffGitNewPath = RegExp(r' b/(.+)$');

/// The hunks of [diff], in order, with their new-file line ranges.
List<DiffHunk> parseUnifiedDiff(String diff) {
  final parser = _UnifiedDiffParser();
  const LineSplitter().convert(diff).forEach(parser.addLine);
  return parser.finish();
}

/// Line-by-line state for [parseUnifiedDiff].
final class _UnifiedDiffParser {
  final _hunks = <DiffHunk>[];
  final _body = StringBuffer();
  String? _path;
  var _deleted = false;
  var _index = 0;
  int? _start;
  var _count = 0;

  void addLine(String line) {
    if (line.startsWith('diff --git ')) {
      _flush();
      _path = _diffGitNewPath.firstMatch(line)?[1];
      _deleted = false;
      _index = 0;
    } else if (line.startsWith('deleted file mode')) {
      _deleted = true;
    } else if (line.startsWith('+++ ') && _start == null) {
      final target = line.substring(4);
      if (target.startsWith('b/')) _path = target.substring(2);
    } else if (_hunkHeader.firstMatch(line) case final m?) {
      _flush();
      _start = int.parse(m[1]!);
      _count = m[2] == null ? 1 : int.parse(m[2]!);
      _body.writeln(line);
    } else if (_start != null) {
      _body.writeln(line);
    }
  }

  /// Flushes the open hunk and returns every hunk parsed so far.
  List<DiffHunk> finish() {
    _flush();
    return _hunks;
  }

  void _flush() {
    final path = _path;
    final start = _start;
    if (path != null && start != null) {
      _hunks.add(
        DiffHunk(
          path: path,
          index: _index++,
          newStart: start,
          newCount: _count,
          text: _body.toString().trimRight(),
          deletedFile: _deleted,
        ),
      );
    }
    _start = null;
    _body.clear();
  }
}

/// Line-numbered lines of [file] from `context` lines before [hunk] to
/// `context` lines after it, or null when the file does not exist.
String? fileContext(File file, DiffHunk hunk, int context) {
  if (hunk.deletedFile || !file.existsSync()) return null;
  final lines = file.readAsLinesSync();
  final from = (hunk.newStart - context).clamp(1, lines.length + 1);
  final to = (hunk.newEnd + context).clamp(0, lines.length);
  return [for (var n = from; n <= to; n++) '$n: ${lines[n - 1]}'].join('\n');
}

/// The file domain tag used by the persona inspection checklists.
String fileDomain(String path) {
  final base = p.basename(path);
  if (path.startsWith('.github/workflows/') || base == '.ci.yaml') {
    return 'ci_workflow_yaml';
  }
  if (path.startsWith('.github/')) return 'github_config';
  if (base.endsWith('.gradle') ||
      base.endsWith('.gradle.kts') ||
      base.endsWith('.kt') ||
      base.endsWith('.java')) {
    return 'gradle_android';
  }
  if (base.endsWith('.cc') ||
      base.endsWith('.cpp') ||
      base.endsWith('.h') ||
      base.endsWith('.mm') ||
      base.endsWith('.m') ||
      base.endsWith('.swift')) {
    return 'native_cpp_darwin';
  }
  if (base.endsWith('.dart')) {
    return (path.contains('test/') || base.endsWith('_test.dart'))
        ? 'dart_test'
        : 'dart_lib';
  }
  if (base.endsWith('.md')) return 'markdown_doc';
  if (base.endsWith('.yaml') || base.endsWith('.yml')) return 'yaml_config';
  return 'other';
}

/// Severity levels a Stage 2 critic may assign, lowest first.
const severities = ['nit', 'minor', 'major', 'blocking'];

/// One candidate's fields inside a Stage 2 record, with its 1-based candidate
/// index and its sort order.
typedef _CandidateMap = ({Map<String, dynamic> fields, int index, int order});

/// Record-level fields that a Stage 2 candidate falls back to.
typedef _RecordDefaults = ({
  String path,
  int line,
  String lens,
  Object? severity,
});

/// One Stage 2 critic candidate comment.
final class CriticComment {
  final String path;
  final int line;
  final String comment;
  final String severity;
  final String lens;
  final String feedbackType;
  final double confidence;
  final int candidateIndex;
  final int order;

  const CriticComment({
    required this.path,
    required this.line,
    required this.comment,
    required this.severity,
    this.lens = '',
    this.feedbackType = '',
    this.confidence = 0.75,
    this.candidateIndex = 1,
    this.order = 0,
  });

  int get severityRank => severities.indexOf(severity);

  String get effectiveType => feedbackType.isNotEmpty ? feedbackType : lens;

  /// Parses up to 2 candidate comments (`candidate_1`, `candidate_2`,
  /// `candidates`, or a flat v1 record) from [r].
  static List<CriticComment> parseRecord(
    Map<String, dynamic> r,
    int baseOrder,
  ) {
    final defaults = (
      path: '${r['path'] ?? ''}'.trim(),
      line: (r['line'] as num?)?.toInt() ?? 0,
      lens: '${r['lens'] ?? ''}'.trim(),
      severity: r['severity'],
    );
    return [
      for (final candidate in _candidateMaps(r, baseOrder))
        ?_parseCandidate(candidate, defaults),
    ];
  }

  /// The candidate maps in [r], each with its candidate index and sort order.
  static List<_CandidateMap> _candidateMaps(
    Map<String, dynamic> r,
    int baseOrder,
  ) {
    if (r['candidate_1'] is Map || r['candidate_2'] is Map) {
      return [
        if (r['candidate_1'] case final Map c1)
          (fields: c1.cast<String, dynamic>(), index: 1, order: baseOrder * 2),
        if (r['candidate_2'] case final Map c2)
          (
            fields: c2.cast<String, dynamic>(),
            index: 2,
            order: baseOrder * 2 + 1,
          ),
      ];
    }
    if (r['candidates'] ?? r['comments'] case final List list) {
      return [
        for (var i = 0; i < list.length; i++)
          if (list[i] case final Map item)
            (
              fields: item.cast<String, dynamic>(),
              index: i + 1,
              order: baseOrder * 2 + i,
            ),
      ];
    }
    return [(fields: r, index: 1, order: baseOrder)];
  }

  static const _severityAliases = {
    'must-fix': 'blocking',
    'should-fix': 'major',
    'question': 'minor',
  };

  /// Comment texts that mean the critic had nothing to say.
  static const _noCommentTexts = {'', 'no comment', 'null'};

  /// Parses one [candidate], falling back to the record-level [defaults], or
  /// returns null for a no-comment or unknown-severity candidate.
  static CriticComment? _parseCandidate(
    _CandidateMap candidate,
    _RecordDefaults defaults,
  ) {
    final c = candidate.fields;
    final text = '${c['body'] ?? c['comment'] ?? ''}'.trim();
    final rawSeverity = '${c['severity'] ?? defaults.severity ?? 'minor'}'
        .toLowerCase();
    final severity = _severityAliases[rawSeverity] ?? rawSeverity;
    if (_noCommentTexts.contains(text.toLowerCase()) ||
        !severities.contains(severity)) {
      return null;
    }
    final feedbackType = '${c['feedback_type'] ?? c['lens'] ?? defaults.lens}'
        .trim();
    return CriticComment(
      path: '${c['path'] ?? defaults.path}'.trim(),
      line: (c['line'] as num?)?.toInt() ?? defaults.line,
      comment: text,
      severity: severity,
      lens: defaults.lens.isNotEmpty ? defaults.lens : feedbackType,
      feedbackType: feedbackType,
      confidence: ((c['confidence'] as num?)?.toDouble() ?? 0.75).clamp(
        0.0,
        1.0,
      ),
      candidateIndex: candidate.index,
      order: candidate.order,
    );
  }

  /// Parses [r], or returns null for "no comment" records.
  static CriticComment? tryParse(Map<String, dynamic> r, int order) =>
      parseRecord(r, order).firstOrNull;

  CriticComment withComment(String text) => CriticComment(
    path: path,
    line: line,
    comment: text,
    severity: severity,
    lens: lens,
    feedbackType: feedbackType,
    confidence: confidence,
    candidateIndex: candidateIndex,
    order: order,
  );

  Map<String, Object?> toJson() => {
    'path': path,
    'line': line,
    'severity': severity,
    'lens': lens,
    if (feedbackType.isNotEmpty && feedbackType != lens)
      'feedback_type': feedbackType,
    'comment': comment,
  };
}

/// The Stage 3 result for one review pass.
final class RankedReview {
  final List<CriticComment> deduped;
  final List<CriticComment> comments;
  final String verdict;
  final String? maxSeverity;

  const RankedReview({
    required this.deduped,
    required this.comments,
    required this.verdict,
    required this.maxSeverity,
  });

  Map<String, Object?> toJson() => {
    'verdict': verdict,
    'max_severity': maxSeverity,
    'comments': [for (final c in comments) c.toJson()],
    'uncapped': [for (final c in deduped) c.toJson()],
  };
}

const _stopwords = {
  'the',
  'and',
  'for',
  'that',
  'this',
  'with',
  'from',
  'are',
  'was',
  'were',
  'have',
  'has',
  'had',
  'not',
  'but',
  'what',
  'when',
  'where',
  'which',
  'who',
  'why',
  'how',
  'can',
  'could',
  'should',
  'would',
  'will',
  'may',
  'might',
  'into',
  'only',
  'also',
  'than',
  'then',
  'there',
  'here',
  'they',
  'them',
  'their',
  'our',
  'your',
  'you',
  'its',
  'does',
  'did',
  'use',
  'using',
  'used',
  'make',
  'makes',
  'instead',
  'consider',
  'please',
  'think',
  'looks',
  'like',
  'feel',
  'feels',
  'some',
  'any',
  'all',
  'more',
  'other',
  'such',
  'each',
};

Set<String> _words(String s) => {
  for (final w in s.toLowerCase().split(RegExp(r'[^a-z0-9_]+')))
    if (w.length > 2) w,
};

Set<String> _contentWords(String s) => {
  for (final w in _words(s))
    if (!_stopwords.contains(w)) w,
};

/// Specific code identifiers (backticked, snake_case, camelCase, or hyphenated
/// tokens like `pubspec-lock` or `release_build_and_tag.yaml`) in [s].
Set<String> _codeIdentifiers(String s) {
  final out = <String>{};
  for (final m in RegExp(r'`([^`]{2,60})`').allMatches(s)) {
    final token = m.group(1)!.trim().toLowerCase();
    if (token.length >= 3 && !token.contains(' ')) out.add(token);
  }
  for (final m in RegExp(
    r'\b[a-zA-Z0-9_]+(?:[-_.][a-zA-Z0-9_]+)+\b',
  ).allMatches(s)) {
    out.add(m.group(0)!.toLowerCase());
  }
  return out;
}

/// Semantic similarity of [a] and [b], combining word Jaccard and content-word
/// containment so that a short question and a verbose paragraph asking the
/// same question match.
double textSimilarity(String a, String b) {
  final x = _words(a), y = _words(b);
  if (x.isEmpty || y.isEmpty) return 0;
  final jaccard = x.intersection(y).length / x.union(y).length;

  final cx = _contentWords(a), cy = _contentWords(b);
  if (cx.isEmpty || cy.isEmpty) return jaccard;
  final inter = cx.intersection(cy).length;
  final minLen = math.min(cx.length, cy.length);
  final maxLen = math.max(cx.length, cy.length);
  if (minLen >= 4 && inter >= 4) {
    final containment = inter / minLen;
    final adjusted = containment >= 0.70
        ? containment * 0.85
        : containment * (0.5 + 0.5 * (minLen / maxLen));
    return math.max(jaccard, adjusted);
  }
  return jaccard;
}

/// Phrases matched against a lowercased review comment: the comment matches
/// when it contains every [all] phrase and at least one [any] phrase.
final class _PhraseRule {
  const _PhraseRule(this.any, {this.all = const []});

  final List<String> any;
  final List<String> all;

  bool matches(String lowerComment) =>
      all.every(lowerComment.contains) && any.any(lowerComment.contains);
}

/// One [personaTypeWeight] tier: [weight] applies when the lowercased
/// feedback type contains a [types] entry, the lowercased comment matches a
/// [phrases] rule, or the raw comment contains a [rawPhrases] entry.
final class _WeightTier {
  const _WeightTier(
    this.weight, {
    this.types = const [],
    this.phrases = const [],
    this.rawPhrases = const [],
  });

  final double weight;
  final List<String> types;
  final List<_PhraseRule> phrases;
  final List<String> rawPhrases;

  bool matches(String lowerType, String lowerComment, String comment) =>
      types.any(lowerType.contains) ||
      phrases.any((rule) => rule.matches(lowerComment)) ||
      rawPhrases.any(comment.contains);
}

/// The keyword tables that Stage 3 scoring uses for one persona.
final class _PersonaProfile {
  const _PersonaProfile({
    required this.weightTiers,
    required this.fallbackWeight,
    required this.bugWeight,
    required this.signatureMarkers,
    required this.craftCategories,
    required this.fallbackCraftCategory,
    this.suggestionBonus = 0.25,
    this.penalizesHedges = false,
  });

  /// Type weight tiers, highest priority first; the first match wins.
  final List<_WeightTier> weightTiers;

  /// Type weight when no tier matches and the type is not a bug.
  final double fallbackWeight;

  /// Type weight for `bug` and `bug-lifecycle` comments no tier matched.
  final double bugWeight;

  /// Concrete craft markers for [hasPersonaSignatureMarker].
  final List<_PhraseRule> signatureMarkers;

  /// Craft categories for stratified slots, in priority order.
  final List<(String, _PhraseRule)> craftCategories;

  /// Craft category when no [craftCategories] entry matches.
  final String fallbackCraftCategory;

  /// Move bonus for a fenced `suggestion` or `dart` code block.
  final double suggestionBonus;

  /// Whether `non-blocking` and `IMO` hedges cost a move penalty.
  final bool penalizesHedges;
}

/// Phrases that point at work landed in, or deferred to, another PR.
const _scopeCreepPhrases = [
  'landed in pr',
  'other pr',
  'did not agree to this deferred',
];

/// A review comment about a code comment: the `test-comment` craft marker.
const _testCommentRule = _PhraseRule(
  ['dry run', 'deleted', 'empty quote', "'bottom'"],
  all: ['comment'],
);

const _jennProfile = _PersonaProfile(
  weightTiers: [
    _WeightTier(
      0.75,
      types: ['ios-macos', 'xcode', 'cocoapods', 'objc'],
      phrases: [
        _PhraseRule([
          'build_macos.dart',
          'only migrates ios',
          'also want to add it here for macos',
          'podfile',
          'use_frameworks!',
          'use_modular_headers!',
          'xcscheme',
          'info.plist',
          'xcdevice',
          'gesturerecognizer',
        ]),
      ],
    ),
    _WeightTier(
      0.72,
      types: ['concrete-suggestion', 'suggestion'],
      rawPhrases: ['```suggestion'],
    ),
    _WeightTier(
      0.68,
      types: ['docs-comments'],
      phrases: [
        _PhraseRule([
          'link back to an issue',
          'link back to a',
          'working around',
          'explaining the bug',
          'apple.com/xcode',
        ]),
      ],
    ),
    _WeightTier(
      0.62,
      types: ['flutter-tools', 'architecture', 'api-contract', 'api-naming'],
      phrases: [
        _PhraseRule(['enhanced enum', 'unawaited', 'aliases', 'dry out']),
      ],
    ),
    _WeightTier(
      0.58,
      types: ['tests-verification', 'test-assertion', 'simplify'],
      phrases: [
        _PhraseRule(['testwithoutcontext', 'unused import']),
      ],
    ),
    _WeightTier(
      0.55,
      types: ['scope'],
      phrases: [
        _PhraseRule(['break it into', 'split']),
      ],
    ),
  ],
  fallbackWeight: 0.30,
  bugWeight: 0.25,
  signatureMarkers: [
    _PhraseRule([
      'build_macos.dart',
      'only migrates ios',
      'also want to add it here for macos',
      'info.plist',
      'xcscheme',
      'podfile',
      'use_frameworks!',
      'use_modular_headers!',
      'gesturerecognizer',
      'respondstoselector',
      'synthesized property',
      'backing ivar',
      'link back to an issue',
      'link back to a',
      'working around',
      'explaining the bug',
      'unused import',
      'enhanced enum',
      'unawaited',
      'testwithoutcontext',
      'i know you',
      'directoryfrompath',
      'help.apple.com/xcode',
      '--vm-service-url',
      'takescreenshot',
      '```suggestion',
    ]),
    _PhraseRule(['target'], all: ['codesign']),
    _PhraseRule(['screenshot'], all: ['aliases']),
  ],
  craftCategories: [
    (
      'ios-macos-parity',
      _PhraseRule(['build_macos', 'only migrates ios', 'for macos']),
    ),
    (
      'workaround-issue-link',
      _PhraseRule(['link back to', 'working around', 'explaining the bug']),
    ),
    (
      'objc-idiom',
      _PhraseRule([
        'gesturerecognizer',
        'synthesized property',
        'backing ivar',
      ]),
    ),
    ('xcode-plist-podfile', _PhraseRule(['info.plist', 'xcscheme', 'podfile'])),
    ('unused-import', _PhraseRule(['unused import'])),
    ('dart-idiom', _PhraseRule(['enhanced enum', 'unawaited'])),
    ('suggestion-block', _PhraseRule(['```suggestion'])),
  ],
  fallbackCraftCategory: 'other-jenn-craft',
  suggestionBonus: 0.38,
  penalizesHedges: true,
);

const _flutterToolsGhostProfile = _PersonaProfile(
  weightTiers: [
    _WeightTier(
      0.78,
      types: ['di-globals', 'hermetic'],
      phrases: [
        _PhraseRule([
          'globals.',
          'testwithoutcontext',
          'bufferlogger',
          'fakeprocessmanager',
          'memoryfilesystem',
        ]),
      ],
    ),
    _WeightTier(
      0.75,
      types: ['build-system'],
      phrases: [
        _PhraseRule([
          'releaseunpackmacos',
          'unpackmacos',
          'caching fail',
          'source.artifact',
        ]),
        _PhraseRule(['outputs'], all: ['target']),
      ],
    ),
    _WeightTier(
      0.70,
      types: ['cli-ux', 'toolexit'],
      phrases: [
        _PhraseRule(['throwtoolexit', 'toolexit', 'stateerror', 'printtrace']),
        _PhraseRule(['option'], all: ['top-level']),
      ],
    ),
    _WeightTier(
      0.68,
      types: ['ios-macos', 'cocoapods', 'swiftpm'],
      phrases: [
        _PhraseRule([
          'swift package manager',
          'vendored_frameworks',
          'podspec',
          'info.plist',
        ]),
      ],
    ),
    _WeightTier(
      0.64,
      types: ['process-io', 'correctness'],
      phrases: [
        _PhraseRule([
          'errorhandlingfilesystem',
          'getbuilddirectory',
          'dart:io',
          'base/io.dart',
        ]),
      ],
    ),
    _WeightTier(
      0.58,
      types: ['concrete-suggestion', 'docs-comments', 'simplify'],
      rawPhrases: ['```suggestion'],
    ),
  ],
  fallbackWeight: 0.30,
  bugWeight: 0.25,
  signatureMarkers: [
    _PhraseRule([
      'globals.',
      'context globals',
      'testwithoutcontext',
      'bufferlogger',
      'fakeprocessmanager',
      'memoryfilesystem',
      'throwtoolexit',
      'errorhandlingfilesystem',
      'getbuilddirectory',
      'base/io.dart',
      'releaseunpackmacos',
      'unpackmacos',
      '_gethostartifactpath',
      'fluttermacosframeworkdsym',
      'printtrace',
      'unused import',
      'swift package manager',
      'swiftpm',
      'vendored_frameworks',
      'copyvendoredframeworks',
      'dartdevc-module-format',
      '```suggestion',
    ]),
    _PhraseRule(['stateerror'], all: ['toolexit']),
  ],
  craftCategories: [
    ('di-globals', _PhraseRule(['globals.', 'context globals'])),
    (
      'hermetic-test-fakes',
      _PhraseRule([
        'testwithoutcontext',
        'bufferlogger',
        'fakeprocessmanager',
        'memoryfilesystem',
      ]),
    ),
    (
      'toolexit-error',
      _PhraseRule(['throwtoolexit', 'toolexit', 'stateerror']),
    ),
    (
      'build-system-target',
      _PhraseRule(['releaseunpackmacos', 'unpackmacos', 'source.artifact']),
    ),
    ('trace-plist', _PhraseRule(['printtrace', 'info.plist'])),
    ('unused-import', _PhraseRule(['unused import'])),
    (
      'darwin-frameworks',
      _PhraseRule([
        'vendored_frameworks',
        'swift package manager',
        'copyvendoredframeworks',
      ]),
    ),
  ],
  fallbackCraftCategory: 'other-tools-craft',
  penalizesHedges: true,
);

const _defaultWeightTiers = [
  _WeightTier(
    0.75,
    types: ['scope'],
    phrases: [_PhraseRule(_scopeCreepPhrases)],
  ),
  _WeightTier(
    0.72,
    types: ['script-vs-dart', 'bash'],
    phrases: [
      _PhraseRule(['bash', 'into the dart', 'into dart']),
    ],
  ),
  _WeightTier(
    0.65,
    types: ['architecture', 'api-design', 'placement', 'structure'],
  ),
  _WeightTier(
    0.60,
    types: ['ci-workflow', 'process-release-ci'],
    phrases: [
      _PhraseRule(['reusable workflow', 'duplicate']),
    ],
  ),
  _WeightTier(0.55, types: ['test', 'why-needed', 'clarify']),
  _WeightTier(
    0.50,
    types: [
      'api-naming',
      'ergonomics',
      'future-maintainer',
      'config-sdk',
      'suggestion',
    ],
  ),
];

const _defaultSignatureMarkers = [
  _PhraseRule([
    'pronoun',
    'two-space',
    'two spaces',
    'empty string',
    "empty `''`",
    'already have a',
    'someplace else in the codebase',
    'change detector',
    'reusable workflow',
    'why a new file',
    'in a new test file',
    'why duplicate the',
    'duplicate the false default',
    'inline it',
    'inlining the',
    'helper method',
    'easier to read',
    'easier to follow',
    'hard to read',
    'type safe',
    'typesafe',
    'multi-line string',
    '```suggestion',
    '->',
    's/',
    "don't start any dartdoc with",
    'start with `returns`',
    'single sentence',
    'single-pass',
    '.tolist()',
    '.toset()',
    'here and below:',
    'here and elsewhere:',
    'nitpick:',
  ]),
  _PhraseRule(
    [
      'standardize',
      'mix',
      'both',
      'match the',
      'all .yaml',
      'all `.yaml`',
      'all yaml',
    ],
    all: ['.yaml', '.yml'],
  ),
  _testCommentRule,
];

const _defaultCraftCategories = [
  ('pronoun', _PhraseRule(['pronoun'])),
  (
    'inline-constant',
    _PhraseRule(['two-space', 'two spaces', 'inline it', 'inlining the']),
  ),
  ('yaml-ext', _PhraseRule(['.yml'], all: ['.yaml'])),
  ('test-comment', _testCommentRule),
  ('default-dedup', _PhraseRule(['why duplicate', 'false default'])),
  ('concrete-rewrite', _PhraseRule(['```suggestion', '->', 's/'])),
  ('dartdoc-precision', _PhraseRule(['dartdoc', 'returns', 'single sentence'])),
  ('collection-idiom', _PhraseRule(['.tolist()', '.toset()', 'single-pass'])),
  (
    'readability-helper',
    _PhraseRule([
      'helper method',
      'easier to read',
      'easier to follow',
      'hard to read',
    ]),
  ),
];

const _defaultProfile = _PersonaProfile(
  weightTiers: _defaultWeightTiers,
  fallbackWeight: 0.25,
  bugWeight: 0.45,
  signatureMarkers: _defaultSignatureMarkers,
  craftCategories: _defaultCraftCategories,
  fallbackCraftCategory: 'other-craft',
);

/// The Reid persona weighs `bug` comments lower than other default personas.
const _reidProfile = _PersonaProfile(
  weightTiers: _defaultWeightTiers,
  fallbackWeight: 0.25,
  bugWeight: 0.10,
  signatureMarkers: _defaultSignatureMarkers,
  craftCategories: _defaultCraftCategories,
  fallbackCraftCategory: 'other-craft',
);

_PersonaProfile _profileFor(String personSlug) => switch (personSlug) {
  'jenn-magder' => _jennProfile,
  'flutter-tools-ghost' => _flutterToolsGhostProfile,
  'reid-baker' => _reidProfile,
  _ => _defaultProfile,
};

/// Empirical persona weight for [effectiveType] and [comment] content.
double personaTypeWeight(
  String personSlug,
  String effectiveType,
  String comment,
) {
  final profile = _profileFor(personSlug);
  final t = effectiveType.toLowerCase();
  final lower = comment.toLowerCase();
  for (final tier in profile.weightTiers) {
    if (tier.matches(t, lower, comment)) return tier.weight;
  }
  if (t == 'bug' || t.contains('bug-lifecycle')) return profile.bugWeight;
  return profile.fallbackWeight;
}

final _personaOpener = RegExp(
  r'^(why|should|do we|is there|can we|can this|can you|does this|what happens|too many|too long|this change was landed|here and below|nitpick:|nit:|i know you|how about|did you mean)',
  caseSensitive: false,
);

final _genericDefensiveEssay = RegExp(
  r'network blip|rate limit|if `gh release view` fails|malformed input|race condition|\bfwiw\b|\bimho\b|\bmy gut says\b',
  caseSensitive: false,
);

final _blockingHedge = RegExp(
  r'\bnon-?blocking\b|\bimo\b',
  caseSensitive: false,
);

/// Bonus for persona-aligned rhetorical moves and penalty for generic AI
/// defensive-programming essays.
double personaMoveAdjustment(
  String comment, {
  String personSlug = 'reid-baker',
}) {
  final profile = _profileFor(personSlug);
  final trimmed = comment.trim();
  final lower = trimmed.toLowerCase();
  final wordCount = trimmed.split(RegExp(r'\s+')).length;
  // Keep this summation order: the scores feed sort ties and thresholds.
  var adj = 0.0;
  if (trimmed.contains('```suggestion') || trimmed.contains('```dart')) {
    adj += profile.suggestionBonus;
  }
  if (trimmed.contains('?') && wordCount <= 35) adj += 0.20;
  if (_personaOpener.hasMatch(trimmed)) adj += 0.20;
  if (hasPersonaSignatureMarker(lower, personSlug: personSlug)) adj += 0.65;
  if (_scopeCreepPhrases.any(lower.contains)) adj += 0.35;
  adj += _lengthAdjustment(wordCount);
  if (_genericDefensiveEssay.hasMatch(trimmed)) adj -= 0.55;
  if (profile.penalizesHedges && _blockingHedge.hasMatch(trimmed)) {
    adj -= 0.40;
  }
  return adj;
}

/// Bonus for a short comment and penalty for an essay.
double _lengthAdjustment(int wordCount) {
  if (wordCount >= 3 && wordCount <= 32) return 0.20;
  if (wordCount > 65) return -0.45;
  return 0.0;
}

/// Whether [lowerComment] exhibits a concrete persona craft/readability,
/// naming, collection-idiom, or test-clarity marker (typically emitted by
/// Stage 2 as `minor` or `nit`).
bool hasPersonaSignatureMarker(
  String lowerComment, {
  String personSlug = 'reid-baker',
}) =>
    _profileFor(personSlug).signatureMarkers
        .any((rule) => rule.matches(lowerComment));

/// Compressed base severity score so that high-signal `minor`/`nit` persona
/// craft catches can compete with generic `major` structural questions.
double _severityBaseScore(String severity) => switch (severity) {
  'blocking' => 3.0,
  'major' => 1.85,
  'minor' => 1.35,
  'nit' => 1.05,
  _ => 1.0,
};

/// Composite Stage 3 score for [c].
double compositeScore(CriticComment c, {String personSlug = 'reid-baker'}) {
  final base = _severityBaseScore(c.severity);
  final typeW = personaTypeWeight(personSlug, c.effectiveType, c.comment);
  final moveAdj = personaMoveAdjustment(c.comment, personSlug: personSlug);
  final confAdj = (c.confidence - 0.75) * 0.3;
  final candAdj = c.candidateIndex == 1 ? 0.08 : 0.0;
  return base + (typeW + moveAdj + confAdj + candAdj).clamp(-0.85, 1.45);
}

/// Craft category for stratified slot diversity so two pronoun comments do not
/// crowd out a constant-inlining, rewrite, or test-comment catch.
String _craftCategory(String lowerComment, {String personSlug = 'reid-baker'}) {
  final profile = _profileFor(personSlug);
  for (final (category, rule) in profile.craftCategories) {
    if (rule.matches(lowerComment)) return category;
  }
  return profile.fallbackCraftCategory;
}

/// Whether [c] belongs to the persona craft/readability/test-clarity stratum
/// (`minor` or `nit` comment with a concrete signature marker or
/// readability/test-assertion lens).
bool _isCraftPersonaCandidate(
  CriticComment c, {
  String personSlug = 'reid-baker',
}) {
  if (c.comment == 'Same here.') return false;
  if (c.severityRank >= severities.indexOf('major')) return false;
  final lower = c.comment.toLowerCase();
  return hasPersonaSignatureMarker(lower, personSlug: personSlug) ||
      c.effectiveType == 'api-naming-ergonomics' ||
      c.effectiveType == 'test-assertion-fidelity';
}

/// Selects up to [k] comments from [deduped] using stratified slot allocation:
/// reserves up to 2 slots (when `k >= 5` on Pass 1–2, or 1 slot on Pass 3+)
/// for high-scoring `minor`/`nit` persona craft catches on distinct files and
/// distinct craft categories so `major` structural questions and `Same here.`
/// repeats do not crowd out all craft moves.
List<CriticComment> _selectStratifiedTopK(
  List<CriticComment> deduped, {
  required int k,
  required String personSlug,
  int passIndex = 1,
}) {
  if (deduped.length <= k || k < 4) {
    return deduped.take(k).toList();
  }

  final maxCraftQuota = passIndex >= 3 ? 1 : (k >= 5 ? 2 : 1);
  final craftPool = _craftPool(
    deduped,
    maxCraftQuota: maxCraftQuota,
    personSlug: personSlug,
  );
  if (craftPool.isEmpty) {
    return deduped.take(k).toList();
  }

  final reservedCraft = craftPool.take(maxCraftQuota).toList();
  final selected = _structuralPicks(
    deduped,
    quota: k - reservedCraft.length,
    reserved: reservedCraft,
  );
  // Add the reserved craft catches, then fill any free slots in rank order.
  for (final c in [...reservedCraft, ...deduped]) {
    if (selected.length >= k) break;
    if (!selected.contains(c)) selected.add(c);
  }
  return selected;
}

/// High-scoring craft catches from [deduped]: first one per file and per
/// craft category, then, while fewer than [maxCraftQuota], one per category.
List<CriticComment> _craftPool(
  List<CriticComment> deduped, {
  required int maxCraftQuota,
  required String personSlug,
}) {
  bool qualifies(CriticComment c) =>
      _isCraftPersonaCandidate(c, personSlug: personSlug) &&
      compositeScore(c, personSlug: personSlug) >= 2.0;
  String category(CriticComment c) =>
      _craftCategory(c.comment.toLowerCase(), personSlug: personSlug);

  final pool = <CriticComment>[];
  final files = <String>{};
  final categories = <String>{};
  for (final c in deduped) {
    if (qualifies(c) && files.add(c.path) && categories.add(category(c))) {
      pool.add(c);
    }
  }
  for (final c in deduped) {
    if (pool.length >= maxCraftQuota) break;
    if (!pool.contains(c) && qualifies(c) && categories.add(category(c))) {
      pool.add(c);
    }
  }
  return pool;
}

/// The first [quota] comments of [deduped] that are not in [reserved],
/// keeping at most one `Same here.` repeat.
List<CriticComment> _structuralPicks(
  List<CriticComment> deduped, {
  required int quota,
  required List<CriticComment> reserved,
}) {
  final selected = <CriticComment>[];
  var keptSameHere = false;
  for (final c in deduped) {
    if (selected.length >= quota) break;
    if (reserved.contains(c)) continue;
    if (c.comment == 'Same here.') {
      if (keptSameHere) continue;
      keptSameHere = true;
    }
    selected.add(c);
  }
  return selected;
}

bool _sharesSameFileSubject(CriticComment a, CriticComment b) {
  if (a.path != b.path) return false;
  final idsA = _codeIdentifiers(a.comment);
  final idsB = _codeIdentifiers(b.comment);
  if (idsA.intersection(idsB).isNotEmpty) return true;
  final cx = _contentWords(a.comment);
  final cy = _contentWords(b.comment);
  return cx.intersection(cy).length >= 4;
}

/// Dedupes, ranks, and caps [candidates] to [k], then computes the verdict.
RankedReview rankReview(
  List<CriticComment> candidates, {
  required int k,
  String personSlug = 'reid-baker',
  int passIndex = 1,
  int lineWindow = 3,
  double similarity = 0.5,
}) {
  final ranked = [...candidates]
    ..sort((x, y) {
      final s = compositeScore(
        y,
        personSlug: personSlug,
      ).compareTo(compositeScore(x, personSlug: personSlug));
      return s != 0 ? s : x.order.compareTo(y.order);
    });
  final deduped = _dedupeRanked(
    ranked,
    lineWindow: lineWindow,
    similarity: similarity,
  );
  final kept = _selectStratifiedTopK(
    deduped,
    k: k,
    personSlug: personSlug,
    passIndex: passIndex,
  );
  final max = kept.isEmpty
      ? null
      : severities[kept
            .map((c) => c.severityRank)
            .reduce((a, b) => a > b ? a : b)];
  final majorCount = kept
      .where(
        (c) =>
            c.comment != 'Same here.' &&
            c.severityRank >= severities.indexOf('major'),
      )
      .length;
  return RankedReview(
    deduped: deduped,
    comments: kept,
    verdict: reviewVerdict(
      passIndex: passIndex,
      maxSeverity: max,
      majorCount: majorCount,
      hasBlockingScopeCreep: kept.any(_isBlockingScopeCreep),
    ),
    maxSeverity: max,
  );
}

/// Drops comments within [lineWindow] lines of a higher-ranked comment on the
/// same file, and folds near-duplicates on other files into one `Same here.`
/// repeat per file after the comment they repeat.
List<CriticComment> _dedupeRanked(
  List<CriticComment> ranked, {
  required int lineWindow,
  required double similarity,
}) {
  final primaries = <CriticComment>[];
  final repeats = <CriticComment, List<CriticComment>>{};
  for (final c in ranked) {
    if (primaries.any(
      (o) => o.path == c.path && (o.line - c.line).abs() <= lineWindow,
    )) {
      continue;
    }
    final twin = primaries
        .where((o) => _isNearDuplicate(o, c, similarity))
        .firstOrNull;
    if (twin == null) {
      primaries.add(c);
    } else if (!_sharesSameFileSubject(twin, c)) {
      final list = repeats[twin] ??= [];
      if (!list.any((r) => r.path == c.path)) {
        list.add(c.withComment('Same here.'));
      }
    }
  }
  return [
    for (final c in primaries) ...[c, ...?repeats[c]],
  ];
}

/// Whether [c] repeats the higher-ranked [primary].
bool _isNearDuplicate(
  CriticComment primary,
  CriticComment c,
  double similarity,
) {
  if (_sharesSameFileSubject(primary, c) &&
      _contentWords(primary.comment)
              .intersection(_contentWords(c.comment))
              .length >=
          3) {
    return true;
  }
  final threshold = primary.path == c.path ? similarity * 0.85 : similarity;
  return textSimilarity(primary.comment, c.comment) >= threshold;
}

/// Phrases that make a `major` comment block the review as scope creep.
const _blockingScopeCreepPhrases = [
  ..._scopeCreepPhrases,
  'new test file',
  'why a new file',
];

bool _isBlockingScopeCreep(CriticComment c) {
  final lower = c.comment.toLowerCase();
  return c.severityRank >= severities.indexOf('major') &&
      _blockingScopeCreepPhrases.any(lower.contains);
}

/// The review verdict for a pass with the given maximum comment severity.
///
/// The verdict never depends on who authored the PR: accounts are often
/// shared between a person and their agents.
String reviewVerdict({
  required int passIndex,
  required String? maxSeverity,
  int majorCount = 0,
  bool hasBlockingScopeCreep = false,
}) {
  final rank = maxSeverity == null ? -1 : severities.indexOf(maxSeverity);
  if (rank >= severities.indexOf('blocking')) return 'REQUEST_CHANGES';
  if (hasBlockingScopeCreep) return 'REQUEST_CHANGES';
  if (passIndex <= 1) {
    if (rank <= severities.indexOf('nit')) return 'APPROVE';
    if (majorCount >= 4) return 'REQUEST_CHANGES';
    return 'COMMENT';
  }
  if (passIndex == 2 && rank >= severities.indexOf('major')) {
    return 'REQUEST_CHANGES';
  }
  if (passIndex >= 3 && majorCount >= 5) {
    return 'REQUEST_CHANGES';
  }
  return 'APPROVE';
}

/// Reads a JSON Lines file into a list of decoded JSON objects.
List<Map<String, dynamic>> readJsonl(File file) {
  if (!file.existsSync()) return [];
  final out = <Map<String, dynamic>>[];
  for (final line in file.readAsLinesSync()) {
    final trimmed = line.trim();
    if (trimmed.isEmpty || !trimmed.startsWith('{')) continue;
    try {
      final decoded = jsonDecode(trimmed);
      if (decoded is Map<String, dynamic>) {
        out.add(decoded);
      }
    } on FormatException {
      // Skip malformed/truncated lines when reading incremental logs.
    }
  }
  return out;
}

/// Writes a list of JSON objects to [file] atomically.
void writeJsonl(File file, Iterable<Map<String, Object?>> records) {
  file.parent.createSync(recursive: true);
  final tmp = File('${file.path}.tmp');
  final buf = StringBuffer();
  for (final r in records) {
    buf.writeln(jsonEncode(r));
  }
  tmp.writeAsStringSync(buf.toString());
  tmp.renameSync(file.path);
}
