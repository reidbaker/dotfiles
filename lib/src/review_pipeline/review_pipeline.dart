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
  paul('paul', 'paul-berry', 5);

  const ReviewPersona(this.shortName, this.slug, this.defaultCap);

  final String shortName;
  final String slug;
  final int defaultCap;

  static ReviewPersona parse(String input) {
    final normalized = input.trim().toLowerCase();
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

/// The hunks of [diff], in order, with their new-file line ranges.
List<DiffHunk> parseUnifiedDiff(String diff) {
  final hunks = <DiffHunk>[];
  String? path;
  var deleted = false;
  var index = 0;
  int? start;
  var count = 0;
  final body = StringBuffer();

  void flush() {
    if (path != null && start != null) {
      hunks.add(
        DiffHunk(
          path: path,
          index: index++,
          newStart: start!,
          newCount: count,
          text: body.toString().trimRight(),
          deletedFile: deleted,
        ),
      );
    }
    start = null;
    body.clear();
  }

  for (final line in const LineSplitter().convert(diff)) {
    if (line.startsWith('diff --git ')) {
      flush();
      final m = RegExp(r' b/(.+)$').firstMatch(line);
      path = m?[1];
      deleted = false;
      index = 0;
    } else if (line.startsWith('deleted file mode')) {
      deleted = true;
    } else if (line.startsWith('+++ ') && start == null) {
      final target = line.substring(4);
      if (target.startsWith('b/')) path = target.substring(2);
    } else if (_hunkHeader.firstMatch(line) case final m?) {
      flush();
      start = int.parse(m[1]!);
      count = m[2] == null ? 1 : int.parse(m[2]!);
      body.writeln(line);
    } else if (start != null) {
      body.writeln(line);
    }
  }
  flush();
  return hunks;
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
    final path = '${r['path'] ?? ''}'.trim();
    final defaultLine = (r['line'] as num?)?.toInt() ?? 0;
    final defaultLens = '${r['lens'] ?? ''}'.trim();
    final out = <CriticComment>[];

    CriticComment? parseOne(
      Map<String, dynamic> c,
      int candIdx,
      int itemOrder,
    ) {
      final rawText = '${c['body'] ?? c['comment'] ?? ''}'.trim();
      const sevAliases = {
        'must-fix': 'blocking',
        'should-fix': 'major',
        'question': 'minor',
      };
      final rawSev = '${c['severity'] ?? r['severity'] ?? 'minor'}'
          .toLowerCase();
      final sev = sevAliases[rawSev] ?? rawSev;
      if (rawText.isEmpty ||
          rawText.toLowerCase() == 'no comment' ||
          rawText.toLowerCase() == 'null' ||
          !severities.contains(sev)) {
        return null;
      }
      final cPath = '${c['path'] ?? path}'.trim();
      final cLine = (c['line'] as num?)?.toInt() ?? defaultLine;
      final fType = '${c['feedback_type'] ?? c['lens'] ?? defaultLens}'.trim();
      final conf = ((c['confidence'] as num?)?.toDouble() ?? 0.75).clamp(
        0.0,
        1.0,
      );
      return CriticComment(
        path: cPath,
        line: cLine,
        comment: rawText,
        severity: sev,
        lens: defaultLens.isNotEmpty ? defaultLens : fType,
        feedbackType: fType,
        confidence: conf,
        candidateIndex: candIdx,
        order: itemOrder,
      );
    }

    if (r['candidate_1'] is Map || r['candidate_2'] is Map) {
      if (r['candidate_1'] is Map) {
        final c1 = parseOne(
          (r['candidate_1'] as Map).cast<String, dynamic>(),
          1,
          baseOrder * 2,
        );
        if (c1 != null) out.add(c1);
      }
      if (r['candidate_2'] is Map) {
        final c2 = parseOne(
          (r['candidate_2'] as Map).cast<String, dynamic>(),
          2,
          baseOrder * 2 + 1,
        );
        if (c2 != null) out.add(c2);
      }
      return out;
    }

    final listField = r['candidates'] ?? r['comments'];
    if (listField is List) {
      final list = listField;
      for (var i = 0; i < list.length; i++) {
        final item = list[i];
        if (item is Map) {
          final parsed = parseOne(
            item.cast<String, dynamic>(),
            i + 1,
            baseOrder * 2 + i,
          );
          if (parsed != null) out.add(parsed);
        }
      }
      return out;
    }

    final single = parseOne(r, 1, baseOrder);
    if (single != null) out.add(single);
    return out;
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

/// Empirical persona weight for [effectiveType] and [comment] content.
double personaTypeWeight(
  String personSlug,
  String effectiveType,
  String comment,
) {
  final t = effectiveType.toLowerCase();
  final lower = comment.toLowerCase();

  var weight = 0.25;
  if (t.contains('scope') ||
      lower.contains('landed in pr') ||
      lower.contains('other pr') ||
      lower.contains('did not agree to this deferred')) {
    weight = 0.75;
  } else if (t.contains('script-vs-dart') ||
      t.contains('bash') ||
      lower.contains('bash') ||
      lower.contains('into the dart') ||
      lower.contains('into dart')) {
    weight = 0.72;
  } else if (t.contains('architecture') ||
      t.contains('api-design') ||
      t.contains('placement') ||
      t.contains('structure')) {
    weight = 0.65;
  } else if (t.contains('ci-workflow') ||
      t.contains('process-release-ci') ||
      lower.contains('reusable workflow') ||
      lower.contains('duplicate')) {
    weight = 0.60;
  } else if (t.contains('test') ||
      t.contains('why-needed') ||
      t.contains('clarify')) {
    weight = 0.55;
  } else if (t.contains('api-naming') ||
      t.contains('ergonomics') ||
      t.contains('future-maintainer') ||
      t.contains('config-sdk') ||
      t.contains('suggestion')) {
    weight = 0.50;
  } else if (t == 'bug' || t.contains('bug-lifecycle')) {
    weight = personSlug == 'reid-baker' ? 0.10 : 0.45;
  }
  return weight;
}

/// Bonus for persona-aligned rhetorical moves and penalty for generic AI
/// defensive-programming essays.
double personaMoveAdjustment(String comment) {
  final trimmed = comment.trim();
  final lower = trimmed.toLowerCase();
  final wordCount = trimmed.split(RegExp(r'\s+')).length;
  var adj = 0.0;

  if (trimmed.contains('```suggestion') || trimmed.contains('```dart')) {
    adj += 0.25;
  }
  if (trimmed.contains('?') && wordCount <= 35) adj += 0.20;
  if (RegExp(
    r'^(why|should|do we|is there|can we|can this|does this|what happens|too many|too long|this change was landed|here and below|nitpick:)',
    caseSensitive: false,
  ).hasMatch(trimmed)) {
    adj += 0.20;
  }
  if (hasPersonaSignatureMarker(lower)) {
    adj += 0.65;
  }
  if (lower.contains('landed in pr') ||
      lower.contains('other pr') ||
      lower.contains('did not agree to this deferred')) {
    adj += 0.35;
  }

  if (wordCount >= 5 && wordCount <= 32) {
    adj += 0.20;
  } else if (wordCount > 65) {
    adj -= 0.45;
  }

  if (RegExp(
    r'network blip|rate limit|if `gh release view` fails|malformed input|race condition',
    caseSensitive: false,
  ).hasMatch(trimmed)) {
    adj -= 0.55;
  }

  return adj;
}

/// Whether [lowerComment] exhibits a concrete persona craft/readability,
/// naming, collection-idiom, or test-clarity marker (typically emitted by
/// Stage 2 as `minor` or `nit`).
bool hasPersonaSignatureMarker(String lowerComment) {
  if (lowerComment.contains('pronoun') ||
      lowerComment.contains('two-space') ||
      lowerComment.contains('two spaces') ||
      lowerComment.contains('empty string') ||
      lowerComment.contains("empty `''`") ||
      lowerComment.contains('already have a') ||
      lowerComment.contains('someplace else in the codebase') ||
      lowerComment.contains('change detector') ||
      lowerComment.contains('reusable workflow') ||
      lowerComment.contains('why a new file') ||
      lowerComment.contains('in a new test file') ||
      lowerComment.contains('why duplicate the') ||
      lowerComment.contains('duplicate the false default') ||
      lowerComment.contains('inline it') ||
      lowerComment.contains('inlining the') ||
      lowerComment.contains('helper method') ||
      lowerComment.contains('easier to read') ||
      lowerComment.contains('easier to follow') ||
      lowerComment.contains('hard to read') ||
      lowerComment.contains('type safe') ||
      lowerComment.contains('typesafe') ||
      lowerComment.contains('multi-line string') ||
      lowerComment.contains('```suggestion') ||
      lowerComment.contains('->') ||
      lowerComment.contains('s/') ||
      lowerComment.contains("don't start any dartdoc with") ||
      lowerComment.contains('start with `returns`') ||
      lowerComment.contains('single sentence') ||
      lowerComment.contains('single-pass') ||
      lowerComment.contains('.tolist()') ||
      lowerComment.contains('.toset()') ||
      lowerComment.contains('here and below:') ||
      lowerComment.contains('here and elsewhere:') ||
      lowerComment.contains('nitpick:')) {
    return true;
  }
  if (lowerComment.contains('.yaml') &&
      lowerComment.contains('.yml') &&
      (lowerComment.contains('standardize') ||
          lowerComment.contains('mix') ||
          lowerComment.contains('both') ||
          lowerComment.contains('match the') ||
          lowerComment.contains('all .yaml') ||
          lowerComment.contains('all `.yaml`') ||
          lowerComment.contains('all yaml'))) {
    return true;
  }
  if (lowerComment.contains('comment') &&
      (lowerComment.contains('dry run') ||
          lowerComment.contains('deleted') ||
          lowerComment.contains('empty quote') ||
          lowerComment.contains("'bottom'"))) {
    return true;
  }
  return false;
}

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
  final moveAdj = personaMoveAdjustment(c.comment);
  final confAdj = (c.confidence - 0.75) * 0.3;
  final candAdj = c.candidateIndex == 1 ? 0.08 : 0.0;
  return base + (typeW + moveAdj + confAdj + candAdj).clamp(-0.85, 1.45);
}

/// Craft category for stratified slot diversity so two pronoun comments do not
/// crowd out a constant-inlining, rewrite, or test-comment catch.
String _craftCategory(String lowerComment) {
  if (lowerComment.contains('pronoun')) return 'pronoun';
  if (lowerComment.contains('two-space') ||
      lowerComment.contains('two spaces') ||
      lowerComment.contains('inline it') ||
      lowerComment.contains('inlining the')) {
    return 'inline-constant';
  }
  if (lowerComment.contains('.yaml') && lowerComment.contains('.yml')) {
    return 'yaml-ext';
  }
  if (lowerComment.contains('comment') &&
      (lowerComment.contains('dry run') ||
          lowerComment.contains('deleted') ||
          lowerComment.contains('empty quote') ||
          lowerComment.contains("'bottom'"))) {
    return 'test-comment';
  }
  if (lowerComment.contains('why duplicate') ||
      lowerComment.contains('false default')) {
    return 'default-dedup';
  }
  if (lowerComment.contains('```suggestion') ||
      lowerComment.contains('->') ||
      lowerComment.contains('s/')) {
    return 'concrete-rewrite';
  }
  if (lowerComment.contains('dartdoc') ||
      lowerComment.contains('returns') ||
      lowerComment.contains('single sentence')) {
    return 'dartdoc-precision';
  }
  if (lowerComment.contains('.tolist()') ||
      lowerComment.contains('.toset()') ||
      lowerComment.contains('single-pass')) {
    return 'collection-idiom';
  }
  if (lowerComment.contains('helper method') ||
      lowerComment.contains('easier to read') ||
      lowerComment.contains('easier to follow') ||
      lowerComment.contains('hard to read')) {
    return 'readability-helper';
  }
  return 'other-craft';
}

/// Whether [c] belongs to the persona craft/readability/test-clarity stratum
/// (`minor` or `nit` comment with a concrete signature marker or
/// readability/test-assertion lens).
bool _isCraftPersonaCandidate(CriticComment c) {
  if (c.comment == 'Same here.') return false;
  if (c.severityRank >= severities.indexOf('major')) return false;
  final lower = c.comment.toLowerCase();
  return hasPersonaSignatureMarker(lower) ||
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

  final craftPool = <CriticComment>[];
  final craftFiles = <String>{};
  final craftCategories = <String>{};
  for (final c in deduped) {
    final cat = _craftCategory(c.comment.toLowerCase());
    if (_isCraftPersonaCandidate(c) &&
        compositeScore(c, personSlug: personSlug) >= 2.0 &&
        craftFiles.add(c.path) &&
        craftCategories.add(cat)) {
      craftPool.add(c);
    }
  }
  if (craftPool.length < maxCraftQuota) {
    for (final c in deduped) {
      if (craftPool.length >= maxCraftQuota) break;
      if (craftPool.contains(c)) continue;
      final cat = _craftCategory(c.comment.toLowerCase());
      if (_isCraftPersonaCandidate(c) &&
          compositeScore(c, personSlug: personSlug) >= 2.0 &&
          craftCategories.add(cat)) {
        craftPool.add(c);
      }
    }
  }
  if (craftPool.isEmpty) {
    return deduped.take(k).toList();
  }

  final reservedCraft = craftPool.take(maxCraftQuota).toList();
  final structuralQuota = k - reservedCraft.length;
  final selected = <CriticComment>[];

  var sameHereCount = 0;
  for (final c in deduped) {
    if (selected.length >= structuralQuota) break;
    if (reservedCraft.contains(c)) continue;
    if (c.comment == 'Same here.') {
      if (sameHereCount >= 1) continue;
      sameHereCount++;
    }
    selected.add(c);
  }

  for (final c in reservedCraft) {
    if (selected.length < k && !selected.contains(c)) {
      selected.add(c);
    }
  }

  for (final c in deduped) {
    if (selected.length >= k) break;
    if (!selected.contains(c)) {
      selected.add(c);
    }
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
  final primaries = <CriticComment>[];
  final repeats = <CriticComment, List<CriticComment>>{};
  for (final c in ranked) {
    if (primaries.any(
      (o) => o.path == c.path && (o.line - c.line).abs() <= lineWindow,
    )) {
      continue;
    }
    final twin = primaries.where((o) {
      if (_sharesSameFileSubject(o, c)) {
        final cx = _contentWords(o.comment);
        final cy = _contentWords(c.comment);
        if (cx.intersection(cy).length >= 3) return true;
      }
      final sim = textSimilarity(o.comment, c.comment);
      final threshold = o.path == c.path ? similarity * 0.85 : similarity;
      return sim >= threshold;
    }).firstOrNull;
    if (twin != null) {
      if (_sharesSameFileSubject(twin, c)) {
        continue;
      }
      final list = repeats[twin] ??= [];
      if (!list.any((r) => r.path == c.path)) {
        list.add(c.withComment('Same here.'));
      }
      continue;
    }
    primaries.add(c);
  }
  final deduped = [
    for (final c in primaries) ...[c, ...?repeats[c]],
  ];
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
  final hasBlockingScopeCreep = kept.any((c) {
    final lower = c.comment.toLowerCase();
    return c.severityRank >= severities.indexOf('major') &&
        (lower.contains('landed in pr') ||
            lower.contains('other pr') ||
            lower.contains('did not agree to this deferred') ||
            lower.contains('new test file') ||
            lower.contains('why a new file'));
  });
  return RankedReview(
    deduped: deduped,
    comments: kept,
    verdict: reviewVerdict(
      passIndex: passIndex,
      maxSeverity: max,
      majorCount: majorCount,
      hasBlockingScopeCreep: hasBlockingScopeCreep,
    ),
    maxSeverity: max,
  );
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
