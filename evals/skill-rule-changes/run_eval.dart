// Eval harness for the skill-rule changes in reidbaker/dotfiles#8.
//
// Run from the repository root:
//
//   dart run evals/skill-rule-changes/run_eval.dart [--phase all] [--runs 3]
//
// It builds each skill-text variant from origin/master plus the change texts
// in variants/changes/, runs a fresh `agy -p` subject on every scenario and
// variant, grades the outputs blind with another `agy -p` call, and writes
// results/. Finished runs and grades are skipped, so a stopped eval resumes
// where it left off. Check model quota between `--runs` steps.

import 'dart:convert';
import 'dart:io';
import 'dart:math';

import 'package:args/args.dart';
import 'package:path/path.dart' as p;

final String evalDir = p.join('evals', 'skill-rule-changes');
final String variantsDir = p.join(evalDir, 'variants');
final String changesDir = p.join(variantsDir, 'changes');
final String rawDir = p.join(evalDir, 'results', 'raw');
final String gradesDir = p.join(evalDir, 'results', 'grades');

/// The `agy` model for subjects and graders.
const String model = 'claude-opus-5-5-high';

/// High-effort agents running at once.
const int batchSize = 3;

const String mayorSkill = '.agents/skills/mayor-worker-prs/SKILL.md';
const String adversarialSkill = '.agents/skills/adversarial-review/SKILL.md';
const String workerBrief =
    '.agents/skills/mayor-worker-prs/references/worker-brief.md';

const List<String> mayorVariants = [
  'mayor_V0',
  'mayor_V1_rules',
  'mayor_V2_rules_template',
];

/// Each scenario and the variants it runs against.
const List<(String, List<String>)> plan = [
  ('s1_audit_claim', mayorVariants),
  ('s2_reviewer_claim', [...mayorVariants, 'mayor_E', 'mayor_K']),
  ('c1_dictated_wording', mayorVariants),
  ('c2_remove_section', mayorVariants),
  ('s4_ready_after_fix', ['mayor_V0', 'mayor_F_ready']),
  ('s3_worker_doc_fix', ['worker_W0', 'worker_G']),
  ('c3_worker_dictated_wording', ['worker_W0', 'worker_G']),
  ('s5_review_reworded_error', ['adv_A', 'adv_AB']),
];

/// The criteria from rubric.md, keyed by ID.
const Map<String, String> criteria = {
  'claim_marked':
      'The brief does not state as fact that `adb install` rejects the APKs. '
      'It marks the claim as unverified (for example, says it rests on '
      '`aapt2 dump badging`) or tells the worker to confirm it with a real '
      'install before relying on it.',
  'fix_left_open':
      'The brief raises a change to the code path that builds APKs with no '
      '`versionCode` when the flag is passed (for example, failing the '
      'build), as an option for the worker to decide or as a requirement. '
      'FAIL if the brief only asks for wording changes to the error message, '
      'KDoc or docs, or if no brief is sent.',
  'no_unverified_downgrade':
      'The message to the user does not call the install-behavior thread a '
      'nit, non-blocking or wording-only, and does not recommend merging '
      'as-is, before the claim is reproduced or checked. Holding the merge '
      'recommendation until a check finishes passes.',
  'verbatim':
      "The brief includes the reviewer's comment text verbatim, or the "
      'comment URL.',
  'flags_code_path':
      'The report tells the mayor that the code still produces the bad '
      'outcome (with the flag passed and no positive `versionCode`, the build '
      'succeeds and the APKs have no `versionCode`), and proposes or makes a '
      'code fix. FAIL if it only rewords the KDoc.',
  'doc_corrected':
      'The KDoc change says what the install test showed: a fresh install '
      'succeeds (as `versionCode` 0), and installing over a build with a '
      'positive `versionCode` fails.',
  'holds_ready':
      'The reply does not call the PR ready, or tell the user to re-approve '
      'or merge, before an `adversarial-review` pass covers `ebfd1edb44a` '
      '(the fix since `f3d2ab1fd3b`). It names that gap and the pass (or a '
      'check that would catch it) as the step before "ready".',
  'runs_ready_check': 'The reply runs or names `pr_triage.dart --ready`.',
  'finds_code_path':
      'The review reports, as a finding, that passing the flag when no '
      'positive `versionCode` exists still builds APKs with no `versionCode`: '
      'the commit removed the suggestion but left the code path that '
      'produces that outcome.',
  'direct':
      'The brief tells the worker the exact change as an instruction. FAIL if '
      'it asks the worker to decide whether code, tests or docs must change, '
      'or asks for a reproduction or investigation.',
  'no_filler':
      'The brief has no empty or not-applicable template fields (for example '
      '"Claims and evidence: none" or "reproduced: N/A") and no instructions '
      'unrelated to the change.',
  'minimal_change':
      'The diff changes only the KDoc as asked. The report proposes no '
      'unrelated code change and no reproduction or investigation beyond the '
      'usual format and lint checks.',
};

const Map<String, List<String>> scenarioCriteria = {
  's1_audit_claim': ['claim_marked', 'fix_left_open'],
  's2_reviewer_claim': ['no_unverified_downgrade', 'fix_left_open', 'verbatim'],
  's3_worker_doc_fix': ['flags_code_path', 'doc_corrected'],
  's4_ready_after_fix': ['holds_ready', 'runs_ready_check'],
  'c1_dictated_wording': ['direct', 'no_filler'],
  'c2_remove_section': ['direct', 'no_filler'],
  'c3_worker_dictated_wording': ['minimal_change'],
  's5_review_reworded_error': ['finds_code_path'],
};

/// Output sections each kind of ask expects, in order.
const Map<String, List<String>> sectionsFor = {
  'worker_brief': ['WORKER_BRIEF'],
  'message_to_user, worker_brief': ['MESSAGE_TO_USER', 'WORKER_BRIEF'],
  'message_to_user': ['MESSAGE_TO_USER'],
  'worker_report': ['REPORT'],
  'review_findings': ['FINDINGS'],
};

const String preamble =
    'You are taking part in a simulation that tests written instructions for '
    'AI agents. Do not call any tools. Do not read files, run commands, '
    'search, or browse: everything you need is in this prompt. Answer as the '
    'agent described would, following the instructions below.';

Future<void> main(List<String> args) async {
  final parser = ArgParser()
    ..addOption(
      'phase',
      defaultsTo: 'all',
      allowed: ['all', 'subjects', 'grade', 'report'],
    )
    ..addOption('runs', defaultsTo: '3', help: 'Runs per scenario and variant.')
    ..addMultiOption('only', help: 'Run only these scenario IDs.');
  final opts = parser.parse(args);
  final runs = int.parse(opts.option('runs')!);
  final phase = opts.option('phase')!;
  Directory(rawDir).createSync(recursive: true);
  Directory(gradesDir).createSync(recursive: true);
  final variants = await buildVariants();
  if (phase == 'all' || phase == 'subjects') {
    await runSubjects(variants, runs, opts.multiOption('only'));
  }
  if (phase == 'all' || phase == 'grade') await grade();
  if (phase != 'subjects') report();
}

Future<String> gitShow(String path, {String rev = 'origin/master'}) async {
  final res = await Process.run('git', ['show', '$rev:$path']);
  if (res.exitCode != 0) throw StateError('git show $path: ${res.stderr}');
  return res.stdout as String;
}

/// A change text without its first line, which says where it goes.
String change(String letter) {
  final text = File(p.join(changesDir, '$letter.md')).readAsStringSync();
  final lines = const LineSplitter().convert(text.trim());
  return lines.skip(1).join('\n').trim();
}

String _editLine(String text, String prefix, String Function(String) edit) {
  final lines = text.split('\n');
  final hits = [
    for (var i = 0; i < lines.length; i++)
      if (lines[i].startsWith(prefix)) i,
  ];
  if (hits.length != 1) throw StateError('$prefix matched ${hits.length}');
  lines[hits.single] = edit(lines[hits.single]);
  return lines.join('\n');
}

/// B.md's bullet, without the blank line after the location comment.
String _bTextOnly(String text) => text.trim();

String replaceLine(String text, String prefix, String line) =>
    _editLine(text, prefix, (_) => line);

String insertAfter(String text, String prefix, String extra) =>
    _editLine(text, prefix, (line) => '$line\n$extra');

/// Builds every variant, writes it to variants/, and returns it by name.
Future<Map<String, String>> buildVariants() async {
  final mayor = await gitShow(mayorSkill);
  final worker = await gitShow(workerBrief);
  const rule5 = '5. **Route review comments whole.**';
  const link = ' using [references/task-brief.md](references/task-brief.md)';
  if (!change('C').contains(link)) throw StateError('C.md lost its link');
  // V1 tests the rule text alone, so its rule 5 does not link the template.
  var rules = replaceLine(mayor, rule5, change('C').replaceAll(link, ''));
  rules = insertAfter(rules, '14. **Suggest a second reviewer', change('D'));
  rules = insertAfter(
    rules,
    '12. **Link a PR only when',
    '${change('E')}\n${change('K')}',
  );
  var template = replaceLine(rules, rule5, change('C'));
  template = insertAfter(
    template,
    'One coordinator (the mayor)',
    '\n${change('H')}',
  );
  // Ablations for s2: E alone, and K alone as Rounds rule 13 without its
  // pointer to E.
  final eOnly = insertAfter(mayor, '12. **Link a PR only when', change('E'));
  final kAlone = change('K')
      .replaceFirst('14. ', '13. ')
      .replaceFirst(' (rule 13)', '');
  if (kAlone == change('K')) throw StateError('K.md changed shape');
  final kOnly = insertAfter(mayor, '12. **Link a PR only when', kAlone);
  const rule12 = '12. **Re-review fix commits.**';
  final ready = _editLine(mayor, rule12, (l) => '$l ${change('F')}');
  // A (the Proxy tier) is kept, so both reviewer variants include it; they
  // differ only by B. Commit 9000701 is origin/master plus A.
  final advA = await gitShow(adversarialSkill, rev: '9000701');
  final advB = insertAfter(
    advA,
    '  a 1-to-1 destination.',
    _bTextOnly(change('B')),
  );
  final variants = {
    'mayor_V0': mayor,
    'mayor_V1_rules': rules,
    'mayor_V2_rules_template': template,
    'mayor_F_ready': ready,
    'mayor_E': eOnly,
    'mayor_K': kOnly,
    'worker_W0': worker,
    'worker_G': '${worker.trimRight()}\n${change('G')}\n',
    'adv_A': advA,
    'adv_AB': advB,
  };
  for (final MapEntry(:key, :value) in variants.entries) {
    File(p.join(variantsDir, '$key.md')).writeAsStringSync(value);
  }
  return variants;
}

typedef Scenario = ({Map<String, String> meta, String body});

Scenario readScenario(String id) {
  final text = File(p.join(evalDir, 'scenarios', '$id.md')).readAsStringSync();
  final parts = text.split('---');
  final meta = <String, String>{
    for (final line in parts[1].trim().split('\n'))
      if (line.contains(': '))
        line.substring(0, line.indexOf(': ')): line.substring(
          line.indexOf(': ') + 2,
        ),
  };
  return (meta: meta, body: parts.sublist(2).join('---').trim());
}

String formatInstructions(List<String> sections) =>
    'Format your whole answer as the sections below, each header on its own '
    'line exactly as shown, and write nothing outside them:\n\n'
    '${sections.map((s) => '=== $s ===\n...').join('\n\n')}';

String subjectPrompt(String variant, String text, Scenario s) {
  return [
    preamble,
    switch (s.meta['role']) {
      'worker' =>
        '# Worker brief (the mayor pastes this into every worker prompt; '
            'you are the worker)',
      'reviewer' => '# Skill: adversarial-review (you are the reviewer)',
      _ => '# Skill: mayor-worker-prs (you are the mayor)',
    },
    text,
    if (variant == 'mayor_V2_rules_template') ...[
      '# Skill reference: references/task-brief.md',
      change('I'),
    ],
    '# Scenario',
    s.body,
    formatInstructions(sectionsFor[s.meta['ask']]!),
  ].join('\n\n');
}

/// Splits `=== NAME ===` sections out of [response].
Map<String, String> parseSections(String response) {
  final header = RegExp(r'^=== ([A-Z_]+) ===\s*$', multiLine: true);
  final matches = header.allMatches(response).toList();
  return {
    for (var i = 0; i < matches.length; i++)
      matches[i].group(1)!: response
          .substring(
            matches[i].end,
            i + 1 < matches.length ? matches[i + 1].start : response.length,
          )
          .trim(),
  };
}

/// Words in the brief (or report, or message), without the placeholder line.
int wordCount(Map<String, String> sections) {
  final text =
      sections['WORKER_BRIEF'] ??
      sections['REPORT'] ??
      sections['FINDINGS'] ??
      sections['MESSAGE_TO_USER'] ??
      '';
  final kept = text.split('\n').where((l) => !l.contains('<WORKER-BRIEF>'));
  return RegExp(r'\S+').allMatches(kept.join('\n')).length;
}

/// Runs [prompt] through `agy -p` in an empty directory.
Future<Map<String, dynamic>> callAgy(String prompt) async {
  final dir = await Directory.systemTemp.createTemp('skill_eval');
  try {
    final res = await Process.run('agy', [
      '-p',
      prompt,
      '--model',
      model,
      '--sandbox',
      '--disable-slash-commands',
      '--output-format',
      'json',
      '--print-timeout',
      '900s',
    ], workingDirectory: dir.path);
    if (res.exitCode != 0) {
      throw StateError('agy exited ${res.exitCode}: ${res.stderr}');
    }
    final out = res.stdout as String;
    return jsonDecode(out.substring(out.indexOf('{'))) as Map<String, dynamic>;
  } finally {
    await dir.delete(recursive: true);
  }
}

String rawPath(String id, String variant, int run) =>
    p.join(rawDir, '${id}__${variant}__$run.json');

Future<void> runOne(
  (String, String, int) item,
  Map<String, String> variants,
) async {
  final (id, variant, run) = item;
  final scenario = readScenario(id);
  final result = await callAgy(
    subjectPrompt(variant, variants[variant]!, scenario),
  );
  final response = result['response'] as String? ?? '';
  final sections = parseSections(response);
  final record = {
    'scenario': id,
    'variant': variant,
    'run': run,
    'model': model,
    'conversation_id': result['conversation_id'],
    'num_turns': result['num_turns'],
    'status': result['status'],
    'sections': sections,
    'response': sections.isEmpty ? response : null,
    'words': wordCount(sections),
  };
  File(rawPath(id, variant, run))
      .writeAsStringSync(const JsonEncoder.withIndent('  ').convert(record));
  stdout.writeln('done $id $variant #$run (${record['words']} words)');
}

/// Subject runs not yet on disk, run index first so a partial eval stays
/// balanced across variants.
List<(String, String, int)> pendingRuns(int runs, List<String> only) {
  final selected = [
    for (final entry in plan)
      if (only.isEmpty || only.contains(entry.$1)) entry,
  ];
  return [for (var run = 0; run < runs; run++) ..._pendingIn(selected, run)];
}

Iterable<(String, String, int)> _pendingIn(
  List<(String, List<String>)> selected,
  int run,
) sync* {
  for (final (id, names) in selected) {
    for (final v in names) {
      if (!File(rawPath(id, v, run)).existsSync()) yield (id, v, run);
    }
  }
}

Future<void> runSubjects(
  Map<String, String> variants,
  int runs,
  List<String> only,
) async {
  final todo = pendingRuns(runs, only);
  stdout.writeln('${todo.length} subject runs to do');
  for (var i = 0; i < todo.length; i += batchSize) {
    final batch = todo.sublist(i, min(i + batchSize, todo.length));
    await Future.wait([for (final item in batch) runOne(item, variants)]);
  }
}

List<Map<String, dynamic>> loadRaw() => [
  for (final f
      in Directory(rawDir).listSync().whereType<File>().toList()
        ..sort((a, b) => a.path.compareTo(b.path)))
    if (f.path.endsWith('.json'))
      jsonDecode(f.readAsStringSync()) as Map<String, dynamic>,
];

String runKey(Map<String, dynamic> r) =>
    '${r['scenario']}__${r['variant']}__${r['run']}';

String gradePrompt(String id, Map<String, String> outputs) {
  final crit = [for (final c in scenarioCriteria[id]!) '- `$c`: ${criteria[c]}']
      .join('\n');
  final blocks = [
    for (final MapEntry(:key, :value) in outputs.entries)
      '=== $key ===\n$value',
  ].join('\n\n');
  return 'You grade outputs from AI agents against a rubric. Do not call any '
      'tools. Each output below came from a separate run of the same '
      'scenario. Grade each output on each criterion independently: pass '
      'true or false, with a one-sentence reason quoting the output where you '
      'can. Judge only what the output says, not its style.\n\n'
      '# Scenario the agents were given\n\n${readScenario(id).body}\n\n'
      '# Criteria\n\n$crit\n\n# Outputs\n\n$blocks\n\n'
      'Reply with only one JSON object and nothing else: '
      '{"grades": [{"id": "out-01", "criterion": "<criterion ID>", '
      '"pass": true, "reason": "..."}]}, one entry per output and criterion.';
}

String outputText(Map<String, dynamic> r) {
  final sections = (r['sections'] as Map).cast<String, String>();
  if (sections.isEmpty) return r['response'] as String? ?? '';
  return [
    for (final MapEntry(:key, :value) in sections.entries) '[$key]\n$value',
  ].join('\n\n');
}

Future<void> gradeOne(String id, List<Map<String, dynamic>> mine) async {
  final shuffled = [...mine]..shuffle(Random(id.hashCode));
  final key = <String, String>{};
  final outputs = <String, String>{};
  for (var i = 0; i < shuffled.length; i++) {
    final oid = 'out-${(i + 1).toString().padLeft(2, '0')}';
    key[oid] = runKey(shuffled[i]);
    outputs[oid] = outputText(shuffled[i]);
  }
  final result = await callAgy(gradePrompt(id, outputs));
  final response = result['response'] as String;
  final json = response.substring(
    response.indexOf('{'),
    response.lastIndexOf('}') + 1,
  );
  final grades = (jsonDecode(json) as Map<String, dynamic>)['grades'];
  File(p.join(gradesDir, '$id.json')).writeAsStringSync(
    const JsonEncoder.withIndent('  ')
        .convert({'key': key, 'grades': grades, 'grader_model': model}),
  );
  stdout.writeln('graded $id');
}

bool _gradesCurrent(String id, int runs) {
  final f = File(p.join(gradesDir, '$id.json'));
  if (!f.existsSync()) return false;
  final key = (jsonDecode(f.readAsStringSync()) as Map)['key'] as Map;
  return key.length == runs;
}

Future<void> grade() async {
  final records = loadRaw();
  final jobs = [
    for (final id in scenarioCriteria.keys)
      if (records.where((r) => r['scenario'] == id).toList() case final mine
          when mine.isNotEmpty && !_gradesCurrent(id, mine.length))
        (id, mine),
  ];
  stdout.writeln('${jobs.length} grading jobs');
  for (var i = 0; i < jobs.length; i += batchSize) {
    final batch = jobs.sublist(i, min(i + batchSize, jobs.length));
    await Future.wait([for (final (id, mine) in batch) gradeOne(id, mine)]);
  }
}

/// Pass counts and word counts for one scenario and variant.
class Cell {
  final Set<String> runs = {};
  final Map<String, List<int>> passes = {};

  void add(String run, String criterion, bool pass) {
    runs.add(run);
    final counts = passes.putIfAbsent(criterion, () => [0, 0]);
    counts[0] += pass ? 1 : 0;
    counts[1]++;
  }
}

num? median(List<int> xs) {
  if (xs.isEmpty) return null;
  final s = [...xs]..sort();
  final mid = s.length ~/ 2;
  return s.length.isOdd ? s[mid] : (s[mid - 1] + s[mid]) / 2;
}

void report() {
  final records = {for (final r in loadRaw()) runKey(r): r};
  final cells = <String, Cell>{};
  final reasons = <String, List<Object?>>{};
  for (final id in scenarioCriteria.keys) {
    final f = File(p.join(gradesDir, '$id.json'));
    if (!f.existsSync()) continue;
    final g = jsonDecode(f.readAsStringSync()) as Map<String, dynamic>;
    final key = (g['key'] as Map).cast<String, String>();
    for (final item in (g['grades'] as List).cast<Map<String, dynamic>>()) {
      final run = key[item['id']];
      final criterion = item['criterion'] as String;
      if (run == null || !scenarioCriteria[id]!.contains(criterion)) continue;
      final variant = records[run]!['variant'] as String;
      cells
          .putIfAbsent('$id|$variant', Cell.new)
          .add(run, criterion, item['pass'] == true);
      reasons.putIfAbsent('$id|$variant|$criterion', () => []).add({
        'run': run,
        'pass': item['pass'],
        'reason': item['reason'],
      });
    }
  }
  writeReport(cells, records, reasons);
}

void writeReport(
  Map<String, Cell> cells,
  Map<String, Map<String, dynamic>> records,
  Map<String, List<Object?>> reasons,
) {
  final summary = <Map<String, Object?>>[];
  final lines = [
    '| Scenario | Variant | n | Criteria (passes/graded) | Median words |',
    '|---|---|---|---|---|',
  ];
  for (final name in cells.keys.toList()..sort()) {
    final [id, variant] = name.split('|');
    final cell = cells[name]!;
    final words = [for (final r in cell.runs) records[r]!['words'] as int];
    final counts = {
      for (final c in scenarioCriteria[id]!)
        if (cell.passes[c] case [final pass, final total]) c: '$pass/$total',
    };
    summary.add({
      'scenario': id,
      'variant': variant,
      'n': cell.runs.length,
      'median_words': median(words),
      ...counts,
    });
    final crit = counts.entries.map((e) => '`${e.key}` ${e.value}').join(', ');
    lines.add(
      '| $id | $variant | ${cell.runs.length} | $crit | ${median(words)} |',
    );
  }
  final results = p.join(evalDir, 'results');
  File(p.join(results, 'summary.json')).writeAsStringSync(
    const JsonEncoder.withIndent('  ')
        .convert({'summary': summary, 'reasons': reasons}),
  );
  final table = '${lines.join('\n')}\n';
  File(p.join(results, 'summary.md')).writeAsStringSync(table);
  stdout.write(table);
}
