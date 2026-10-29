import 'dart:convert';
import 'dart:io';

import 'package:args/command_runner.dart';
import 'package:dotfiles/src/review_pipeline/review_pipeline.dart';
import 'package:path/path.dart' as p;

Future<void> main(List<String> args) async {
  final runner =
      CommandRunner<int>(
          'review_pipeline',
          'Deterministic stages of the Locus Detector -> Focused Critic -> '
              'Stratified Ranker persona review pipeline.',
        )
        ..addCommand(_LociPrepCommand())
        ..addCommand(_RankCommand());

  try {
    final code = await runner.run(args);
    exitCode = code ?? 0;
  } on UsageException catch (e) {
    stderr
      ..writeln(e.message)
      ..writeln(e.usage);
    exitCode = 64;
  }
}

final class _LociPrepCommand extends Command<int> {
  @override
  final String name = 'loci-prep';

  @override
  final String description =
      'Split a commit-exact diff into hunks with surrounding file context '
      'and write hunks.jsonl for the Stage 1 locus detector.';

  _LociPrepCommand() {
    argParser
      ..addOption('diff', mandatory: true, help: 'Unified diff file.')
      ..addOption('repo', help: 'Checkout at the reviewed commit.')
      ..addOption('context', defaultsTo: '50', help: 'Lines of context.')
      ..addOption('out', mandatory: true, help: 'Output directory.');
  }

  @override
  Future<int> run() async {
    final a = argResults!;
    final repo = a['repo'] as String?;
    final context = int.parse(a['context'] as String);
    final hunks = parseUnifiedDiff(
      File(a['diff'] as String).readAsStringSync(),
    );
    final out = Directory(a['out'] as String)..createSync(recursive: true);
    final records = [
      for (final h in hunks)
        h.toJson(
          context: repo == null
              ? null
              : fileContext(File(p.join(repo, h.path)), h, context),
        ),
    ];
    writeJsonl(File(p.join(out.path, 'hunks.jsonl')), records);
    final files = {for (final h in hunks) h.path}.length;
    stdout.writeln('hunks: ${hunks.length} across $files files');
    return 0;
  }
}

final class _RankCommand extends Command<int> {
  @override
  final String name = 'rank';

  @override
  final String description =
      'Stage 3: dedupe, rank, and cap Stage 2 critic comments to the '
      "persona's budget, then compute the review verdict.";

  _RankCommand() {
    argParser
      ..addOption(
        'person',
        abbr: 'p',
        mandatory: true,
        help:
            'Persona name or slug (reid, stuart, lasse, jake, david, codefu, '
            'loic, chris, paul).',
      )
      ..addOption('in', mandatory: true, help: 'Critic outputs (.jsonl).')
      ..addOption('out', mandatory: true, help: 'Review JSON to write.')
      ..addOption('k', help: 'Comment cap. Defaults to the persona budget.')
      ..addOption('pass-index', defaultsTo: '1')
      ..addOption(
        'author-relationship',
        defaultsTo: 'insider',
        allowed: const ['insider', 'external', 'agent', 'self'],
      );
  }

  @override
  Future<int> run() async {
    final a = argResults!;
    final persona = ReviewPersona.parse(a['person'] as String);
    final k = a['k'] == null ? persona.defaultCap : int.parse(a['k'] as String);
    final candidates = <CriticComment>[];
    var order = 0;
    for (final r in readJsonl(File(a['in'] as String))) {
      for (final c in CriticComment.parseRecord(r, order)) {
        candidates.add(c);
        order++;
      }
    }
    final review = rankReview(
      candidates,
      k: k,
      personSlug: persona.slug,
      passIndex: int.parse(a['pass-index'] as String),
      authorRelationship: a['author-relationship'] as String,
    );
    final file = File(a['out'] as String)..parent.createSync(recursive: true);
    file.writeAsStringSync(
      '${const JsonEncoder.withIndent('  ').convert(review.toJson())}\n',
    );
    stdout.writeln(
      'candidates: ${candidates.length}, after dedupe: '
      '${review.deduped.length}, kept: ${review.comments.length} (k=$k), '
      'verdict: ${review.verdict}',
    );
    return 0;
  }
}
