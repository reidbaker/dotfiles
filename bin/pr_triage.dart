import 'dart:convert';
import 'dart:io';

import 'package:args/args.dart';
import 'package:dotfiles/src/pr_triage/triage_engine.dart';
import 'package:file/local.dart';
import 'package:path/path.dart' as p;

/// Default location of the skill's config, relative to the repository root.
const List<String> _configSegments = [
  '.agents',
  'skills',
  'pr-triage',
  'resources',
  'config.yaml',
];

Future<void> main(List<String> args) async {
  final parser = ArgParser()
    ..addOption('config', abbr: 'c', help: 'Path to config.yaml.')
    ..addOption('limit',
        abbr: 'l', help: 'Max PRs to fetch per query (1-100).')
    ..addOption('top',
        abbr: 't', defaultsTo: '3', help: 'Highlights per queue.')
    ..addFlag('help', abbr: 'h', negatable: false, help: 'Show usage.');

  final ArgResults opts;
  try {
    opts = parser.parse(args);
  } on FormatException catch (e) {
    stderr.writeln(e.message);
    stderr.writeln(parser.usage);
    exitCode = 64;
    return;
  }

  if (opts.flag('help')) {
    stdout.writeln('Usage: dart run bin/pr_triage.dart [options]');
    stdout.writeln(parser.usage);
    return;
  }

  final limit = _parseInt(opts.option('limit'), 'limit');
  final top = _parseInt(opts.option('top'), 'top') ?? 3;
  if (limit == null && opts.option('limit') != null) {
    exitCode = 64;
    return;
  }

  const fs = LocalFileSystem();
  final configPath = opts.option('config') ?? _findConfig(fs);

  try {
    final result = await TriageEngine(fs: fs).runTriage(
      configPath: configPath,
      topCount: top,
      limitOverride: limit,
    );

    for (final warning in result.warnings) {
      stderr.writeln('warning: $warning');
    }
    if (result.truncated) {
      stderr.writeln(
        'warning: GitHub reported more matches than --limit allowed; '
        'the queues below are incomplete.',
      );
    }

    stdout.writeln(const JsonEncoder.withIndent('  ').convert(result.toJson()));
  } catch (e, st) {
    stderr.writeln('Error running PR triage: $e');
    stderr.writeln(st);
    exitCode = 1;
  }
}

int? _parseInt(String? raw, String name) {
  if (raw == null) return null;
  final value = int.tryParse(raw);
  if (value == null) {
    stderr.writeln('--$name must be an integer, got "$raw".');
    return null;
  }
  return value;
}

/// Locates `config.yaml` by walking up from the current directory.
///
/// `Platform.script` points into the pub cache or a snapshot path once the
/// tool is compiled or run via a global activation, so it cannot be used to
/// find repository-relative resources.
String? _findConfig(LocalFileSystem fs) {
  var dir = fs.currentDirectory.path;
  while (true) {
    final candidate = p.joinAll([dir, ..._configSegments]);
    if (fs.file(candidate).existsSync()) return candidate;
    final parent = p.dirname(dir);
    if (parent == dir) return null;
    dir = parent;
  }
}
