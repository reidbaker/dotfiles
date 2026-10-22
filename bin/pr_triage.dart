import 'dart:convert';
import 'dart:io';

import 'package:args/args.dart';
import 'package:dotfiles/src/pr_triage/github_fetcher.dart';
import 'package:dotfiles/src/pr_triage/thread_scanner.dart';
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
  final parser = _buildParser();
  final opts = _parseArgs(parser, args);
  if (opts == null) return;
  if (opts.flag('threads')) {
    exitCode = await runThreads(opts);
    return;
  }

  final limit = _parseInt(opts.option('limit'), 'limit');
  final top = _parseInt(opts.option('top'), 'top') ?? 3;
  final prSpecs = _parsePrSpecs(opts.multiOption('pr'));
  if ((limit == null && opts.option('limit') != null) || prSpecs == null) {
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
      prSpecs: prSpecs,
    );
    _writeOutput(result);
  } catch (e, st) {
    stderr.writeln('Error running PR triage: $e');
    stderr.writeln(st);
    exitCode = 1;
  }
}

ArgParser _buildParser() => ArgParser()
  ..addOption('config', abbr: 'c', help: 'Path to config.yaml.')
  ..addOption('limit', abbr: 'l', help: 'Max PRs to fetch per query (1-100).')
  ..addOption('top', abbr: 't', defaultsTo: '3', help: 'Highlights per queue.')
  ..addMultiOption(
    'pr',
    help: 'Specific PR(s) to triage (owner/repo#N or GitHub PR URL).',
  )
  ..addFlag(
    'threads',
    negatable: false,
    help: 'List reviewer feedback the agent has not answered (needs --repo).',
  )
  ..addOption('repo', help: '--threads: repository as owner/name.')
  ..addOption(
    'agent',
    help: '--threads: login whose reply marks a thread done.',
  )
  ..addOption('since', help: '--threads: ISO time or duration (e.g. 6h).')
  ..addOption(
    'include-merged',
    defaultsTo: '2d',
    help: '--threads: include PRs merged within this duration.',
  )
  ..addMultiOption(
    'ignore-author',
    help: '--threads: login to treat like the agent (repeatable).',
  )
  ..addFlag('json', negatable: false, help: '--threads: print JSON.')
  ..addFlag('help', abbr: 'h', negatable: false, help: 'Show usage.');

ArgResults? _parseArgs(ArgParser parser, List<String> args) {
  final ArgResults opts;
  try {
    opts = parser.parse(args);
  } on FormatException catch (e) {
    stderr.writeln(e.message);
    stderr.writeln(parser.usage);
    exitCode = 64;
    return null;
  }
  if (opts.flag('help')) {
    stdout.writeln('Usage: dart run bin/pr_triage.dart [options]');
    stdout.writeln(parser.usage);
    return null;
  }
  return opts;
}

void _writeOutput(TriageResult result) {
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
}

List<PrSpec>? _parsePrSpecs(List<String> rawSpecs) {
  final specs = <PrSpec>[];
  for (final raw in rawSpecs) {
    try {
      specs.add(PrSpec.parse(raw));
    } on FormatException catch (e) {
      stderr.writeln(e.message);
      return null;
    }
  }
  return specs;
}

int? _parseInt(String? raw, String name) {
  if (raw == null) return null;
  final value = int.tryParse(raw);
  if (value == null) {
    stderr.writeln('--$name must be an integer, got "$raw".');
  }
  return value;
}

/// Locates `config.yaml` by walking up from the current directory, falling
/// back to `$HOME/.agents/skills/pr-triage/resources/config.yaml` when invoked
/// from outside the home directory tree.
String? _findConfig(LocalFileSystem fs) {
  var dir = fs.currentDirectory.path;
  while (true) {
    final candidate = p.joinAll([dir, ..._configSegments]);
    if (fs.file(candidate).existsSync()) return candidate;
    final parent = p.dirname(dir);
    if (parent == dir) break;
    dir = parent;
  }
  final home = Platform.environment['HOME'];
  if (home != null && home.isNotEmpty) {
    final fallback = p.joinAll([home, ..._configSegments]);
    if (fs.file(fallback).existsSync()) return fallback;
  }
  return null;
}

/// Runs `--threads` mode and returns the process exit code.
Future<int> runThreads(ArgResults opts, {ThreadScanner? scanner}) async {
  final options = _threadOptions(opts, DateTime.now());
  if (options == null) return 64;
  final result = await (scanner ?? ThreadScanner()).scan(options);
  if (opts.flag('json')) {
    stdout.writeln(const JsonEncoder.withIndent('  ').convert(result.toJson()));
  } else {
    stdout.write(formatThreadReport(result));
  }
  for (final failure in result.failures) {
    stderr.writeln('error: fetch failed, report is incomplete: $failure');
  }
  return result.failures.isEmpty ? 0 : 1;
}

ThreadScanOptions? _threadOptions(ArgResults opts, DateTime now) {
  final repo = opts.option('repo');
  if (repo == null || !RegExp(r'^[\w.-]+/[\w.-]+$').hasMatch(repo)) {
    stderr.writeln('--threads requires --repo owner/name.');
    return null;
  }
  final merged = parseDuration(opts.option('include-merged')!);
  if (merged == null) {
    stderr.writeln('--include-merged must be a duration such as 2d or 12h.');
    return null;
  }
  final rawSince = opts.option('since');
  DateTime? since;
  if (rawSince != null) {
    final d = parseDuration(rawSince);
    since = d != null ? now.subtract(d) : DateTime.tryParse(rawSince);
    if (since == null) {
      stderr.writeln('--since must be an ISO time or a duration such as 6h.');
      return null;
    }
  }
  return ThreadScanOptions(
    repo: repo,
    agent: opts.option('agent'),
    since: since,
    includeMerged: merged,
    ignoreAuthors: opts.multiOption('ignore-author').toSet(),
  );
}
