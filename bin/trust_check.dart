import 'dart:convert';
import 'dart:io';

import 'package:args/args.dart';
import 'package:dotfiles/src/trust_check/trust_checker.dart';
import 'package:dotfiles/src/trust_check/trust_config.dart';
import 'package:file/local.dart';
import 'package:path/path.dart' as p;

/// Default location of the config, relative to the repository root.
const List<String> _configSegments = [
  '.agents',
  'skills',
  'adversarial-review',
  'resources',
  'config.yaml',
];

/// Exit codes. Anything other than [_exitTrusted] means "run the security
/// pass before executing the change".
const _exitTrusted = 0;
const _exitUntrusted = 1;
const _exitError = 2;
const _exitUsage = 64;

Future<void> main(List<String> args) async {
  final parser = ArgParser()
    ..addOption('repo', abbr: 'r', help: 'Repository as owner/name.')
    ..addOption('pr', abbr: 'p', help: 'Pull request number.')
    ..addOption('config', abbr: 'c', help: 'Path to config.yaml.')
    ..addFlag('json', negatable: false, help: 'Print a JSON report.')
    ..addFlag('help', abbr: 'h', negatable: false, help: 'Show usage.');

  final ArgResults opts;
  try {
    opts = parser.parse(args);
  } on FormatException catch (e) {
    stderr
      ..writeln(e.message)
      ..writeln(parser.usage);
    exitCode = _exitUsage;
    return;
  }

  if (opts.flag('help')) {
    stdout
      ..writeln('Usage: dart run bin/trust_check.dart --repo owner/name --pr N')
      ..writeln()
      ..writeln('Exit 0: all trusted. 1: someone untrusted. 2: check failed.')
      ..writeln(parser.usage);
    return;
  }

  final repo = opts.option('repo');
  final number = int.tryParse(opts.option('pr') ?? '');
  if (repo == null || !repo.contains('/') || number == null) {
    stderr
      ..writeln('--repo owner/name and --pr <number> are required.')
      ..writeln(parser.usage);
    exitCode = _exitUsage;
    return;
  }

  final TrustConfig config;
  try {
    config = _loadConfig(opts.option('config'));
  } on Object catch (e) {
    stderr.writeln('error: invalid trust config: $e');
    exitCode = _exitError;
    return;
  }

  final TrustReport report;
  try {
    report = await TrustChecker().check(
      repo: repo,
      number: number,
      config: config,
    );
  } on Object catch (e) {
    stderr.writeln('error: $e (treat this change as untrusted)');
    exitCode = _exitError;
    return;
  }

  if (opts.flag('json')) {
    stdout.writeln(const JsonEncoder.withIndent('  ').convert(report.toJson()));
  } else {
    for (final v in report.verdicts) {
      stdout.writeln(
        '${v.trusted ? 'TRUSTED  ' : 'UNTRUSTED'} ${v.login} (${v.via})',
      );
    }
  }
  exitCode = report.allTrusted ? _exitTrusted : _exitUntrusted;
}

TrustConfig _loadConfig(String? explicit) {
  const fs = LocalFileSystem();
  final path = explicit ?? _findConfig(fs);
  if (path == null) {
    stderr.writeln(
      'warning: no ${p.joinAll(_configSegments)} found; using the defaults '
      '(${TrustConfig.kDefaultTrustedOrgs.join(', ')}, '
      '${TrustConfig.kDefaultTrustedTeams.join(', ')}).',
    );
    return const TrustConfig();
  }
  final file = fs.file(path);
  if (!file.existsSync()) {
    throw FileSystemException('config not found', path);
  }
  return TrustConfig.fromYamlString(file.readAsStringSync());
}

/// Locates the config by walking up from the current directory, the same way
/// `bin/pr_triage.dart` does.
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
