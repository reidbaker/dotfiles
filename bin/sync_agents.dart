#!/usr/bin/env dart

import 'dart:io';

import 'package:args/args.dart';
import 'package:dotfiles/agent_syncer.dart';
import 'package:dotfiles/machine_selector.dart';
import 'package:path/path.dart' as p;

const List<String> _defaultExternalRoots = [
  '~/Documents/ai-sandbox/scratch/ai-team/skills',
  '~/.gemini/config/external_skills',
];

ArgParser _buildParser() {
  return ArgParser()
    ..addFlag('help', abbr: 'h', negatable: false, help: 'Show this help.')
    ..addFlag(
      'dry-run',
      abbr: 'n',
      negatable: false,
      help: 'Preview changes without modifying filesystem.',
    )
    ..addFlag(
      'quiet',
      abbr: 'q',
      negatable: false,
      help: 'Suppress non-error output.',
    )
    ..addOption('dotfiles-dir', help: 'Path to dotfiles directory.')
    ..addOption(
      'home-dir',
      help: 'Path to user home directory.',
      defaultsTo: Platform.environment['HOME'],
    )
    ..addOption(
      'machine',
      help: 'Override detected machine tag(s), comma-separated (e.g. mac,cloudtop).',
      defaultsTo: Platform.environment['AGENT_SYNC_MACHINE'],
    )
    ..addMultiOption(
      'external-root',
      help: 'Additional skill root directory with an optional skills_config.json.',
      defaultsTo: _defaultExternalRoots,
    );
}

void main(List<String> arguments) {
  final parser = _buildParser();
  final ArgResults results;
  try {
    results = parser.parse(arguments);
  } catch (e) {
    stderr.writeln('Error parsing arguments: $e\n');
    stderr.writeln(parser.usage);
    exit(1);
  }

  if (results['help'] as bool) {
    print('Sync agent skills, rules, and personas into ~/.agents and ~/.gemini\n');
    print('Usage: dart run bin/sync_agents.dart [options]\n');
    print(parser.usage);
    exit(0);
  }

  final homeDir = results['home-dir'] as String?;
  if (homeDir == null || homeDir.isEmpty) {
    stderr.writeln('Error: Could not determine HOME directory.');
    exit(1);
  }

  final dotfilesDir = _resolveDotfilesDir(results['dotfiles-dir'] as String?);
  final selector = MachineSelector.detect(
    isMacOS: Platform.isMacOS,
    isLinux: Platform.isLinux,
    overrideTag: results['machine'] as String?,
  );

  final syncer = AgentSyncer(
    dotfilesDir: dotfilesDir,
    homeDir: homeDir,
    dryRun: results['dry-run'] as bool,
    logger: (results['quiet'] as bool) ? null : print,
    machineSelector: selector,
    externalSkillRoots: results['external-root'] as List<String>,
  );

  final result = syncer.syncAll();
  if (!(results['quiet'] as bool)) {
    print('\nSummary: ${result.totalLinked} link(s) created/updated.');
  }
}

String _resolveDotfilesDir(String? rawDir) {
  if (rawDir != null) {
    return p.canonicalize(rawDir);
  }
  final scriptPath = Platform.script.toFilePath();
  return p.canonicalize(p.join(p.dirname(scriptPath), '..'));
}
