#!/usr/bin/env dart

import 'dart:io';
import 'package:args/args.dart';
import 'package:dotfiles/agent_syncer.dart';
import 'package:path/path.dart' as p;


void main(List<String> arguments) {
  final parser =
      ArgParser()
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
        );

  final ArgResults results;
  try {
    results = parser.parse(arguments);
  } catch (e) {
    stderr.writeln('Error parsing arguments: $e\n');
    stderr.writeln(parser.usage);
    exit(1);
  }

  if (results['help'] as bool) {
    print('Sync agent skills and personas into ~/.agents and .gemini/skills\n');
    print('Usage: dart run bin/sync_agents.dart [options]\n');
    print(parser.usage);
    exit(0);
  }

  final dryRun = results['dry-run'] as bool;
  final quiet = results['quiet'] as bool;

  final homeDir = results['home-dir'] as String?;
  if (homeDir == null || homeDir.isEmpty) {
    stderr.writeln('Error: Could not determine HOME directory.');
    exit(1);
  }

  final String dotfilesDir;
  if (results['dotfiles-dir'] != null) {
    dotfilesDir = p.canonicalize(results['dotfiles-dir'] as String);
  } else {
    // Infer root from script location: bin/sync_agents.dart -> repo root
    final scriptPath = Platform.script.toFilePath();
    dotfilesDir = p.canonicalize(p.join(p.dirname(scriptPath), '..'));
  }

  final syncer = AgentSyncer(
    dotfilesDir: dotfilesDir,
    homeDir: homeDir,
    dryRun: dryRun,
    logger: quiet ? null : print,
  );

  final result = syncer.syncAll();

  if (!quiet) {
    print('\nSummary: ${result.totalLinked} link(s) created/updated.');
  }
}
