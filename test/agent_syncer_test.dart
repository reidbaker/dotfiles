import 'package:dotfiles/agent_syncer.dart';
import 'package:dotfiles/sync_action.dart';
import 'package:file/file.dart';
import 'package:file/memory.dart';
import 'package:test/test.dart';

void main() {
  const dotfilesDir = '/users/testuser/dotfiles';
  const homeDir = '/users/testuser';

  late MemoryFileSystem fs;

  void createSkillFile(String path, String name) {
    fs.directory(path).createSync(recursive: true);
    fs.file('$path/SKILL.md').writeAsStringSync('---\nname: $name\n---\n');
  }

  void createCustomSkill(String name) {
    createSkillFile('$dotfilesDir/.agents/skills/$name', name);
  }

  void createThirdPartySkill(String repo, String name) {
    createSkillFile('$dotfilesDir/third_party/$repo/skills/$name', name);
    fs
        .file('$dotfilesDir/third_party/$repo/LICENSE')
        .writeAsStringSync('MIT License');
  }

  void createAgentPersona(String name) {
    final path = '$dotfilesDir/.agents/agents/$name';
    fs.directory(path).createSync(recursive: true);
    fs.file('$path/agent.json').writeAsStringSync('{"name": "$name"}');
  }

  Link userSkillLink(String name) =>
      fs.link('$homeDir/.agents/skills/$name');

  Link geminiSkillLink(String name) =>
      fs.link('$dotfilesDir/.gemini/skills/$name');

  Link userAgentLink(String name) =>
      fs.link('$homeDir/.agents/agents/$name');

  void expectSymlink(Link link, String expectedTarget) {
    expect(link.existsSync(), isTrue, reason: '${link.path} should exist');
    expect(link.targetSync(), equals(expectedTarget));
  }

  AgentSyncer createSyncer({
    bool dryRun = false,
    void Function(String)? logger,
    FileSystem? fileSystem,
  }) {
    return AgentSyncer(
      fs: fileSystem ?? fs,
      dotfilesDir: dotfilesDir,
      homeDir: homeDir,
      dryRun: dryRun,
      logger: logger,
    );
  }

  setUp(() {
    fs = MemoryFileSystem(style: FileSystemStyle.posix);
    createCustomSkill('custom-skill');
    createThirdPartySkill('superpowers', 'brainstorming');
    createAgentPersona('reidbaker-agent');
  });

  group('AgentSyncer Unit Tests', () {
    test('creates symlinks for custom and third-party skills', () {
      final syncer = createSyncer();
      final result = syncer.syncAll();

      expectSymlink(
        userSkillLink('custom-skill'),
        '$dotfilesDir/.agents/skills/custom-skill',
      );
      expectSymlink(
        geminiSkillLink('custom-skill'),
        '../../.agents/skills/custom-skill',
      );
      expectSymlink(
        userSkillLink('brainstorming'),
        '$dotfilesDir/third_party/superpowers/skills/brainstorming',
      );
      expectSymlink(
        geminiSkillLink('brainstorming'),
        '../../third_party/superpowers/skills/brainstorming',
      );
      expectSymlink(
        userAgentLink('reidbaker-agent'),
        '$dotfilesDir/.agents/agents/reidbaker-agent',
      );

      expect(result.totalLinked, equals(5));
    });

    test('is idempotent on successive runs', () {
      final syncer = createSyncer();

      final firstRun = syncer.syncAll();
      expect(firstRun.totalLinked, equals(5));

      final secondRun = syncer.syncAll();
      expect(secondRun.totalLinked, equals(0));
      for (final action in secondRun.allActions) {
        expect(action.type, equals(SyncActionType.ok));
      }
    });

    test('updates outdated symlinks when target changes', () {
      final syncer = createSyncer();
      syncer.syncAll();

      // Redirect link to invalid target
      final link = userSkillLink('custom-skill');
      link.deleteSync();
      link.createSync('/wrong/path');

      final result = syncer.syncAll();
      final updatedAction = result.customSkillActions.firstWhere(
        (a) => a.name == 'custom-skill',
      );

      expect(updatedAction.type, equals(SyncActionType.updated));
      expectSymlink(link, '$dotfilesDir/.agents/skills/custom-skill');
    });

    test('backs up existing directories to .bak before symlinking', () {
      final realDir = fs.directory('$homeDir/.agents/skills/custom-skill');
      realDir.createSync(recursive: true);
      fs.file('${realDir.path}/file.txt').writeAsStringSync('existing');

      final syncer = createSyncer();
      final result = syncer.syncAll();

      final backupAction = result.customSkillActions.firstWhere(
        (a) => a.name == 'custom-skill',
      );
      expect(backupAction.type, equals(SyncActionType.backedUpAndLinked));

      expect(
        fs.directory('$homeDir/.agents/skills/custom-skill.bak').existsSync(),
        isTrue,
      );
      expectSymlink(
        userSkillLink('custom-skill'),
        '$dotfilesDir/.agents/skills/custom-skill',
      );
    });

    test('skips directories missing SKILL.md', () {
      fs
          .directory('$dotfilesDir/.agents/skills/empty-folder')
          .createSync(recursive: true);

      final syncer = createSyncer();
      final result = syncer.syncAll();

      final skipped = result.customSkillActions.firstWhere(
        (a) => a.name == 'empty-folder',
      );
      expect(skipped.type, equals(SyncActionType.skipped));
      expect(userSkillLink('empty-folder').existsSync(), isFalse);
    });

    test('dry-run previews actions without filesystem modifications', () {
      final syncer = createSyncer(dryRun: true);
      final result = syncer.syncAll();

      expect(result.totalLinked, equals(5));
      expect(fs.directory('$homeDir/.agents').existsSync(), isFalse);
      expect(userSkillLink('custom-skill').existsSync(), isFalse);
    });

    test('handles missing optional source directories gracefully', () {
      final emptyFs = MemoryFileSystem(style: FileSystemStyle.posix);
      emptyFs.directory('$dotfilesDir/.agents/skills').createSync(
        recursive: true,
      );

      final syncer = createSyncer(fileSystem: emptyFs);
      final result = syncer.syncAll();

      expect(result.customSkillActions, isEmpty);
      expect(result.thirdPartySkillActions, isEmpty);
      expect(result.agentActions, isEmpty);
    });

    test('deduplicates skills with precedence to custom skills', () {
      createThirdPartySkill('superpowers', 'custom-skill');

      final syncer = createSyncer();
      final result = syncer.syncAll();

      final customAction = result.customSkillActions.firstWhere(
        (a) => a.name == 'custom-skill',
      );
      final duplicateAction = result.thirdPartySkillActions.firstWhere(
        (a) => a.name == 'custom-skill',
      );

      expect(customAction.type, equals(SyncActionType.linked));
      expect(duplicateAction.type, equals(SyncActionType.skipped));
      expectSymlink(
        userSkillLink('custom-skill'),
        '$dotfilesDir/.agents/skills/custom-skill',
      );
    });
  });
}
