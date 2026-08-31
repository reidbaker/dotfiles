import 'package:file/file.dart';
import 'package:file/local.dart';
import 'package:path/path.dart' as p;

import 'sync_action.dart';
import 'sync_result.dart';

/// Orchestrates synchronization of custom authored skills, third-party skills,
/// and agent personas from the dotfiles repository into ~/.agents/ and .gemini/skills/.
class AgentSyncer {
  AgentSyncer({
    FileSystem? fs,
    required this.dotfilesDir,
    required this.homeDir,
    this.dryRun = false,
    this.logger,
  }) : fs = fs ?? const LocalFileSystem();

  final FileSystem fs;
  final String dotfilesDir;
  final String homeDir;
  final bool dryRun;
  final void Function(String message)? logger;

  p.Context get pathContext => fs.path;

  String get agentsTargetDir => pathContext.join(homeDir, '.agents');
  String get skillsTargetDir => pathContext.join(agentsTargetDir, 'skills');
  String get agentsConfTargetDir => pathContext.join(agentsTargetDir, 'agents');
  String get geminiSkillsDir =>
      pathContext.join(dotfilesDir, '.gemini', 'skills');

  String get customSkillsSrcDir =>
      pathContext.join(dotfilesDir, '.agents', 'skills');
  String get thirdPartySrcDir => pathContext.join(dotfilesDir, 'third_party');
  String get agentsSrcDir => pathContext.join(dotfilesDir, '.agents', 'agents');

  void _log(String message) {
    if (logger != null) {
      logger!(message);
    }
  }

  /// Runs the full synchronization process.
  SyncResult syncAll() {
    _log('==================================================');
    _log(' Syncing agent skills and personas into ~/.agents');
    _log(' Dotfiles Source: $dotfilesDir');
    _log(' Target Dir:      $agentsTargetDir');
    if (dryRun) {
      _log(' Mode:            DRY RUN (no changes applied)');
    }
    _log('==================================================');

    _prepareTargetDirectories();

    final seenSkillNames = <String>{};

    _log('\nLinking custom authored skills (.agents/skills/):');
    final customActions = syncCustomSkills(seenSkillNames);

    _log('\nLinking third-party skills (third_party/*/skills/):');
    final thirdPartyActions = syncThirdPartySkills(seenSkillNames);

    _log('\nLinking agent personas into ~/.agents/agents/:');
    final agentActions = syncAgents();

    _log('\nDone! Agent skills and personas are synchronized into ~/.agents');

    return SyncResult(
      customSkillActions: customActions,
      thirdPartySkillActions: thirdPartyActions,
      agentActions: agentActions,
    );
  }

  void _prepareTargetDirectories() {
    if (dryRun) {
      return;
    }
    fs.directory(skillsTargetDir).createSync(recursive: true);
    fs.directory(agentsConfTargetDir).createSync(recursive: true);
    fs.directory(geminiSkillsDir).createSync(recursive: true);
  }

  /// Syncs custom authored skills located in `.agents/skills/`.
  List<SyncAction> syncCustomSkills([Set<String>? seenNames]) {
    final actions = <SyncAction>[];
    final customDir = fs.directory(customSkillsSrcDir);
    if (!customDir.existsSync()) {
      return actions;
    }

    final skillDirs = _listSubdirectories(customDir);
    for (final dir in skillDirs) {
      actions.addAll(_syncSingleSkill(dir.path, seenNames));
    }
    return actions;
  }

  /// Syncs third-party skills located in `third_party/<repo>/skills/`.
  List<SyncAction> syncThirdPartySkills([Set<String>? seenNames]) {
    final actions = <SyncAction>[];
    final skillDirs = _findThirdPartySkillDirectories();
    for (final dirPath in skillDirs) {
      actions.addAll(_syncSingleSkill(dirPath, seenNames));
    }
    return actions;
  }

  List<SyncAction> _syncSingleSkill(String skillPath, Set<String>? seenNames) {
    final name = pathContext.basename(skillPath);
    if (seenNames != null && seenNames.contains(name)) {
      _log('  [SKIP] $name -> duplicate skill name (already linked)');
      return [
        SyncAction(
          name: name,
          sourcePath: skillPath,
          targetPath: pathContext.join(skillsTargetDir, name),
          type: SyncActionType.skipped,
          message: 'Duplicate skill name',
        ),
      ];
    }

    final actions = _linkSkillToTargets(skillPath);
    if (seenNames != null &&
        actions.any((a) => a.type != SyncActionType.skipped)) {
      seenNames.add(name);
    }
    return actions;
  }

  List<String> _findThirdPartySkillDirectories() {
    final tpDir = fs.directory(thirdPartySrcDir);
    if (!tpDir.existsSync()) {
      return const [];
    }

    final skillPaths = <String>[];
    final repoDirs = _listSubdirectories(tpDir);
    for (final repoDir in repoDirs) {
      final skillsDir = fs.directory(pathContext.join(repoDir.path, 'skills'));
      if (skillsDir.existsSync()) {
        final subDirs = _listSubdirectories(skillsDir);
        skillPaths.addAll(subDirs.map((d) => d.path));
      }
    }
    return skillPaths;
  }

  List<Directory> _listSubdirectories(Directory parent) {
    return parent
        .listSync()
        .whereType<Directory>()
        .toList()
      ..sort((a, b) => a.path.compareTo(b.path));
  }

  List<SyncAction> _linkSkillToTargets(String skillPath) {
    final actionGlobal = linkEntry(
      srcPath: skillPath,
      targetParentDir: skillsTargetDir,
      relativeLink: false,
      requireSkillMd: true,
    );
    final actionGemini = linkEntry(
      srcPath: skillPath,
      targetParentDir: geminiSkillsDir,
      relativeLink: true,
      requireSkillMd: true,
    );
    return [actionGlobal, actionGemini];
  }

  /// Syncs agent personas located in `.agents/agents/`.
  List<SyncAction> syncAgents() {
    final actions = <SyncAction>[];
    final agentsDir = fs.directory(agentsSrcDir);
    if (!agentsDir.existsSync()) {
      return actions;
    }

    final entities = _listSubdirectories(agentsDir);
    for (final entity in entities) {
      final action = linkEntry(
        srcPath: entity.path,
        targetParentDir: agentsConfTargetDir,
        relativeLink: false,
        requireSkillMd: false,
      );
      actions.add(action);
    }
    return actions;
  }

  /// Links a single source directory into [targetParentDir].
  SyncAction linkEntry({
    required String srcPath,
    required String targetParentDir,
    bool relativeLink = false,
    bool requireSkillMd = false,
  }) {
    final name = pathContext.basename(srcPath);
    final destLinkPath = pathContext.join(targetParentDir, name);

    if (requireSkillMd && !_hasSkillMd(srcPath)) {
      return SyncAction(
        name: name,
        sourcePath: srcPath,
        targetPath: destLinkPath,
        type: SyncActionType.skipped,
        message: 'Missing SKILL.md',
      );
    }

    final linkTarget =
        relativeLink
            ? pathContext.relative(srcPath, from: targetParentDir)
            : srcPath;

    final destType = fs.typeSync(destLinkPath, followLinks: false);

    return switch (destType) {
      FileSystemEntityType.link => _handleExistingLink(
        destLinkPath: destLinkPath,
        name: name,
        targetParentDir: targetParentDir,
        linkTarget: linkTarget,
        srcPath: srcPath,
      ),
      FileSystemEntityType.directory => _handleExistingDirectory(
        destLinkPath: destLinkPath,
        name: name,
        linkTarget: linkTarget,
        srcPath: srcPath,
      ),
      _ => _createNewLink(
        destLinkPath: destLinkPath,
        name: name,
        targetParentDir: targetParentDir,
        linkTarget: linkTarget,
        srcPath: srcPath,
        destType: destType,
      ),
    };
  }

  bool _hasSkillMd(String srcPath) {
    return fs.file(pathContext.join(srcPath, 'SKILL.md')).existsSync();
  }

  SyncAction _handleExistingLink({
    required String destLinkPath,
    required String name,
    required String targetParentDir,
    required String linkTarget,
    required String srcPath,
  }) {
    final currentTarget = fs.link(destLinkPath).targetSync();
    if (currentTarget == linkTarget || currentTarget == srcPath) {
      _log(
        '  [OK] $name -> already linked in ${pathContext.basename(targetParentDir)}',
      );
      return SyncAction(
        name: name,
        sourcePath: srcPath,
        targetPath: destLinkPath,
        type: SyncActionType.ok,
      );
    }

    _log(
      '  [UPDATE] $name -> updating link in ${pathContext.basename(targetParentDir)} from $currentTarget',
    );
    if (!dryRun) {
      fs.link(destLinkPath).deleteSync();
      fs.link(destLinkPath).createSync(linkTarget);
    }
    return SyncAction(
      name: name,
      sourcePath: srcPath,
      targetPath: destLinkPath,
      type: SyncActionType.updated,
      message: 'Updated target from $currentTarget to $linkTarget',
    );
  }

  SyncAction _handleExistingDirectory({
    required String destLinkPath,
    required String name,
    required String linkTarget,
    required String srcPath,
  }) {
    final backupPath = '$destLinkPath.bak';
    _log(
      '  [BACKUP] Existing directory found at $destLinkPath. Backing up to $backupPath',
    );
    if (!dryRun) {
      fs.directory(destLinkPath).renameSync(backupPath);
      fs.link(destLinkPath).createSync(linkTarget);
    }
    return SyncAction(
      name: name,
      sourcePath: srcPath,
      targetPath: destLinkPath,
      type: SyncActionType.backedUpAndLinked,
      message: 'Backed up existing directory to $backupPath',
    );
  }

  SyncAction _createNewLink({
    required String destLinkPath,
    required String name,
    required String targetParentDir,
    required String linkTarget,
    required String srcPath,
    required FileSystemEntityType destType,
  }) {
    if (!dryRun) {
      if (destType == FileSystemEntityType.file) {
        fs.file(destLinkPath).deleteSync();
      }
      fs.link(destLinkPath).createSync(linkTarget);
    }
    _log(
      '  [LINKED] $name in ${pathContext.basename(targetParentDir)} -> $linkTarget',
    );
    return SyncAction(
      name: name,
      sourcePath: srcPath,
      targetPath: destLinkPath,
      type: SyncActionType.linked,
    );
  }
}
