import 'dart:io' show Platform;

import 'package:file/file.dart';
import 'package:file/local.dart';
import 'package:path/path.dart' as p;

import 'machine_selector.dart';
import 'sync_action.dart';
import 'sync_result.dart';

/// Orchestrates synchronization of custom authored skills, third-party skills,
/// external skill roots, rules, and agent personas into `~/.agents/` and
/// `~/.gemini/config/`.
class AgentSyncer {
  AgentSyncer({
    FileSystem? fs,
    required this.dotfilesDir,
    required this.homeDir,
    this.dryRun = false,
    this.logger,
    MachineSelector? machineSelector,
    this.externalSkillRoots = const [],
  }) : fs = fs ?? const LocalFileSystem(),
       machineSelector =
           machineSelector ??
           MachineSelector.detect(
             isMacOS: Platform.isMacOS,
             isLinux: Platform.isLinux,
           );

  final FileSystem fs;
  final String dotfilesDir;
  final String homeDir;
  final bool dryRun;
  final void Function(String message)? logger;
  final MachineSelector machineSelector;
  final List<String> externalSkillRoots;

  p.Context get pathContext => fs.path;

  String get agentsTargetDir => pathContext.join(homeDir, '.agents');
  String get skillsTargetDir => pathContext.join(agentsTargetDir, 'skills');
  String get agentsConfTargetDir => pathContext.join(agentsTargetDir, 'agents');
  String get geminiSkillsDir =>
      pathContext.join(dotfilesDir, '.gemini', 'skills');
  String get rulesTargetDir =>
      pathContext.join(homeDir, '.gemini', 'config', 'rules');
  String get geminiConfigSkillsLink =>
      pathContext.join(homeDir, '.gemini', 'config', 'skills');

  String get customSkillsSrcDir =>
      pathContext.join(dotfilesDir, '.agents', 'skills');
  String get rulesSrcDir => pathContext.join(dotfilesDir, '.agents', 'rules');
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
    _log(' Syncing agent skills, rules, and personas');
    _log(' Dotfiles Source: $dotfilesDir');
    _log(' Target Dir:      $agentsTargetDir');
    _log(' Machine Tags:    ${machineSelector.activeTags.join(', ')}');
    if (dryRun) {
      _log(' Mode:            DRY RUN (no changes applied)');
    }
    _log('==================================================');

    _prepareTargetDirectories();

    final seenSkillNames = <String>{};

    _log('\nLinking custom authored skills (.agents/skills/):');
    final customActions = syncCustomSkills(seenSkillNames);

    _log('\nLinking external skill roots:');
    final externalActions = syncExternalSkillRoots(seenSkillNames);

    _log('\nLinking third-party skills (third_party/*/skills/):');
    final thirdPartyActions = syncThirdPartySkills(seenSkillNames);

    _log('\nLinking rules (.agents/rules/ -> ~/.gemini/config/rules/):');
    final ruleActions = syncRules();

    _log('\nLinking agent personas into ~/.agents/agents/:');
    final agentActions = syncAgents();

    _log('\nDone! Agent skills, rules, and personas are synchronized.');

    return SyncResult(
      customSkillActions: customActions,
      thirdPartySkillActions: thirdPartyActions,
      externalSkillActions: externalActions,
      ruleActions: ruleActions,
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
    fs.directory(rulesTargetDir).createSync(recursive: true);
    _ensureGeminiConfigSkillsLink();
  }

  void _ensureGeminiConfigSkillsLink() {
    final type = fs.typeSync(geminiConfigSkillsLink, followLinks: false);
    if (type == FileSystemEntityType.notFound) {
      fs.link(geminiConfigSkillsLink).createSync(skillsTargetDir);
    }
  }

  /// Syncs custom authored skills located in `.agents/skills/`.
  List<SyncAction> syncCustomSkills([Set<String>? seenNames]) {
    final customDir = fs.directory(customSkillsSrcDir);
    if (!customDir.existsSync()) {
      return const [];
    }
    return _syncConfiguredSkillDir(
      skillsDir: customDir,
      configFile: fs.file(
        pathContext.join(customSkillsSrcDir, 'skills_config.json'),
      ),
      seenNames: seenNames,
    );
  }

  /// Syncs additional external skill roots (such as `ai-team` or CitC roots).
  List<SyncAction> syncExternalSkillRoots([Set<String>? seenNames]) {
    final actions = <SyncAction>[];
    for (final root in externalSkillRoots) {
      final resolvedRoot = _expandHome(root);
      final dir = fs.directory(resolvedRoot);
      if (!dir.existsSync()) {
        _log('  [SKIP] External root not found: $resolvedRoot');
        continue;
      }
      actions.addAll(
        _syncConfiguredSkillDir(
          skillsDir: dir,
          configFile: fs.file(pathContext.join(resolvedRoot, 'skills_config.json')),
          seenNames: seenNames,
        ),
      );
    }
    return actions;
  }

  /// Syncs third-party skills located in `third_party/<repo>/skills/`.
  List<SyncAction> syncThirdPartySkills([Set<String>? seenNames]) {
    final tpDir = fs.directory(thirdPartySrcDir);
    if (!tpDir.existsSync()) {
      return const [];
    }

    final actions = <SyncAction>[];
    for (final repoDir in _listSubdirectories(tpDir)) {
      final skillsDir = fs.directory(pathContext.join(repoDir.path, 'skills'));
      if (!skillsDir.existsSync()) {
        continue;
      }
      actions.addAll(
        _syncConfiguredSkillDir(
          skillsDir: skillsDir,
          configFile: fs.file(
            pathContext.join(repoDir.path, 'skills_config.json'),
          ),
          seenNames: seenNames,
        ),
      );
    }
    return actions;
  }

  List<SyncAction> _syncConfiguredSkillDir({
    required Directory skillsDir,
    required File configFile,
    required Set<String>? seenNames,
  }) {
    final config = SyncConfig.tryLoad(
      configFile: configFile,
      listKey: 'skills',
      selector: machineSelector,
    );
    final actions = <SyncAction>[];
    for (final dir in _listSubdirectories(skillsDir)) {
      final name = pathContext.basename(dir.path);
      if (_isSkillEnabled(name, config)) {
        actions.addAll(_syncSingleSkill(dir.path, seenNames));
      } else {
        _unlinkUnlistedSkill(dir.path);
      }
    }
    if (config != null) {
      actions.addAll(_syncExternalEntries(config, seenNames));
    }
    return actions;
  }

  bool _isSkillEnabled(String name, SyncConfig? config) {
    if (config == null) {
      return true;
    }
    return config.enabledNames.contains(name) &&
        !config.disabledNames.contains(name);
  }

  List<SyncAction> _syncExternalEntries(
    SyncConfig config,
    Set<String>? seenNames,
  ) {
    final actions = <SyncAction>[];
    for (final entry in config.externalSources.entries) {
      final targetSrc = _expandHome(entry.value);
      if (!fs.directory(targetSrc).existsSync()) {
        _log('  [SKIP] ${entry.key} -> missing source $targetSrc');
        continue;
      }
      actions.addAll(
        _syncSingleSkill(targetSrc, seenNames, overrideName: entry.key),
      );
    }
    for (final entry in config.disabledExternalSources.entries) {
      final targetSrc = _expandHome(entry.value);
      _unlinkUnlistedSkill(targetSrc, overrideName: entry.key);
    }
    return actions;
  }

  String _expandHome(String rawPath) {
    if (rawPath == '~') {
      return homeDir;
    }
    if (rawPath.startsWith('~/')) {
      return pathContext.join(homeDir, rawPath.substring(2));
    }
    return rawPath;
  }

  /// Syncs rule markdown files located in `.agents/rules/` into
  /// `~/.gemini/config/rules/`.
  List<SyncAction> syncRules() {
    final rulesDir = fs.directory(rulesSrcDir);
    if (!rulesDir.existsSync()) {
      return const [];
    }
    final config = SyncConfig.tryLoad(
      configFile: fs.file(pathContext.join(rulesSrcDir, 'rules_config.json')),
      listKey: 'rules',
      selector: machineSelector,
    );
    final actions = <SyncAction>[];
    final files = rulesDir.listSync().whereType<File>().toList()
      ..sort((a, b) => a.path.compareTo(b.path));
    for (final file in files) {
      final name = pathContext.basename(file.path);
      if (!name.endsWith('.md')) {
        continue;
      }
      if (_isSkillEnabled(name, config)) {
        actions.add(_linkRuleFile(file.path));
      } else {
        _unlinkIfTargeting(file.path, rulesTargetDir);
      }
    }
    return actions;
  }

  SyncAction _linkRuleFile(String srcFilePath) {
    final name = pathContext.basename(srcFilePath);
    final destLinkPath = pathContext.join(rulesTargetDir, name);
    final destType = fs.typeSync(destLinkPath, followLinks: false);
    if (destType == FileSystemEntityType.link) {
      return _handleExistingLink(
        destLinkPath: destLinkPath,
        name: name,
        targetParentDir: rulesTargetDir,
        linkTarget: srcFilePath,
        srcPath: srcFilePath,
      );
    }
    if (destType == FileSystemEntityType.file && !dryRun) {
      fs.file(destLinkPath).renameSync('$destLinkPath.bak');
    }
    return _createNewLink(
      destLinkPath: destLinkPath,
      name: name,
      targetParentDir: rulesTargetDir,
      linkTarget: srcFilePath,
      srcPath: srcFilePath,
      destType: FileSystemEntityType.notFound,
    );
  }

  List<SyncAction> _syncSingleSkill(
    String skillPath,
    Set<String>? seenNames, {
    String? overrideName,
  }) {
    final name = overrideName ?? pathContext.basename(skillPath);
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

    final actions = _linkSkillToTargets(skillPath, overrideName: overrideName);
    if (seenNames != null &&
        actions.any((a) => a.type != SyncActionType.skipped)) {
      seenNames.add(name);
    }
    return actions;
  }

  void _unlinkUnlistedSkill(String skillPath, {String? overrideName}) {
    _unlinkIfTargeting(skillPath, skillsTargetDir, overrideName: overrideName);
    _unlinkIfTargeting(skillPath, geminiSkillsDir, overrideName: overrideName);
  }

  void _unlinkIfTargeting(
    String srcPath,
    String targetParentDir, {
    String? overrideName,
  }) {
    final name = overrideName ?? pathContext.basename(srcPath);
    final linkPath = pathContext.join(targetParentDir, name);
    if (fs.typeSync(linkPath, followLinks: false) !=
        FileSystemEntityType.link) {
      return;
    }
    final relativeTarget = pathContext.relative(srcPath, from: targetParentDir);
    final currentTarget = fs.link(linkPath).targetSync();
    if (currentTarget != srcPath && currentTarget != relativeTarget) {
      return;
    }
    if (!dryRun) {
      fs.link(linkPath).deleteSync();
    }
    _log(
      '  [UNLINK] $name -> unlisted in ${pathContext.basename(targetParentDir)}',
    );
  }

  List<Directory> _listSubdirectories(Directory parent) {
    return parent.listSync().whereType<Directory>().toList()
      ..sort((a, b) => a.path.compareTo(b.path));
  }

  List<SyncAction> _linkSkillToTargets(
    String skillPath, {
    String? overrideName,
  }) {
    final actionGlobal = linkEntry(
      srcPath: skillPath,
      targetParentDir: skillsTargetDir,
      relativeLink: false,
      requireSkillMd: true,
      overrideName: overrideName,
    );
    final actionGemini = linkEntry(
      srcPath: skillPath,
      targetParentDir: geminiSkillsDir,
      relativeLink: true,
      requireSkillMd: true,
      overrideName: overrideName,
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

    for (final entity in _listSubdirectories(agentsDir)) {
      actions.add(
        linkEntry(
          srcPath: entity.path,
          targetParentDir: agentsConfTargetDir,
          relativeLink: false,
          requireSkillMd: false,
        ),
      );
    }
    return actions;
  }

  /// Links a single source directory into [targetParentDir].
  SyncAction linkEntry({
    required String srcPath,
    required String targetParentDir,
    bool relativeLink = false,
    bool requireSkillMd = false,
    String? overrideName,
  }) {
    final name = overrideName ?? pathContext.basename(srcPath);
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

    final linkTarget = relativeLink
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
