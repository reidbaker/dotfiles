/// Represents the type of action performed during synchronization.
enum SyncActionType {
  /// A new symlink was created.
  linked,

  /// Symlink is already up to date.
  ok,

  /// An outdated or differing symlink was updated.
  updated,

  /// An existing real directory was backed up to .bak and replaced with a symlink.
  backedUpAndLinked,

  /// The entry was skipped (e.g., missing SKILL.md, duplicate, or invalid directory).
  skipped,
}

/// A record of a sync action on a skill or agent directory.
class SyncAction {
  const SyncAction({
    required this.name,
    required this.sourcePath,
    required this.targetPath,
    required this.type,
    this.message,
  });

  final String name;
  final String sourcePath;
  final String targetPath;
  final SyncActionType type;
  final String? message;

  @override
  String toString() =>
      'SyncAction($name, $type, src: $sourcePath, dest: $targetPath)';
}
