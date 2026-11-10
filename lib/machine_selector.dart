import 'dart:convert';

import 'package:file/file.dart';

/// Evaluates whether a skill or rule config entry applies to the current host.
class MachineSelector {
  const MachineSelector({required this.activeTags});

  /// Detects machine capability tags from the host OS.
  factory MachineSelector.detect({
    required bool isMacOS,
    required bool isLinux,
    String? overrideTag,
  }) {
    if (overrideTag != null && overrideTag.trim().isNotEmpty) {
      return MachineSelector(
        activeTags: overrideTag
            .split(',')
            .map((s) => s.trim())
            .where((s) => s.isNotEmpty)
            .toSet(),
      );
    }

    final tags = <String>{};
    if (isMacOS) {
      tags.add('mac');
      tags.add('macos');
    }
    if (isLinux) {
      tags.add('linux');
      // Linux workstations typically skip local macOS GUI automation; extra
      // capability tags can be set via ~/.zshenv.local (AGENT_SYNC_MACHINE).
      tags.add('cloudtop');
    }
    return MachineSelector(activeTags: tags);
  }

  final Set<String> activeTags;

  /// Returns true if [entryMachines] is null/empty or intersects [activeTags].
  bool matches(Set<String>? entryMachines) {
    if (entryMachines == null || entryMachines.isEmpty) {
      return true;
    }
    return entryMachines.any(activeTags.contains);
  }
}

/// Parsed allowlist and machine filters from a `skills_config.json` or
/// `rules_config.json` file.
class SyncConfig {
  const SyncConfig({
    required this.enabledNames,
    required this.disabledNames,
    this.externalSources = const {},
    this.disabledExternalSources = const {},
  });

  final Set<String> enabledNames;
  final Set<String> disabledNames;
  final Map<String, String> externalSources;
  final Map<String, String> disabledExternalSources;

  /// Parses `skills` or `rules` entries from [configFile], returning null if
  /// the file does not exist or has no valid entries list.
  static SyncConfig? tryLoad({
    required File configFile,
    required String listKey,
    required MachineSelector selector,
  }) {
    if (!configFile.existsSync()) {
      return null;
    }
    final decoded = jsonDecode(configFile.readAsStringSync());
    if (decoded is! Map<String, Object?>) {
      return null;
    }
    return _parseMap(decoded, listKey, selector);
  }

  static SyncConfig? _parseMap(
    Map<String, Object?> decoded,
    String listKey,
    MachineSelector selector,
  ) {
    final entries = decoded[listKey];
    final externalMap = _parseExternal(decoded['external'], selector);
    if (entries is! List<Object?>) {
      if (externalMap.enabled.isEmpty && externalMap.disabled.isEmpty) {
        return null;
      }
      return SyncConfig(
        enabledNames: externalMap.enabled.keys.toSet(),
        disabledNames: externalMap.disabled.keys.toSet(),
        externalSources: externalMap.enabled,
        disabledExternalSources: externalMap.disabled,
      );
    }

    final enabled = <String>{...externalMap.enabled.keys};
    final disabled = <String>{...externalMap.disabled.keys};

    for (final item in entries) {
      _classifyEntry(item, selector, enabled, disabled);
    }

    return SyncConfig(
      enabledNames: enabled,
      disabledNames: disabled,
      externalSources: externalMap.enabled,
      disabledExternalSources: externalMap.disabled,
    );
  }

  static void _classifyEntry(
    Object? item,
    MachineSelector selector,
    Set<String> enabled,
    Set<String> disabled,
  ) {
    if (item is String) {
      enabled.add(item);
      return;
    }
    if (item is! Map<String, Object?>) {
      return;
    }
    final name = item['name'];
    if (name is! String || name.isEmpty) {
      return;
    }
    final machines = _readStringSet(item['machines']);
    if (selector.matches(machines)) {
      enabled.add(name);
    } else {
      disabled.add(name);
    }
  }

  static ({Map<String, String> enabled, Map<String, String> disabled})
  _parseExternal(Object? raw, MachineSelector selector) {
    if (raw is! List<Object?>) {
      return (enabled: const {}, disabled: const {});
    }
    final enabled = <String, String>{};
    final disabled = <String, String>{};
    for (final item in raw) {
      if (item is! Map<String, Object?>) {
        continue;
      }
      final name = item['name'];
      final path = item['path'];
      if (name is! String || path is! String) {
        continue;
      }
      final machines = _readStringSet(item['machines']);
      if (selector.matches(machines)) {
        enabled[name] = path;
      } else {
        disabled[name] = path;
      }
    }
    return (enabled: enabled, disabled: disabled);
  }

  static Set<String>? _readStringSet(Object? raw) {
    if (raw is! List<Object?>) {
      return null;
    }
    return raw.whereType<String>().toSet();
  }
}
