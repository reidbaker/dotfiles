import 'sync_action.dart';

/// Aggregated result of a full synchronization run.
class SyncResult {
  const SyncResult({
    required this.customSkillActions,
    required this.thirdPartySkillActions,
    required this.agentActions,
  });

  final List<SyncAction> customSkillActions;
  final List<SyncAction> thirdPartySkillActions;
  final List<SyncAction> agentActions;

  List<SyncAction> get allActions => [
    ...customSkillActions,
    ...thirdPartySkillActions,
    ...agentActions,
  ];

  int get totalLinked =>
      allActions
          .where(
            (a) =>
                a.type == SyncActionType.linked ||
                a.type == SyncActionType.updated ||
                a.type == SyncActionType.backedUpAndLinked,
          )
          .length;
}
