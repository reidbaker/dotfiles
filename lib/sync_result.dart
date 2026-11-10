import 'sync_action.dart';

/// Aggregated result of a full synchronization run.
class SyncResult {
  const SyncResult({
    required this.customSkillActions,
    required this.thirdPartySkillActions,
    required this.agentActions,
    this.ruleActions = const [],
    this.externalSkillActions = const [],
  });

  final List<SyncAction> customSkillActions;
  final List<SyncAction> thirdPartySkillActions;
  final List<SyncAction> agentActions;
  final List<SyncAction> ruleActions;
  final List<SyncAction> externalSkillActions;

  List<SyncAction> get allActions => [
    ...customSkillActions,
    ...thirdPartySkillActions,
    ...externalSkillActions,
    ...agentActions,
    ...ruleActions,
  ];

  int get totalLinked => allActions
      .where(
        (a) =>
            a.type == SyncActionType.linked ||
            a.type == SyncActionType.updated ||
            a.type == SyncActionType.backedUpAndLinked,
      )
      .length;
}
