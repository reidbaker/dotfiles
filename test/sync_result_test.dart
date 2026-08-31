import 'package:dotfiles/sync_action.dart';
import 'package:dotfiles/sync_result.dart';
import 'package:test/test.dart';

void main() {
  group('SyncResult', () {
    test('aggregates allActions correctly', () {
      const customAction = SyncAction(
        name: 'custom-skill',
        sourcePath: '/dotfiles/.agents/skills/custom-skill',
        targetPath: '/home/.agents/skills/custom-skill',
        type: SyncActionType.linked,
      );
      const tpAction = SyncAction(
        name: 'tp-skill',
        sourcePath: '/dotfiles/third_party/repo/skills/tp-skill',
        targetPath: '/home/.agents/skills/tp-skill',
        type: SyncActionType.updated,
      );
      const agentAction = SyncAction(
        name: 'reidbaker-agent',
        sourcePath: '/dotfiles/.agents/agents/reidbaker-agent',
        targetPath: '/home/.agents/agents/reidbaker-agent',
        type: SyncActionType.ok,
      );

      const result = SyncResult(
        customSkillActions: [customAction],
        thirdPartySkillActions: [tpAction],
        agentActions: [agentAction],
      );

      expect(result.allActions, equals([customAction, tpAction, agentAction]));
      expect(result.totalLinked, equals(2)); // linked + updated
    });

    test(
      'counts backedUpAndLinked towards totalLinked and ignores ok and skipped',
      () {
        const backupAction = SyncAction(
          name: 'backed-up-skill',
          sourcePath: '/src',
          targetPath: '/dest',
          type: SyncActionType.backedUpAndLinked,
        );
        const skippedAction = SyncAction(
          name: 'skipped-skill',
          sourcePath: '/src',
          targetPath: '/dest',
          type: SyncActionType.skipped,
        );

        const result = SyncResult(
          customSkillActions: [backupAction],
          thirdPartySkillActions: [skippedAction],
          agentActions: [],
        );

        expect(result.totalLinked, equals(1));
      },
    );
  });
}
