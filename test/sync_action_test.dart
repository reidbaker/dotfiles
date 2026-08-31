import 'package:dotfiles/sync_action.dart';
import 'package:test/test.dart';

void main() {
  group('SyncAction', () {
    test('instantiates with required fields and formats toString()', () {
      const action = SyncAction(
        name: 'test-skill',
        sourcePath: '/src/test-skill',
        targetPath: '/dest/test-skill',
        type: SyncActionType.linked,
        message: 'created link',
      );

      expect(action.name, equals('test-skill'));
      expect(action.sourcePath, equals('/src/test-skill'));
      expect(action.targetPath, equals('/dest/test-skill'));
      expect(action.type, equals(SyncActionType.linked));
      expect(action.message, equals('created link'));
      expect(
        action.toString(),
        equals(
          'SyncAction(test-skill, SyncActionType.linked, src: /src/test-skill, dest: /dest/test-skill)',
        ),
      );
    });
  });
}
