import 'dart:io';

import 'package:dotfiles/src/review_pipeline/review_pipeline.dart';
import 'package:path/path.dart' as p;
import 'package:test/test.dart';

void main() {
  group('review_pipeline', () {
    const diff = '''
diff --git a/lib/a.dart b/lib/a.dart
index 1..2 100644
--- a/lib/a.dart
+++ b/lib/a.dart
@@ -1,2 +1,3 @@ header
 one
+two
 three
@@ -10 +11,2 @@
-x
+y
+z
diff --git a/old.md b/old.md
deleted file mode 100644
--- a/old.md
+++ /dev/null
@@ -1,1 +0,0 @@
-gone
''';

    test('parseUnifiedDiff splits files and hunks with new ranges', () {
      final hunks = parseUnifiedDiff(diff);
      expect(hunks.map((h) => h.id), [
        'lib/a.dart#0',
        'lib/a.dart#1',
        'old.md#0',
      ]);
      expect([hunks[0].newStart, hunks[0].newEnd], [1, 3]);
      expect([hunks[1].newStart, hunks[1].newEnd], [11, 12]);
      expect(hunks[2].deletedFile, isTrue);
      expect(hunks[1].text, startsWith('@@ -10 +11,2 @@'));
    });

    test('fileContext numbers lines and clamps to the file', () {
      final dir = Directory.systemTemp.createTempSync('rp');
      addTearDown(() => dir.deleteSync(recursive: true));
      final f = File(p.join(dir.path, 'a.dart'))
        ..writeAsStringSync(List.generate(20, (i) => 'L${i + 1}').join('\n'));
      final h = parseUnifiedDiff(diff)[1];
      expect(fileContext(f, h, 2)!.split('\n'), [
        '9: L9',
        '10: L10',
        '11: L11',
        '12: L12',
        '13: L13',
        '14: L14',
      ]);
      expect(fileContext(File(p.join(dir.path, 'nope')), h, 2), isNull);
    });

    test('fileDomain tags common paths', () {
      expect(fileDomain('.github/workflows/x.yaml'), 'ci_workflow_yaml');
      expect(fileDomain('release/test/cli_test.dart'), 'dart_test');
      expect(fileDomain('release/lib/a.dart'), 'dart_lib');
      expect(fileDomain('shell/platform/windows/view.cc'), 'native_cpp_darwin');
      expect(fileDomain('RELEASING.md'), 'markdown_doc');
    });

    test('ReviewPersona.parse resolves short names and full slugs', () {
      expect(ReviewPersona.parse('lasse'), ReviewPersona.lasse);
      expect(ReviewPersona.parse('lasse-nielsen'), ReviewPersona.lasse);
      expect(ReviewPersona.parse('codefu'), ReviewPersona.codefu);
      expect(ReviewPersona.parse('john-mcdole'), ReviewPersona.codefu);
      expect(ReviewPersona.parse('chris').defaultCap, 5);
      expect(ReviewPersona.parse('lasse').defaultCap, 8);
    });

    CriticComment c(
      String path,
      int line,
      String sev,
      String text, [
      int order = 0,
    ]) => CriticComment(
      path: path,
      line: line,
      comment: text,
      severity: sev,
      order: order,
    );

    test('CriticComment.tryParse drops no-comment records', () {
      expect(
        CriticComment.tryParse({'path': 'a', 'comment': 'no comment'}, 0),
        isNull,
      );
      expect(
        CriticComment.tryParse({
          'path': 'a',
          'line': 3,
          'severity': 'major',
          'comment': 'Why?',
        }, 0)?.severity,
        'major',
      );
    });

    test('rankReview dedupes nearby lines, keeps the higher severity', () {
      final r = rankReview([
        c('a.dart', 10, 'nit', 'rename this', 0),
        c('a.dart', 12, 'major', 'this leaks the lock', 1),
        c('b.dart', 1, 'minor', 'unrelated docs point', 2),
      ], k: 5);
      expect(r.comments.map((x) => x.comment), [
        'this leaks the lock',
        'unrelated docs point',
      ]);
    });

    test('rankReview collapses repeated text to Same here.', () {
      final r = rankReview([
        c('a.yaml', 10, 'major', 'This is a lot of bash code in the workflow'),
        c('b.yaml', 50, 'minor', 'unrelated question about names', 1),
        c('c.yaml', 5, 'major', 'This is a lot of bash code in workflow', 2),
      ], k: 5);
      expect(r.comments.map((x) => '${x.path}:${x.comment}'), [
        'a.yaml:This is a lot of bash code in the workflow',
        'c.yaml:Same here.',
        'b.yaml:unrelated question about names',
      ]);
    });

    test('rankReview caps to k but keeps the uncapped list', () {
      final r = rankReview([
        for (var i = 0; i < 8; i++)
          c('f$i.dart', 1, 'minor', 'point $i x$i', i),
      ], k: 3);
      expect(r.comments, hasLength(3));
      expect(r.deduped, hasLength(8));
      expect(r.toJson()['uncapped'], hasLength(8));
    });

    test('reviewVerdict follows pass, relationship and severity', () {
      String v(int pass, String? sev, [String rel = 'agent']) => reviewVerdict(
        passIndex: pass,
        authorRelationship: rel,
        maxSeverity: sev,
      );
      expect(v(1, 'major'), 'COMMENT');
      expect(v(1, 'blocking'), 'REQUEST_CHANGES');
      expect(v(2, 'major'), 'REQUEST_CHANGES');
      expect(v(4, 'minor'), 'APPROVE');
      expect(v(4, null), 'APPROVE');
      expect(v(3, 'minor', 'external'), 'COMMENT');
    });

    test('sanitizeLocusForStage2 strips prescriptive why field', () {
      final cleaned = sanitizeLocusForStage2({
        'path': '.github/workflows/release_publish.yaml',
        'line': 72,
        'line_range': [60, 95],
        'lens': 'script-vs-dart-tooling',
        'what_is_here': 'Workflow draft verification bash script',
        'why': 'gh release view might fail on network blip',
      });
      expect(cleaned.containsKey('why'), isFalse);
      expect(cleaned['lens'], 'script-vs-dart-tooling');
      expect(
        cleaned['what_is_here'],
        'Workflow draft verification bash script',
      );
    });

    test('rankReview stratified slot allocation reserves slots for minor/nit craft catches across personas', () {
      final candidates = [
        CriticComment(
          path: 'a.dart',
          line: 10,
          severity: 'major',
          lens: 'architecture-placement',
          comment: 'Why is this helper in a separate file?',
          order: 0,
        ),
        CriticComment(
          path: 'b.dart',
          line: 20,
          severity: 'major',
          lens: 'architecture-placement',
          comment: 'Why duplicate the parser across two classes?',
          order: 1,
        ),
        CriticComment(
          path: 'c.dart',
          line: 30,
          severity: 'major',
          lens: 'architecture-placement',
          comment: 'Why is dependabot checked in actionPinFiles?',
          order: 2,
        ),
        CriticComment(
          path: 'd.dart',
          line: 40,
          severity: 'major',
          lens: 'architecture-placement',
          comment: 'Why expose this internal class outside src/?',
          order: 3,
        ),
        CriticComment(
          path: 'e.dart',
          line: 50,
          severity: 'major',
          lens: 'architecture-placement',
          comment: 'Why pass three positional strings instead of a record?',
          order: 4,
        ),
        CriticComment(
          path: 'doc.dart',
          line: 8,
          severity: 'nit',
          lens: 'api-naming-ergonomics',
          comment: "Don't start any DartDoc with `Returns`; use a single sentence noun phrase.",
          order: 5,
        ),
        CriticComment(
          path: 'iter.dart',
          line: 28,
          severity: 'minor',
          lens: 'api-naming-ergonomics',
          comment: 'Avoid `.toList()` before single-pass iteration:\n```suggestion\nfor (final item in items) {\n```',
          order: 6,
        ),
      ];

      final ranked = rankReview(
        candidates,
        k: 5,
        personSlug: 'lasse-nielsen',
        passIndex: 1,
      );
      expect(ranked.comments, hasLength(5));
      expect(
        ranked.comments.map((c) => c.path),
        containsAll(['doc.dart', 'iter.dart']),
      );
    });
  });
}
