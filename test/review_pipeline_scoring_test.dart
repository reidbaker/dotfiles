import 'package:dotfiles/src/review_pipeline/review_pipeline.dart';
import 'package:test/test.dart';

/// One-line summary of every field of [c], so a mismatch shows which field
/// changed.
String _describe(CriticComment c) =>
    '${c.path}:${c.line}:${c.severity}:lens=${c.lens}:'
    'type=${c.feedbackType}:conf=${c.confidence}:'
    'cand=${c.candidateIndex}:order=${c.order}:${c.comment}';

void main() {
  group('parseUnifiedDiff', () {
    test('takes the path from +++ b/ and ignores lines before a hunk', () {
      final hunks = parseUnifiedDiff('''
diff --git a/x.dart b/x.dart
index 1..2 100644
--- a/x.dart
+++ b/y.dart
@@ -1 +5 @@
-old
+new
@@ -10,0 +12,0 @@
diff --git a/z.dart b/z.dart
@@ -3,2 +7,4 @@ ctx
 z
''');
      expect(
        [
          for (final h in hunks)
            '${h.id} ${h.newStart}-${h.newEnd} deleted=${h.deletedFile}',
        ],
        [
          'y.dart#0 5-5 deleted=false',
          'y.dart#1 12-12 deleted=false',
          'z.dart#0 7-10 deleted=false',
        ],
      );
      expect(hunks[0].text, '@@ -1 +5 @@\n-old\n+new');
      expect(hunks[1].text, '@@ -10,0 +12,0 @@');
    });
  });

  group('CriticComment.parseRecord', () {
    test('reads candidate_1 and candidate_2 with record defaults', () {
      final parsed = CriticComment.parseRecord({
        'path': 'a.dart',
        'line': 3,
        'lens': 'scope-hygiene',
        'candidate_1': {'comment': 'First', 'severity': 'major'},
        'candidate_2': {
          'body': 'Second',
          'severity': 'should-fix',
          'line': 9,
          'confidence': 2,
        },
        // Ignored: the candidate_N shape wins over a candidates list.
        'candidates': [
          {'comment': 'Ignored'},
        ],
      }, 4);
      expect(parsed.map(_describe), [
        'a.dart:3:major:lens=scope-hygiene:type=scope-hygiene:conf=0.75:'
            'cand=1:order=8:First',
        'a.dart:9:major:lens=scope-hygiene:type=scope-hygiene:conf=1.0:'
            'cand=2:order=9:Second',
      ]);
    });

    test('keeps candidate_2 when candidate_1 is a no-comment', () {
      final parsed = CriticComment.parseRecord({
        'path': 'a.dart',
        'candidate_1': {'comment': 'No comment'},
        'candidate_2': {'comment': 'Kept', 'lens': 'bug'},
      }, 0);
      expect(parsed.map(_describe), [
        'a.dart:0:minor:lens=bug:type=bug:conf=0.75:cand=2:order=1:Kept',
      ]);
    });

    test('reads a candidates list, skipping non-maps and no-comments', () {
      final parsed = CriticComment.parseRecord({
        'path': 'b.dart',
        'line': 2,
        'severity': 'nit',
        'candidates': [
          {'comment': 'One'},
          'not a map',
          {'comment': 'Three', 'feedback_type': 'bug', 'path': 'c.dart'},
          {'comment': 'null'},
        ],
      }, 1);
      expect(parsed.map(_describe), [
        'b.dart:2:nit:lens=:type=:conf=0.75:cand=1:order=2:One',
        'c.dart:2:nit:lens=bug:type=bug:conf=0.75:cand=3:order=4:Three',
      ]);
    });

    test('reads a comments list when candidates is absent', () {
      final parsed = CriticComment.parseRecord({
        'lens': 'scope-hygiene',
        'comments': [
          {'comment': 'X', 'lens': 'other-lens'},
        ],
      }, 2);
      expect(parsed.map(_describe), [
        ':0:minor:lens=scope-hygiene:type=other-lens:conf=0.75:'
            'cand=1:order=4:X',
      ]);
    });

    test('parses a flat record and normalizes its fields', () {
      final parsed = CriticComment.parseRecord({
        'path': ' a ',
        'line': 1.9,
        'comment': ' Flat ',
        'severity': 'Must-Fix',
        'confidence': -1,
        // Not a list, so the record is read as a flat comment.
        'candidates': 'oops',
      }, 7);
      expect(parsed.map(_describe), [
        'a:1:blocking:lens=:type=:conf=0.0:cand=1:order=7:Flat',
      ]);
    });

    test('drops comments with an unknown severity', () {
      expect(
        CriticComment.parseRecord({'comment': 'x', 'severity': 'huge'}, 0),
        isEmpty,
      );
    });
  });

  group('personaTypeWeight', () {
    // (persona slug, feedback type, comment, expected weight).
    const cases = [
      ('jenn-magder', 'xcode', '', 0.75),
      ('jenn-magder', '', 'Update the Podfile', 0.75),
      ('jenn-magder', 'suggestion', '', 0.72),
      ('jenn-magder', '', '```suggestion\nx\n```', 0.72),
      // The suggestion fence is matched case-sensitively.
      ('jenn-magder', '', '```Suggestion', 0.30),
      ('jenn-magder', 'docs-comments', '', 0.68),
      ('jenn-magder', '', 'Please link back to an issue', 0.68),
      ('jenn-magder', 'api-naming', '', 0.62),
      ('jenn-magder', '', 'Use unawaited here', 0.62),
      ('jenn-magder', 'test-assertion', '', 0.58),
      ('jenn-magder', '', 'Remove the unused import', 0.58),
      ('jenn-magder', 'scope', '', 0.55),
      ('jenn-magder', '', 'Split this PR', 0.55),
      ('jenn-magder', 'bug', '', 0.25),
      ('jenn-magder', 'bug-lifecycle', '', 0.25),
      ('jenn-magder', 'other', 'plain', 0.30),
      // An earlier tier wins over a later one.
      ('jenn-magder', 'scope', 'Update the Podfile', 0.75),
      ('flutter-tools-ghost', 'hermetic', '', 0.78),
      ('flutter-tools-ghost', '', 'Use a BufferLogger', 0.78),
      ('flutter-tools-ghost', 'build-system', '', 0.75),
      ('flutter-tools-ghost', '', 'The Target outputs list', 0.75),
      ('flutter-tools-ghost', '', 'target only', 0.30),
      ('flutter-tools-ghost', 'cli-ux', '', 0.70),
      ('flutter-tools-ghost', '', 'A top-level option', 0.70),
      ('flutter-tools-ghost', '', 'top-level only', 0.30),
      ('flutter-tools-ghost', 'swiftpm', '', 0.68),
      ('flutter-tools-ghost', '', 'Edit the podspec', 0.68),
      ('flutter-tools-ghost', 'correctness', '', 0.64),
      ('flutter-tools-ghost', '', 'Import dart:io directly', 0.64),
      ('flutter-tools-ghost', 'simplify', '', 0.58),
      ('flutter-tools-ghost', '', '```suggestion', 0.58),
      ('flutter-tools-ghost', 'bug', '', 0.25),
      ('flutter-tools-ghost', 'other', 'plain', 0.30),
      ('flutter-tools-ghost', 'hermetic', 'Edit the podspec', 0.78),
      ('reid-baker', 'scope-hygiene', '', 0.75),
      ('reid-baker', '', 'This landed in PR 3', 0.75),
      ('reid-baker', 'script-vs-dart-tooling', '', 0.72),
      ('reid-baker', '', 'Move this into Dart', 0.72),
      ('reid-baker', 'architecture-placement', '', 0.65),
      ('reid-baker', 'ci-workflow-safety', '', 0.60),
      ('reid-baker', '', 'Use a reusable workflow', 0.60),
      ('reid-baker', 'test-assertion-fidelity', '', 0.55),
      ('reid-baker', 'api-naming-ergonomics', '', 0.50),
      ('reid-baker', 'config-sdk-bounds', '', 0.50),
      ('reid-baker', 'bug', '', 0.10),
      ('reid-baker', 'other', 'plain', 0.25),
      ('reid-baker', 'test-assertion-fidelity', 'This landed in PR 3', 0.75),
      ('lasse-nielsen', 'bug', '', 0.45),
      ('lasse-nielsen', 'bug-lifecycle', '', 0.45),
      ('lasse-nielsen', 'architecture-placement', '', 0.65),
    ];
    for (final (slug, type, comment, expected) in cases) {
      test('$slug type="$type" comment="$comment" -> $expected', () {
        expect(personaTypeWeight(slug, type, comment), expected);
      });
    }
  });

  group('hasPersonaSignatureMarker', () {
    // (persona slug, lowercased comment, expected).
    const cases = [
      ('jenn-magder', 'see build_macos.dart', true),
      ('jenn-magder', 'codesign the target', true),
      ('jenn-magder', 'codesign only', false),
      ('jenn-magder', 'aliases in the screenshot', true),
      ('jenn-magder', 'aliases only', false),
      ('jenn-magder', 'nitpick: rename', false),
      ('jenn-magder', 'plain text', false),
      ('flutter-tools-ghost', 'avoid globals.fs', true),
      ('flutter-tools-ghost', 'toolexit vs stateerror', true),
      ('flutter-tools-ghost', 'toolexit only', false),
      ('flutter-tools-ghost', 'nitpick: rename', false),
      ('flutter-tools-ghost', 'plain text', false),
      ('reid-baker', 'nitpick: rename', true),
      ('reid-baker', 'use the pronoun', true),
      ('reid-baker', 'mix .yaml and .yml', true),
      ('reid-baker', 'both .yaml and .yml', true),
      ('reid-baker', '.yaml and .yml', false),
      ('reid-baker', 'the comment says dry run', true),
      ('reid-baker', 'dry run only', false),
      ('reid-baker', 'plain text', false),
      ('lasse-nielsen', 'use the pronoun', true),
    ];
    for (final (slug, comment, expected) in cases) {
      test('$slug "$comment" -> $expected', () {
        expect(hasPersonaSignatureMarker(comment, personSlug: slug), expected);
      });
    }
  });

  group('personaMoveAdjustment', () {
    final longEssay = '${List.filled(70, 'word').join(' ')} race condition';
    // (persona slug, comment, expected adjustment).
    final cases = [
      ('reid-baker', 'Why?', 0.40),
      ('reid-baker', '```suggestion\nfoo\n```', 1.10),
      ('jenn-magder', '```suggestion\nfoo\n```', 1.23),
      ('reid-baker', longEssay, -1.00),
      ('reid-baker', 'This is non-blocking IMO, fine.', 0.20),
      ('jenn-magder', 'This is non-blocking IMO, fine.', -0.20),
      ('flutter-tools-ghost', 'This is non-blocking IMO, fine.', -0.20),
      ('reid-baker', 'This landed in PR #12 already', 0.55),
    ];
    for (final (slug, comment, expected) in cases) {
      test('$slug "${comment.length > 40 ? 'long essay' : comment}"', () {
        expect(
          personaMoveAdjustment(comment, personSlug: slug),
          closeTo(expected, 1e-9),
        );
      });
    }
  });

  group('rankReview craft slots', () {
    CriticComment major(String path, String text, int order) => CriticComment(
      path: path,
      line: 10,
      severity: 'major',
      lens: 'architecture-placement',
      comment: text,
      order: order,
    );
    CriticComment craft(String path, String text, int order) => CriticComment(
      path: path,
      line: 1,
      severity: 'nit',
      lens: 'api-naming-ergonomics',
      comment: text,
      order: order,
    );
    final majors = [
      major('a.dart', 'Why is this helper in a separate file?', 0),
      major('b.dart', 'Why does the parser live in two classes?', 1),
      major('c.dart', 'Why is dependabot checked in actionPinFiles?', 2),
      major('d.dart', 'Why expose this internal class outside src/?', 3),
      major('e.dart', 'Why pass three positional strings, not a record?', 4),
    ];

    List<String> paths(List<CriticComment> candidates, {int passIndex = 1}) =>
        rankReview(
          candidates,
          k: 5,
          personSlug: 'reid-baker',
          passIndex: passIndex,
        ).comments.map((c) => c.path).toList();

    test('reserves two slots for craft catches in distinct categories', () {
      expect(
        paths([
          ...majors,
          craft('p1.dart', 'Use a pronoun here.', 5),
          craft('p2.dart', 'Just inline it here.', 6),
        ]),
        ['a.dart', 'b.dart', 'c.dart', 'p1.dart', 'p2.dart'],
      );
    });

    test('reserves one slot when craft catches share a category', () {
      expect(
        paths([
          ...majors,
          craft('p1.dart', 'Use a pronoun here.', 5),
          craft('p2.dart', 'Prefer the pronoun there.', 6),
        ]),
        ['a.dart', 'b.dart', 'c.dart', 'd.dart', 'p1.dart'],
      );
    });

    test('reserves one slot from pass 3 on', () {
      expect(
        paths([
          ...majors,
          craft('p1.dart', 'Use a pronoun here.', 5),
          craft('p2.dart', 'Just inline it here.', 6),
        ], passIndex: 3),
        ['a.dart', 'b.dart', 'c.dart', 'd.dart', 'p1.dart'],
      );
    });
  });
}
