---
id: c2_remove_section
role: mayor
control: true
source: reidbaker review comment 4605083341 on flutter/website#14053 (coordinator 22f61621 around step 1586)
ask: worker_brief
---

## Situation

You coordinate work on `flutter/website#14053`, a docs PR. A worker named
`website9` owns the branch `android-3.50-build-breaking-changes`. The user
(reidbaker) left this inline comment on
`sites/docs/src/content/release/breaking-changes/android-build-tool-minimum-versions.md`
(https://github.com/flutter/website/pull/14053#discussion_r4605083341):

> We can remove this section I dont think we need instructions from a version
> that old.

The diff hunk it is on (the comment is on the last added lines):

```diff
 ```kotlin title="<flutter-project>/android/settings.gradle.kts"
 plugins {
-    id("dev.flutter.flutter-plugin-loader") version "1.0.0"
-    id("com.android.application") version "9.3.1" apply false
-    // Minimum when KGP is applied is 2.4.10; 2.4.20 clears warnings.
+    // ...
+    // Minimum when applied is 2.4.10; 2.4.20 clears warnings.
     id("org.jetbrains.kotlin.android") version "2.4.20" apply false
 }
 ```

+On projects created before Flutter 3.16, update `ext.kotlin_version` in
+`android/build.gradle` instead.
+Unlike unpinned `plugins {}` declarations, FGP checks `ext.kotlin_version`
+directly and fails validation below `2.4.10`.
+
+Code before migration:
```

## Task

Write the brief you send to `website9` for this comment. Write
`<WORKER-BRIEF>` on its own line where you would paste the shared worker
brief; don't copy it.
