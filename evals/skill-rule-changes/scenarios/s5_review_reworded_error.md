---
id: s5_review_reworded_error
role: reviewer
source: fix commit 254132ca9de on flutter/flutter#196561 (worker vcpolish), reviewed by the coordinator's adversarial-review pass
ask: review_findings
---

## Situation

You are reviewing a fix commit on `flutter/flutter#196561` ("Fix
--split-per-abi versionCode when android {} omits versionCode"). An audit found
that the PR's error message told users to pass
`-P force-version-code-ignoring-abi=true` even when no `versionCode` exists
anywhere. The commit's stated goal: "Stop suggesting
force-version-code-ignoring-abi when it cannot help."

The commit's change to `ManifestVersionCodeTaskHelper.kt`:

```diff
     /**
      * Formats the `--split-per-abi` error for [variantName] when `android {}` omits `versionCode`
      * and reading the main manifest fails ([reason]).
+     *
+     * Does not suggest `force-version-code-ignoring-abi`, because it cannot help here. When the
+     * manifest has no positive `versionCode`, the flag builds APKs with no `versionCode`, which
+     * Android refuses to install. When the value is not a decimal integer, AGP fails the build.
      */
     internal fun errorMessage(
         variantName: String,
         reason: String
     ): String =
         "Flutter cannot give each split APK of variant '$variantName' a distinct " +
             "versionCode: its android {} block sets no versionCode, and $reason. " +
             "Google Play does not accept split APKs that share a versionCode. Set " +
-            "versionCode = flutter.versionCode in defaultConfig (or on each product " +
-            "flavor), or pass -P ${FlutterPluginUtils.PROP_FORCE_VERSION_CODE_IGNORING_ABI}=true to keep one versionCode."
+            "versionCode = flutter.versionCode in defaultConfig (or on each product flavor)."
```

The code that applies the per-ABI versionCodes, in `FlutterPlugin.kt`,
unchanged by the commit:

```kotlin
/**
 * Sets each `--split-per-abi` output's `versionCode` to `ABI_VERSION[abi] * 1000 + base`.
 *
 * Uses [baseVersionCode] from the DSL ([DslVersionCodes]), or falls back to
 * `android:versionCode` in the main `AndroidManifest.xml`
 * ([ManifestVersionCodeTaskHelper]). If neither sets a valid `versionCode`, the build fails
 * because Google Play rejects split APKs that share one.
 *
 * Skipped with `force-version-code-ignoring-abi`. Outputs without an ABI filter are left
 * unchanged.
 */
private fun configureSplitPerAbiVersionCodes(
    project: Project,
    variant: ApplicationVariant,
    baseVersionCode: Int?
) {
    if (!FlutterPluginUtils.shouldProjectSplitPerAbi(project) ||
        FlutterPluginUtils.shouldForceVersionCodeIgnoringAbi(project)
    ) {
        return
    }
    val manifestVersionCode by lazy { ManifestVersionCodeTaskHelper.registerTask(project, variant) }
    val variantName = variant.name
    variant.outputs.forEach { output ->
        val abi = CopyFlutterApksTask.abiOf(output.filters)
        val abiVersionCode = FlutterPluginConstants.ABI_VERSION[abi] ?: return@forEach
        if (baseVersionCode != null) {
            output.versionCode.set(splitApkVersionCode(variantName, abi, abiVersionCode, baseVersionCode))
        } else {
            output.versionCode.set(
                manifestVersionCode.map { splitApkVersionCode(variantName, abi, abiVersionCode, it) }
            )
        }
    }
}
```

## Task

Review this commit with the method in the skill. You can't run anything, so
work from the code shown and give each finding its evidence tier. List your
findings, most severe first, or say there are none.
