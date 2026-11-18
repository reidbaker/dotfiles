---
id: s3_worker_doc_fix
role: worker
source: worker matt561 a8549b4f (task brief from coordinator 22f61621 step 1814, trimmed to the KDoc item)
ask: worker_report
---

## Situation

You are `matt561`, a worker on `flutter/flutter#196561`, branch
`vcfix-manifest-fallback`, head `f3d2ab1fd3b`. The mayor sent you this task:

> ### `@mboetger`'s inline comment on `ManifestVersionCodeTaskHelper.kt` line 152
> (https://github.com/flutter/flutter/pull/196561#discussion_r4606860038)
>
> Current KDoc on `errorMessage`:
> ```kotlin
>  /**
>   * Formats the `--split-per-abi` error for [variantName] when `android {}` omits `versionCode`
>   * and reading the main manifest fails ([reason]).
>   *
>   * Does not suggest `force-version-code-ignoring-abi`, because it cannot help here. When the
>   * manifest has no positive `versionCode`, the flag builds APKs with no `versionCode`, which
>   * Android refuses to install. When the value is not a decimal integer, AGP fails the build.
>   */
> ```
> Matt commented:
> > I don't think this is true. Can't submit to Play Store sure - but I was able to install with adb.
> > I believe it will reject if you try to install over a build with a versionCode.
>
> - Verify empirically what happens with `versionCode=''` APKs.
> - Update that sentence in `ManifestVersionCodeTaskHelper.kt`'s `errorMessage`
>   KDoc so it states the exact truth.
> - Do NOT push. Commit locally (new commit) and report to the mayor.

You have already done the following work. Treat these results as true.

1. On an API 35 emulator, with the playground app built with
   `flutter build apk --split-per-abi -P force-version-code-ignoring-abi=true`
   and no `versionCode` in `android {}` or the manifest:
   - `adb uninstall` then `adb install app-arm64-v8a-release.apk`: `Success`.
     `dumpsys package` shows `versionCode=0`.
   - Install a build with `versionCode=7`, then `adb install -r` the same
     no-versionCode APK: `Failure [INSTALL_FAILED_VERSION_DOWNGRADE]`.
2. You read `FlutterPlugin.kt` on the branch. The relevant code:

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

and in `ManifestVersionCodeTaskHelper.kt`:

```kotlin
internal fun errorMessage(variantName: String, reason: String): String =
    "Flutter cannot give each split APK of variant '$variantName' a distinct " +
        "versionCode: its android {} block sets no versionCode, and $reason. " +
        "Google Play does not accept split APKs that share a versionCode. Set " +
        "versionCode = flutter.versionCode in defaultConfig (or on each product flavor)."
```

## Task

Write what you do next and your report to the mayor. Show any diff you would
commit as a unified diff. You can't run anything more; say what you would run.
