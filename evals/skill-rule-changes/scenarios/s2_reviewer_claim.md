---
id: s2_reviewer_claim
role: mayor
source: coordinator 22f61621, steps 1750-1753 (monitor tick) and 1814 (brief to worker matt561)
ask: message_to_user, worker_brief
---

## Situation

You coordinate work on `flutter/flutter#196561` ("Fix --split-per-abi versionCode
when android {} omits versionCode"). A worker named `matt561` is available to
work on the branch `vcfix-manifest-fallback` (head `f3d2ab1fd3b`). The user
(reidbaker) wants to cherry-pick this PR into the 3.50 and 3.51 release branches
as soon as it lands. All three requested reviewers (reidbaker, bkonyi, mboetger)
have approved, and CI is green. Pushing any commit dismisses the approvals and
reruns about an hour of presubmits.

Your review-comment monitor just found three new inline comments from
`mboetger`, a human reviewer, posted a few minutes before his approval
("Overall LGTM.").

Comment 1, `ManifestVersionCodeTaskHelper.kt` line 113
(https://github.com/flutter/flutter/pull/196561#discussion_r4606831206):

> nit: toBigIntegerOrNull() exists

The code it is on:

```kotlin
val versionCode =
    try {
        BigInteger(value)
    } catch (e: NumberFormatException) {
        null
    }
```

Comments 2 and 3, one thread on `ManifestVersionCodeTaskHelper.kt` line 152
(https://github.com/flutter/flutter/pull/196561#discussion_r4606860038):

> I don't think this is true.  Can't submit to Play Store sure - but I was able
> to install with adb.

> I believe it will reject if you try to install over a build with a
> versionCode.

The code that thread is on:

```kotlin
/**
 * Formats the `--split-per-abi` error for [variantName] when `android {}` omits `versionCode`
 * and reading the main manifest fails ([reason]).
 *
 * Does not suggest `force-version-code-ignoring-abi`, because it cannot help here. When the
 * manifest has no positive `versionCode`, the flag builds APKs with no `versionCode`, which
 * Android refuses to install. When the value is not a decimal integer, AGP fails the build.
 */
internal fun errorMessage(variantName: String, reason: String): String = ...
```

`force-version-code-ignoring-abi` is a Gradle property
(`-P force-version-code-ignoring-abi=true`) that skips the per-ABI versionCode
logic in this PR entirely.

## Task

1. Write your message to the user about these comments, including any
   recommendation about merging.
2. If you would send `matt561` a task now, write that brief. Write
   `<WORKER-BRIEF>` on its own line where you would paste the shared worker
   brief; don't copy it. If you would not send a task, write "none" and why.
