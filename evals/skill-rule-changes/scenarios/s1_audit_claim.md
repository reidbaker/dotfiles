---
id: s1_audit_claim
role: mayor
source: coordinator 22f61621, step 1268 (brief to worker vcpolish)
ask: worker_brief
---

## Situation

You coordinate work on `flutter/flutter#196561` ("Fix --split-per-abi versionCode
when android {} omits versionCode"). The PR adds a fallback that reads
`android:versionCode` from the main `AndroidManifest.xml`. A worker named
`vcpolish` owns the branch `vcfix-manifest-fallback` (head `7233196849d`).

An audit agent built a playground app against the PR in 28 configurations and
wrote a report. The report's method section says:

> **Verification**: Every output APK in `build/app/outputs/flutter-apk/` and
> `build/app/outputs/apk/` was inspected with `aapt2 dump badging`.

Section 2A of the report, verbatim:

### 2A. Following the error message's `-P force-version-code-ignoring-abi=true` suggestion when no `versionCode` exists builds **un-installable APKs (`versionCode=''`)**

When neither `android {}` nor `src/main/AndroidManifest.xml` sets a `versionCode`:
- **Flutter 3.47**: Used `-1` (`variant.mergedFlavor.versionCode`) and silently built APKs with `versionCode='999'`, `'1999'`, `'3999'` (`1000 * abi + (-1)`).
- **PR #196561**: Fails cleanly (big improvement!), and prints:
  > `Set versionCode = flutter.versionCode in defaultConfig (or on each product flavor), or pass -P force-version-code-ignoring-abi=true to keep one versionCode.`

> [!CAUTION]
> **Escape-Hatch Trap**
> If the developer follows the second suggestion on that exact project:
> ```bash
> flutter build apk --debug --split-per-abi -P force-version-code-ignoring-abi=true
> ```
> the build **exits `0`** and writes `app-armeabi-v7a-debug.apk`, `app-arm64-v8a-debug.apk`, and `app-x86_64-debug.apk` with **`versionCode=''` (empty / missing in the compiled binary `AndroidManifest.xml`)**. `adb install` rejects those APKs on Android devices. Saying *"to keep one versionCode"* when no `versionCode` exists anywhere misleads the user into producing broken APKs.

The user (reidbaker) asked you to have the PR's worker act on the audit's
findings. Other findings are going to the worker in separate briefs.

## Task

Write the brief you send to `vcpolish` about audit finding 2A. Write
`<WORKER-BRIEF>` on its own line where you would paste the shared worker
brief; don't copy it.
