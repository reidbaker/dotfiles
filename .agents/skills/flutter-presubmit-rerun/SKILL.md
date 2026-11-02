---
name: flutter-presubmit-rerun
description: Re-runs failed presubmit checks on a flutter/flutter or flutter/packages PR by clicking Re-run failed on the Flutter Presubmit Dashboard in work Chrome. Use when a failed check is flaky, an infra failure, unrelated to the PR, or read the PR labels before they were added. Do not use for real failures or when a retry is already scheduled or running.
---

# Flutter Presubmit Re-run

Agents have no API for this. The GitHub check-run rerequest endpoint returns
404, and `bb add` starts a build without updating Cocoon or the GitHub check.
The only path is the dashboard's **Re-run failed** button, driven through
Chrome with AppleScript.

## Prerequisites (macOS)

- Google Chrome is running with a `.corp.google.com` tab open. The script uses
  that window, which has the signed-in work profile.
- Chrome: View > Developer > **Allow JavaScript from Apple Events** is on.
- System Settings > Privacy & Security > Automation lets your terminal control
  Google Chrome.

## 1. Pre-check

Re-run only once per failure, and never while a retry is pending.

```bash
gh pr checks <pr> -R flutter/<repo>
bb ls -predicate '{"tags":[{"key":"buildset","value":"sha/git/<sha>"}]}' -json \
  | jq -r 'select(.status=="SCHEDULED" or .status=="STARTED") | "\(.status) \(.builder.builder)"'
```

Stop if the `bb ls` command prints anything for the failed builder: a retry is
already in flight. Stop if the failure looks real, or if this failure was
already re-run once. In those cases, read the logs and report to the user.

## 2. Run

`<sha>` is the PR head SHA (`gh pr view <pr> -R flutter/<repo> --json headRefOid`).
`<repo>` is `flutter` or `packages`.

```bash
dart ~/.agents/skills/flutter-presubmit-rerun/bin/rerun_presubmit.dart <repo> <pr> <sha>
```

Use the work Flutter `dart` if `dart` is not on `PATH`. A `Waiting for another
flutter command to release the startup lock` line is harmless.

## 3. Read the output

| Exit | Output | Meaning |
|---|---|---|
| 0 | `Click result: CLICKED ...` | Re-run requested. |
| 2 | `No Google Chrome window ...` | Open work Chrome and load a corp page. |
| 3 | `button not found` | Dashboard did not load or is signed out. |
| 4 | `button is disabled` | Nothing to re-run, or a retry is running. |
| 1 | `osascript failed ...` | Follow the printed hint (Chrome setting or Automation permission). |
| 64 | `Usage: ...` | Bad arguments. |

`Cleanup tab: CLOSED` means the tab was closed and the old active tab restored.
After exit 0, run the `bb ls` command again within a minute or two. The failed
builders should show `SCHEDULED` or `STARTED`.

## 4. Report

Tell the user which checks you re-ran, why they look flaky or unrelated (link
the failing build), and the click result. If the retry fails again, do not run
the script a second time. Read the new logs and report.
