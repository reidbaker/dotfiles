---
trigger: always_on
---

# `flutter/flutter` and `flutter/packages` Flaky Presubmit Re-runs

- On `flutter/flutter` and `flutter/packages` pull requests — and especially bot-authored PRs during a release (`flutteractionsbot`, `reidbaker-agent`, autorollers) — actively monitor presubmit checks, automatically re-run failed checks **once** without asking the user first when failures are flaky/unrelated (`INFRA_FAILURE`, missing prebuilt engine artifacts while `flutter_release_builder` is still uploading, unrelated shards, or known branch-wide flakes), and flag the user immediately if a failure looks real.
- Before triggering **Re-run failed** on the Flutter Presubmit Dashboard in work Chrome, verify that no retry is already running or scheduled for that commit (`bb ls` `SCHEDULED` / `STARTED`).
- Only auto-rerun **once** per failure; if the retry fails again, inspect the failure logs and report to the user instead of looping.
- When an already-`APPROVED` PR had `autosubmit` stripped by `auto-submit[bot]` solely because of that flaky/unrelated failure, automatically re-add `autosubmit` (as `reidbaker`) once `Dashboard Checks` and `Merge Queue Guard` turn `SUCCESS` — do not ask first.
- To re-run, use the `flutter-presubmit-rerun` skill (`~/.agents/skills/flutter-presubmit-rerun/SKILL.md`; source in `~/devconfig/.agents/skills/`). Command: `dart ~/.agents/skills/flutter-presubmit-rerun/bin/rerun_presubmit.dart <flutter|packages> <pr> <head-sha>`. There is no API path: the GitHub check-run rerequest returns 404 and `bb add` does not update Cocoon or the GitHub check.
- On the Flutter Postsubmit Dashboard (`get-status` API), tasks with `"IsFlaky": true` or `"IsBringup": true` (shown with the flaky/bringup indicator at the top of the column UI) are non-blocking (`bringup: true` in `.ci.yaml`) and do **not** need to be re-run for release validation.
