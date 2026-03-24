---
name: dependabot-android
description:
  Fix dependabot PRs in flutter/packages that fail CI due to missing CHANGELOG/version bumps. Use when user mentions dependabot, fixing dependabot PRs, or version-check failures in flutter/packages. Do not use for non-dependabot flutter package issues.
---

# Dependabot Android Fix

Assumes dart, git, and gh (authenticated) are on PATH.

## Repo structure (non-obvious bits)

script/tool/bin/flutter_plugin_tools.dart — CLI for package manipulation (version bumps, changelog, version-check)
script/configs/ — CI inclusion/exclusion YAML configs
Plugin packages nest platform implementations as subfolders prefixed by parent name (e.g., packages/google_maps_flutter/google_maps_flutter_android/)
Only modify files under packages/. Never rebase or merge.

## Workflow

### 1. Inspect the PR
bash
gh pr view <PR_ID> --repo flutter/packages --json author,body,changedFiles,state,title,mergeStateStatus,mergeable

If closed/merged → done. Dependabot PR titles follow: [dependabot]: Bump <dep> from <old> to <new> in <path>.

### 2. Identify failing checks
bash
gh pr checks <PR_ID> --repo flutter/packages --json bucket,link,name,state --jq '.[] | select(.bucket == "fail")'

Ignore tree-status failures. The relevant failure is **“Linux repo_check”**.

### 3. Confirm it’s a simple CHANGELOG/version failure

The link field for “Linux repo_check” has format: https://cr-buildbucket.appspot.com/build/<BUILD_ID>

Resolve the build number:
bash
curl https://cr-buildbucket.appspot.com/prpc/buildbucket.v2.Builds/GetBuild --json '{"id":"<BUILD_ID>"}'

Then fetch full build steps using the number field as <BUILD_NUMBER>:
bash
curl 'https://cr-buildbucket.appspot.com/prpc/buildbucket.v2.Builds/GetBuild' \
  -H 'content-type: application/json' \
  -H 'accept: application/json' \
  -H 'origin: https://ci.chromium.org' \
  -H 'referer: https://ci.chromium.org/' \
  --data-raw '{"builder":{"project":"flutter","bucket":"try","builder":"Linux repo_checks"},"buildNumber":<BUILD_NUMBER>,"mask":{"fields":"id,builder,number,status,summaryMarkdown,steps"}}'

The response starts with )]}' — strip that prefix before parsing JSON. Look for **“Tasks failed: CHANGELOG and version validation”** in the steps. If present, this is a simple dependabot fix.

### 4. Checkout and identify modified packages
bash
gh pr checkout <PR_ID>
git diff main...HEAD

Derive <modified-packages> from changed paths:

packages/image_picker/image_picker_android/android/build.gradle → image_picker_android
packages/go_router/pubspec.yaml → go_router

Confirm with:
bash
dart run script/tool/bin/flutter_plugin_tools.dart version-check --check-for-missing-changes --base-branch=$(git merge-base HEAD origin/main)

Packages printing “No version change found” need updating.

### 5. Apply version bump + changelog
bash
dart run script/tool/bin/flutter_plugin_tools.dart update-release-info \
  --version=minimal \
  --changelog="Bumps <dep> from <old> to <new>" \
  --base-branch=$(git merge-base HEAD origin/main)

Commit with message: Bumps version and add changelog entry for <package>.

### 6. Validate

Cross-check changes against https://raw.githubusercontent.com/flutter/flutter/refs/heads/master/docs/ecosystem/contributing/README.md for CHANGELOG/pubspec conventions.
bash
dart run script/tool/bin/flutter_plugin_tools.dart version-check --packages=<modified-packages>

If passes → tell user changes are ready to push. If fails → explain why.