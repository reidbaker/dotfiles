---
name: mayor-worker-prs
description: Rules for running one or more PRs through coordinator ("mayor") and worker subagents. Use when you hand PR work to subagents, act as a worker that opens or updates a PR for a coordinator, or route review feedback between the user and workers.
---

# Mayor-Worker PRs

One coordinator (the mayor) talks to the user and routes work. Workers each own one branch and one PR. The user's review attention is the scarcest resource. Every rule below exists to avoid spending it twice.

## Worker rules

Paste this section into every worker prompt. Workers can't be assumed to read this file.

1. **Identity.** Push over HTTPS as the agent account. Run `gh auth status` before any push or PR command. Push with an explicit URL: `git push https://github.com/<owner>/<repo>.git <branch>`.
2. **Fresh worktree.** Run the package's dependency fetch (`dart pub get`, `flutter pub get`) before you format, analyze or test. A missing fetch gives false failures.
3. **Never rewrite history on a branch with an open PR.** No rebase, amend, squash or force-push, not even for unpushed commits. Add new commits. Reviewers review one commit at a time, and a rewrite resets their progress.
4. **Squash-merged base.** When a PR this branch depends on is squash-merged, merge the default branch (`git merge origin/main`, or `master` where that is the default) right away without asking. Don't rebase.
5. **Verify before pushing.** Run the repo's full definition of done and wait for every command to finish. Paste the output tails into the report.
6. **CI on every OS is part of done.** Local runs can't catch Windows path bugs. After the push, report the CI result for each OS. Don't report "done" on local results alone. To wait for CI, run `gh pr checks <n> -R <owner>/<repo> --watch`.
7. **Don't push while a question is open.** If you asked the mayor something, wait for the answer.
8. **No silent deletions.** Name every removed comment, docstring, test or eval assertion in the PR body, with the reason.
9. **Scope.** Explain every change outside the PR's stated scope, or leave it out and list it in the report.
10. **Links.** Open every URL you add. Never invent one.
11. **PR descriptions and checklists.**
    - **Creating a PR:** Copy the checklist and link references verbatim from the repo's `.github/PULL_REQUEST_TEMPLATE.md` (never type the checklist from memory). Place agent-written technical details inside `<details><summary>Agent authored details</summary>...</details>` so the top of the PR stays reserved for a short human-readable summary.
    - **Updating a PR:** Always fetch the live PR body first (`gh pr view <N> -R <owner>/<repo> --json body -q .body > body.md`) and edit only inside `<details><summary>Agent authored details</summary>...</details>`. Never overwrite or regenerate the human-authored top section, the `<details>` wrapper, or the pre-launch checklist.
12. **Report** to the mayor: PR URL, head SHA, what changed, the definition-of-done tails, CI per OS, and any judgment calls the user should check.

## Mayor rules

1. **Before saying a PR needs review or checking PR status,** run `pr_triage.dart` on the tracked PRs from the devconfig repository root (this skill lives at `<root>/.agents/skills/mayor-worker-prs`):
   ```bash
   dart run bin/pr_triage.dart --pr <owner>/<repo>#<n1>,<owner>/<repo>#<n2>
   ```
   Use the returned `tier_name`, `action_prompt`, `reason`, `ci_status`, and `failing_checks` rather than hand-rolling `gh pr checks` or `gh api` loops. `pr_triage.dart` already ignores repository-wide checks (`tree-status`, `Check Code Freeze`), ignores settled self-review threads between the user's own accounts once approved, and keeps re-review PRs on the author until the author has pushed or replied after the user's last review. `pr_triage.dart` takes a snapshot; to wait for CI to finish, use `gh pr checks <n> -R <owner>/<repo> --watch`.
2. **Before filing an issue,** search open and closed issues: `gh issue list -R <owner>/<repo> --state all --search "<keywords>"`.
3. **Context summaries** list every open PR and every issue filed during the work, with numbers. A summary that drops an issue leads to duplicates.
4. **Workers can't see your context.** Put the hard rules (worker rules 3, 5, 6, 7 and 11 at least) in every prompt, including follow-up messages that ask for fixes.
5. **Route review comments whole.** Send the worker the comment URL and text. Don't paraphrase away the reviewer's reasoning.
6. **Keep shared briefs in a stable, writable place,** such as this skill. A brief inside one chat's artifact folder is lost when that chat ends.
7. **Stop idle workers and remove their worktrees** once their PRs merge.
8. **Test a support claim where it applies.** If a PR changes what it claims to support (an SDK lower bound, a minimum OS, a platform), tell the worker to run the dependency fetch, analysis and tests on that exact version or platform before pushing. A run on a newer toolchain proves nothing about the lower bound.
