---
name: mayor-worker-prs
description: Rules for running one or more PRs through coordinator ("mayor") and worker subagents. Use when you hand PR work to subagents, act as a worker that opens or updates a PR for a coordinator, route review feedback between the user and workers, reply to review threads, run stacked PRs, watch for new review comments, or list what is waiting on the user.
---

# Mayor-Worker PRs

One coordinator (the mayor) talks to the user and routes work. Workers each own one branch and one PR. The user's review attention is the scarcest resource. Every rule below exists to avoid spending it twice.

## Worker rules

Paste the text of [references/worker-brief.md](references/worker-brief.md) into every worker prompt, including follow-up fix requests. Don't send only the path: workers may not be able to read this directory.

## Mayor rules

1. **Before saying a PR needs review or checking PR status,** run `pr_triage.dart` on the tracked PRs from the devconfig repository root (this skill lives at `<root>/.agents/skills/mayor-worker-prs`):
   ```bash
   dart run bin/pr_triage.dart --pr <owner>/<repo>#<n1>,<owner>/<repo>#<n2>
   ```
   Use the returned `tier_name`, `action_prompt`, `reason`, `ci_status`, and `failing_checks` rather than hand-rolling `gh pr checks` or `gh api` loops. `pr_triage.dart` already ignores repository-wide checks (`tree-status`, `Check Code Freeze`), ignores settled self-review threads between the user's own accounts once approved, and keeps re-review PRs on the author until the author has pushed or replied after the user's last review. `pr_triage.dart` takes a snapshot; to wait for CI to finish, use `gh pr checks <n> -R <owner>/<repo> --watch`.
2. **Before filing an issue,** search open and closed issues: `gh issue list -R <owner>/<repo> --state all --search "<keywords>"`.
3. **Context summaries** list every open PR and every issue filed during the work, with numbers. A summary that drops an issue leads to duplicates.
4. **Workers can't see your context.** Paste the worker brief into every prompt, including follow-up fix requests.
5. **Route review comments whole.** Send the worker the comment URL and text. Don't paraphrase away the reviewer's reasoning.
6. **Keep shared briefs in a stable, writable place,** such as this skill. A brief inside one chat's artifact folder is lost when that chat ends.
7. **Stop idle workers and remove their worktrees** once their PRs merge.
8. **Test a support claim where it applies.** If a PR changes what it claims to support (an SDK lower bound, a minimum OS, a platform), tell the worker to run the dependency fetch, analysis and tests on that exact version or platform before pushing. A run on a newer toolchain proves nothing about the lower bound.
9. **Write decisions down.** Keep open questions and the user's answers in the decisions file the user named, not only in chat. After a restart or a quota reset, re-read it and restate what still waits on the user before you assign work.
10. **Challenge early designs with orthogonal personas.** Before dispatching workers on a multi-file feature, RFC, or architectural change, run the Stage A design pair from the [`persona-review`](../persona-review/SKILL.md) skill: pair a **Scope & Simplicity Challenger** (**Reid** [`../persona-review/references/personas/reid.md`](../persona-review/references/personas/reid.md), **David** [`../persona-review/references/personas/david.md`](../persona-review/references/personas/david.md), or **Stuart** [`../persona-review/references/personas/stuart.md`](../persona-review/references/personas/stuart.md)) with a **Formal Invariant & Ergonomics Architect** (**Jake** [`../persona-review/references/personas/jake.md`](../persona-review/references/personas/jake.md) or **Paul** [`../persona-review/references/personas/paul.md`](../persona-review/references/personas/paul.md)).

## Rounds and reviewers

1. **Re-query before listing.** Re-query live PR and issue state in the same turn before you tell the user what waits on them. Never list from memory. (A merged PR was once listed 48 minutes after it merged.)
2. **Re-scan before calling a round done.** From the devconfig root, run `dart run bin/pr_triage.dart --threads --repo <owner>/<repo> --agent <agent-login>`. Handle every unanswered thread from *any* reviewer, including comments posted while the round ran and comments on PRs merged in the last 2 days (`--include-merged`). (A mid-round comment was once missed, and a reviewer's 22 comments sat for 4 days.)
3. **Watch with one cron.** Use one combined cron for new review comments across all tracked PRs, never one per PR or duplicates. When it finds a comment, dispatch the owning worker at once.
4. **Humans who reviewed by hand get replies from the user, not from agents.** Agents may change code and draft replies locally. If you're unsure whether a person wrote a comment by hand, ask. (22 agent replies to a human reviewer had to be deleted.)
5. **Weigh feedback; don't just obey it.** Decide each comment on its merits and report it as changed, modified or declined. Exception: if the user agreed to a reviewer's suggestion, do it unless it breaks code or maintainability.
6. **Reply with direct suggestions.** A reply to the maintainer states the change or the proposal itself. Never deflect by citing an earlier discussion ("Option A").
7. **Run [`persona-review`](../persona-review/SKILL.md) before asking for re-review.** Before you ask the maintainer to re-review a PR that is close to landing, run the domain-routed review panel and 3-stage pipeline (`Locus Detector -> Focused Critic -> Stratified Ranker`, `dart run bin/review_pipeline.dart`) plus the `arch_stuart` whole-PR architecture/duplication pass defined in [`persona-review`](../persona-review/SKILL.md):
   - **Dart libraries & tests (`*.dart`):** **Lasse** ([`../persona-review/references/personas/lasse.md`](../persona-review/references/personas/lasse.md)) + **[`adversarial-review`](../adversarial-review/SKILL.md)** (+ **Reid** [`../persona-review/references/personas/reid.md`](../persona-review/references/personas/reid.md) for CLI/tooling/test craft; keep **Jake** [`../persona-review/references/personas/jake.md`](../persona-review/references/personas/jake.md) for Stage A design/ergonomics or `pubspec.yaml`/`build.yaml` changes rather than line-by-line Dart code).
   - **CI workflows, release scripts & infra (`.github/workflows/*.yaml`, `.ci.yaml`, `*.sh`):** **Codefu** ([`../persona-review/references/personas/codefu.md`](../persona-review/references/personas/codefu.md)) + **Reid** ([`../persona-review/references/personas/reid.md`](../persona-review/references/personas/reid.md)).
   - **Android & Gradle (`*.gradle*`, `*.kt`, `*.java`):** **Reid** ([`../persona-review/references/personas/reid.md`](../persona-review/references/personas/reid.md)) + **[`adversarial-review`](../adversarial-review/SKILL.md)**.
   - **C/C++ engine & embedder ABI (`*.cc`, `*.cpp`, `*.h`, `CMakeLists.txt`):** **Loïc** ([`../persona-review/references/personas/loic.md`](../persona-review/references/personas/loic.md)) + **Chris** ([`../persona-review/references/personas/chris.md`](../persona-review/references/personas/chris.md)).
   - **iOS/macOS Darwin embedder & GN (`*.mm`, `*.m`, `*.swift`, `BUILD.gn`, `.gni`):** **Chris** ([`../persona-review/references/personas/chris.md`](../persona-review/references/personas/chris.md)) + **Stuart** ([`../persona-review/references/personas/stuart.md`](../persona-review/references/personas/stuart.md)).
   - **Markdown, docs, and skill files (`*.md`, `SKILL.md`, `CHANGELOG.md`):** **Stuart** ([`../persona-review/references/personas/stuart.md`](../persona-review/references/personas/stuart.md)) + **Reid** ([`../persona-review/references/personas/reid.md`](../persona-review/references/personas/reid.md)).
   - **Whole-PR architecture & duplication pass (`arch_stuart`, once per PR):** Run **Stuart** ([`../persona-review/references/personas/stuart.md`](../persona-review/references/personas/stuart.md)) on **Argon (`gemini-launch-candidate-low`)** with the `arch_stuart` brief in [`persona-review`](../persona-review/SKILL.md). **Never use `flash` models for code review.** Weigh `BLOCKER` and `SHOULD` findings like any other review (rule 5), and fix the ones that hold up first.
8. **Skill or eval PRs:** post a fresh eval report at the final head commit before merge.
9. **Ask one question at a time.** Give what it is, what breaks, the options, their consequences and a recommendation.
10. **Stacked PRs don't land into main.** Each layer merges into a feature branch. Review the layers as stacked PRs, on the fork if the upstream can't host the branches. Then open the feature branch against main as one PR. Never rebase; merge the layer below into the layer above.
