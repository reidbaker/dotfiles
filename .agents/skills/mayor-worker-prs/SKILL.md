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

## Rounds and reviewers

1. **Re-query before listing.** Re-query live PR and issue state in the same turn before you tell the user what waits on them. Never list from memory. (A merged PR was once listed 48 minutes after it merged.)
2. **Re-scan before calling a round done.** From the devconfig root, run `dart run bin/pr_triage.dart --threads --repo <owner>/<repo> --agent <agent-login>`. Handle every unanswered thread from *any* reviewer, including comments posted while the round ran and comments on PRs merged in the last 2 days (`--include-merged`). (A mid-round comment was once missed, and a reviewer's 22 comments sat for 4 days.)
3. **Watch with one cron.** Use one combined cron for new review comments across all tracked PRs, never one per PR or duplicates. When it finds a comment, dispatch the owning worker at once.
4. **Humans who reviewed by hand get replies from the user, not from agents.** Agents may change code and draft replies locally. If you're unsure whether a person wrote a comment by hand, ask. (22 agent replies to a human reviewer had to be deleted.)
5. **Weigh feedback; don't just obey it.** Decide each comment on its merits and report it as changed, modified or declined. Exception: if the user agreed to a reviewer's suggestion, do it unless it breaks code or maintainability.
6. **Reply with direct suggestions.** A reply to the maintainer states the change or the proposal itself. Never deflect by citing an earlier discussion ("Option A").
7. **Review each file + whole-PR architecture before asking for re-review.** Before you ask the maintainer to re-review a PR that is close to landing, run the review panel across the changed files (one changed file at a time; a whole-PR diff in one prompt misses most of what the maintainer later flags) plus one whole-PR architecture/duplication pass:
   - **Dart files (per changed file):**
     - **Lasse persona** ([`references/personas/lasse.md`](references/personas/lasse.md))
     - **Jake persona** ([`references/personas/jake.md`](references/personas/jake.md))
     - **Adversarial review** (`adversarial-review` skill)
   - **Markdown, docs, and skill files (per changed file):**
     - **Stuart persona** ([`references/personas/stuart.md`](references/personas/stuart.md))
   - **Whole-PR architecture & duplication pass (`arch_stuart`, once per PR):**
     - Load the **Stuart persona** ([`references/personas/stuart.md`](references/personas/stuart.md)) with the full PR diff and a searchable checkout of the whole repository, using this task brief:
       > *"This pass is NOT a line-by-line review: other reviewers cover each changed file on its own. Look only for problems that need more than one file to see: (1) new code that duplicates existing code in the repository (search with grep/rg for similar functions, regexes, parsers, helpers, constants and test setup), or that should reuse an existing helper, model, or a well-known Dart package or tool; (2) code, classes or tests placed in the wrong file or layer, or a new file where an existing one should be extended; (3) other places in the repository (docs, examples, skills, README, RULES.md, tests, config) that should change with this PR but did not; (4) interactions with existing features, rules, options or configuration that the PR does not handle; (5) boundary & single-source-of-truth leaks across files: raw `Map<String, dynamic>`, untyped `Object?` option bags, boolean origin flags (`isFromFile`), or 3+ positional `String` parameters passed across internal layers instead of typed domain records/classes, or default parameter values/constants duplicated between a registry/caller and a rule class. For each finding, cite the existing code as evidence (`path:line` in the repo, or the package/tool name)."*
   - **Model routing:** Run `arch_stuart` on **Argon (`gemini-launch-candidate-low`)**. Run per-file reviewers on `opus-5.5-low` when Opus quota is healthy, or on **Argon (`gemini-launch-candidate-low`)** when conserving Opus quota. **Never use `flash` models for code review.** Weigh `BLOCKER` and `SHOULD` findings like any other review (rule 5), and fix the ones that hold up first.
8. **Skill or eval PRs:** post a fresh eval report at the final head commit before merge.
9. **Ask one question at a time.** Give what it is, what breaks, the options, their consequences and a recommendation.
10. **Stacked PRs don't land into main.** Each layer merges into a feature branch. Review the layers as stacked PRs, on the fork if the upstream can't host the branches. Then open the feature branch against main as one PR. Never rebase; merge the layer below into the layer above.
