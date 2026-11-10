---
name: review-value-audit
description: >-
  Judges whether persona-review and adversarial-review improved merged PRs, using
  ~/.agents/review-log.jsonl, PR history and conversation transcripts. Use when the
  user asks whether the review skills are worth their cost, or on a periodic audit.
  Don't use to review a PR.
---

# Review value audit

The review rule logs each review pass to `~/.agents/review-log.jsonl`. This skill turns that log into a judgment: did the changes the reviews caused survive, did the user correct or undo them, and what did the reviews miss. A script can count; this needs judgment on intent and on how much each change mattered.

## Inputs

- `~/.agents/review-log.jsonl`: `review`, `ready` and `undo` events, plus `assessment` events from earlier audits. Skip PRs that already have an assessment unless they changed since.
- PR history: `gh pr view`, `gh api repos/<repo>/pulls/<n>/comments`, `/reviews`, `/commits`, and `git log -p` on the merged branch.
- Conversation transcripts in `~/.gemini/jetski/brain/*/.system_generated/logs/transcript.jsonl`: the user's messages, for undo or rework requests that no agent logged.

## Steps

1. Pick the PRs: every PR in the log that merged or closed since the last audit. Fan out one low-effort subagent per PR within the quota rule.
2. For each PR, list the review-driven changes: diff each `fix_shas` commit, and split it into one change per applied finding.
3. Trace each change to the final state of the PR and classify it:
   - **kept**: it reached the merge unchanged or with cosmetic edits.
   - **corrected**: a human asked for it to be changed.
   - **undone**: a human asked for it to be removed, or a later commit removed it. Use the transcripts and PR threads to find who asked and why.
   - **moot**: the code it touched was removed for unrelated reasons.
4. Judge intent for every corrected or undone change. Was the finding wrong, right but out of scope, against the user's taste, or overtaken by a later decision? Only a wrong finding or an unwanted scope expansion counts against the skill.
5. Find misses: human review comments after the `ready` event that the review should have caught. Ignore taste-only comments.
6. Weight each item by consequence, not by line count: high (a bug, security issue, wrong public API or user-facing claim, or a release or revert), medium (maintainability or test quality), low (naming, wording, style).
7. Append one `assessment` event per PR to the log: `{"event":"assessment","date":"...","repo":"...","pr":<n>,"skill":"...","kept":{"high":n,"medium":n,"low":n},"corrected":{...},"undone":{...},"missed":{...},"notes":"<one line>"}`.
8. Write a short report to `~/Documents/agent-handoffs/review-value/<date>.md`: per skill, net value (weighted kept minus weighted wrong undos and misses), the two or three most telling examples with links, and any change you'd suggest to the review skills.

## Judgment rules

- A finding the user later reversed because their own plan changed is not a failure of the review.
- One high-weight catch outweighs many low-weight nits; say so when it drives the result.
- If the log is missing events for a PR, reconstruct them from history and say so in `notes`.
- Report numbers with their sample size. Fewer than 10 PRs is an anecdote, not a trend.
