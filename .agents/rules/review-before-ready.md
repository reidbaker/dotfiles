---
trigger: always_on
---

# Code review before ready

- Before you tell the user that the first draft of a PR is ready for their review, run `persona-review` and `adversarial-review` on it without asking, and fix or report what they find.
- After the first draft, decide yourself whether a change needs another review pass, based on its size and risk.
- Log every review pass and its outcome so the user can judge whether the review skills are worth their cost. Append one JSON object per line to `~/.agents/review-log.jsonl` (create it if missing):
  - After a review pass: `{"event":"review","date":"<ISO date>","repo":"<owner/repo>","pr":<n>,"sha":"<head sha>","skill":"persona-review|adversarial-review","findings":<n>,"applied":<n>,"declined":<n>,"summary":"<one line on what changed>"}`.
  - When the user asks you to undo a change that a review finding caused: `{"event":"undo","date":"...","repo":"...","pr":<n>,"skill":"...","summary":"<what was undone and why>"}`.
  - When you see the PR merge: `{"event":"merged","date":"...","repo":"...","pr":<n>,"human_review_rounds":<n>,"human_comments":<n>}`, counting rounds and comments from human reviewers after you first called it ready.
- Never put secrets or internal hostnames in the log.
