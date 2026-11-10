---
trigger: always_on
---

# Code review before ready

- Before you tell the user that the first draft of a PR is ready for their review, run `persona-review` and `adversarial-review` on it without asking, and fix or report what they find.
- After the first draft, decide yourself whether a change needs another review pass, based on its size and risk.
- Log every review pass so the `review-value-audit` skill can later judge whether the review skills pay off. Append one JSON object per line to `~/.agents/review-log.jsonl` (create it if missing):
  - After you apply a review pass: `{"event":"review","date":"<ISO date>","repo":"<owner/repo>","pr":<n>,"skill":"persona-review|adversarial-review","reviewed_sha":"<sha reviewed>","fix_shas":["<commits that applied findings>"],"findings":<n>,"applied":<n>,"declined":<n>,"summary":"<one line per applied finding>"}`.
  - When the user asks you to undo or rework something a review finding caused: `{"event":"undo","date":"...","repo":"...","pr":<n>,"skill":"...","finding":"<the finding>","user_words":"<the user's request, quoted>","summary":"<what you undid>"}`.
  - When you call a PR ready for the user: `{"event":"ready","date":"...","repo":"...","pr":<n>,"sha":"<head sha>"}`.
- Never put secrets or internal hostnames in the log.
