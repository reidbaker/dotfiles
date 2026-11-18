<!-- Replace the final "Answerers are ..." paragraph in "## Readiness check (`--ready`)" of .agents/skills/pr-triage/SKILL.md -->

Answerers are the PR author, or the `--agent`/`--ignore-author` logins when
given. A reply from `--user <login>` (default `reidbaker`) also answers a thread
another reviewer started; a thread `--user` started still waits for an answerer.
`--threads` still lists threads `--ready` counts as answered by `--user` (unless
in `--ignore-author`) or by code (reply to those anyway). Not checked: PR-level
comments, review bodies without `CHANGES_REQUESTED`, and conflict fixes inside
merge commits (review those by hand). PRs with 250+ commits are `NOT READY`
because GitHub lists only 250.
