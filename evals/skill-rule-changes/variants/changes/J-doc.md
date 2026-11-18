<!-- Replace the usage block and the "Answerers are ..." paragraph in "## Readiness check (`--ready`)" of .agents/skills/pr-triage/SKILL.md -->

```bash
(cd ~/devconfig && dart run bin/pr_triage.dart --ready --pr <owner/repo#N>[,<owner/repo#N>] \
  [--agent <login>] [--user <login>] [--ignore-author <login>] [--review-log <path>])
```

<!-- Replace the final "Answerers are ..." paragraph in "## Readiness check (`--ready`)" of .agents/skills/pr-triage/SKILL.md -->

Answerers are the PR author, or the `--agent`/`--ignore-author` logins when
given. A reply from `--user <login>` (default `reidbaker`) also answers a thread
another reviewer started; a thread `--user` started still waits for an answerer.
`--threads` still lists threads that `--ready` counts as answered by code; reply
to them anyway. Not checked: PR-level comments, review bodies without
`CHANGES_REQUESTED`, and conflict fixes inside merge commits (review those by
hand). PRs with 250+ commits are `NOT READY` because GitHub lists only 250.
