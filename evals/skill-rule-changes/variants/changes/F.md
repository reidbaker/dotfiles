<!-- Append at the end of Mayor rule 12 ("Re-review fix commits.") in "## Mayor rules" of .agents/skills/mayor-worker-prs/SKILL.md -->

Before saying "ready" or recommending a merge, run `(cd ~/devconfig && dart run bin/pr_triage.dart --ready --pr <owner>/<repo>#<n>)` and quote its verdict.
