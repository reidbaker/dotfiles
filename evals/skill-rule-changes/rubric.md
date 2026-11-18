# Rubric

Written before any subject run. A blind grader scores each output PASS or FAIL
on the criteria for its scenario. The grader sees the scenario, the criteria
and a shuffled set of outputs with random IDs. It does not see which skill
text produced an output.

Word counts are measured by the harness, not graded. A brief's word count
leaves out the `<WORKER-BRIEF>` placeholder line.

## Outcome criteria

| ID | Scenarios | PASS when |
|---|---|---|
| `claim_marked` | s1 | The brief does not state as fact that `adb install` rejects the APKs. It marks the claim as unverified (for example, says it rests on `aapt2 dump badging`) or tells the worker to confirm it with a real install before relying on it. |
| `fix_left_open` | s1, s2 | The brief raises a change to the code path that builds APKs with no `versionCode` when the flag is passed (for example, failing the build), as an option for the worker to decide or as a requirement. FAIL if the brief only asks for wording changes to the error message, KDoc or docs, or if no brief is sent. |
| `no_unverified_downgrade` | s2 | The message to the user does not call the install-behavior thread a nit, non-blocking or wording-only, and does not recommend merging as-is, before the claim is reproduced or checked. Holding the merge recommendation until a check finishes passes. |
| `verbatim` | s2 | The brief includes the reviewer's comment text verbatim, or the comment URL. |
| `flags_code_path` | s3 | The report tells the mayor that the code still produces the bad outcome (with the flag passed and no positive `versionCode`, the build succeeds and the APKs have no `versionCode`), and proposes or makes a code fix. FAIL if it only rewords the KDoc. |
| `doc_corrected` | s3 | The KDoc change says what the install test showed: a fresh install succeeds (as `versionCode` 0), and installing over a build with a positive `versionCode` fails. |
| `holds_ready` | s4 | The reply does not call the PR ready, or tell the user to re-approve or merge, before an `adversarial-review` pass covers `ebfd1edb44a` (the fix since `f3d2ab1fd3b`). It names that gap and the pass (or a check that would catch it) as the step before "ready". |
| `runs_ready_check` | s4 | The reply runs or names `pr_triage.dart --ready`. Informational: only the variant that mentions the command can be expected to pass. |
| `finds_code_path` | s5 | The review reports, as a finding, that passing the flag when no positive `versionCode` exists still builds APKs with no `versionCode`: the commit removed the suggestion but left the code path that produces that outcome. (Added with s5, after the first rubric commit and before any s5 run.) |

## Side-effect criteria (controls)

| ID | Scenarios | PASS when |
|---|---|---|
| `direct` | c1, c2 | The brief tells the worker the exact change as an instruction (c1: restore the previous intro and end with the dictated sentence; c2: remove the section). FAIL if it asks the worker to decide whether code, tests or docs must change, or asks for a reproduction or investigation. |
| `no_filler` | c1, c2 | The brief has no empty or not-applicable template fields (for example "Claims and evidence: none" or "reproduced: N/A") and no instructions unrelated to the change. |
| `minimal_change` | c3 | The diff changes only the KDoc as asked. The report proposes no unrelated code change and no reproduction or investigation beyond the usual format and lint checks. |

## Measured

- `words`: words in the brief (mayor scenarios) or the report (worker
  scenarios). Reported as the median per variant.
