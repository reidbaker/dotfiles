# Eval: skill-rule changes from reidbaker/dotfiles#8

Tests whether the rule changes proposed in reidbaker/dotfiles#8 change agent
behavior, and what they cost on small tasks where a short, direct brief is
right.

## Method

- **Scenarios** (`scenarios/`): five situations replayed from a real
  coordinator session and its workers, plus three controls. Each gives the
  subject only what the original agent had, without the outcome.
  - s1: route an audit claim that rests on an artifact dump, not on the
    behavior it names.
  - s2: a monitor tick finds a human reviewer's claim of wrong behavior on an
    approved PR the user wants to cherry-pick.
  - s3 (worker): asked to reword a KDoc sentence that describes a bad outcome
    the code still produces.
  - s4: decide whether a PR is ready after an unreviewed fix commit.
  - s5 (reviewer): review a fix that rewords an error message but leaves the
    code path behind it.
  - c1, c2 (controls): route a comment where the user dictated the exact
    change (replace wording; remove a section).
  - c3 (control, worker): apply a dictated KDoc change.
- **Variants** (`variants/`, built by `run_eval.dart` from the PR's base commit
  `3baa97a`, which was `origin/master` when the eval ran, plus the change texts
  in `variants/changes/`):

  | Variant | Text |
  |---|---|
  | `mayor_V0` | mayor-worker-prs at the base commit |
  | `mayor_V1_rules` | V0 + C (rule 5), D (Mayor 15), E (Rounds 13), K (Rounds 14) |
  | `mayor_V2_rules_template` | V1 + H (top line) + I (task-brief template) |
  | `mayor_F_ready` | V0 + F (one `--ready` sentence in Mayor rule 12) |
  | `worker_W0` / `worker_G` | worker-brief at the base commit / + G (rule 25) |
  | `adv_A` / `adv_AB` | adversarial-review with A (Proxy tier and question 7) / + B (hypothesis) |

  All change texts are generic (no incident examples), as they would ship.
- **Subjects:** a fresh `agy -p` run per scenario, variant and repetition, on
  Claude Opus 5.5 (High), the model the original coordinator and workers used.
  Each run gets only the skill text and the scenario, in an empty directory,
  and is told not to use tools. All 60 runs ended in one turn. 3 runs per cell.
- **Grading:** `rubric.md` was committed before any run (s5 and its criterion
  were added later, before s5 ran). One grader per scenario (same model) saw
  that scenario's outputs in a seeded shuffle, numbered `out-01`, `out-02`, and
  so on, with no variant labels,
  and scored each criterion PASS or FAIL with a quoted reason. Raw outputs are
  in `results/raw/`, grades and the ID key in `results/grades/`.

Rerun: `dart run evals/skill-rule-changes/run_eval.dart` (resumes; use
`--only <scenario>` and `--phase subjects|grade|report`).

## Results

Passes out of 3 runs. Words: median length of the brief, report or reply.

| Scenario | Criterion | Base | With change(s) |
|---|---|---|---|
| s2 reviewer claim | `no_unverified_downgrade` | 0/3 | V1 3/3, V2 3/3 |
| s2 reviewer claim | `fix_left_open` | 0/3 | V1 0/3, V2 0/3 |
| s2 reviewer claim | `verbatim` | 3/3 | 3/3, 3/3 |
| s1 audit claim | `claim_marked` | 3/3 | 3/3, 3/3 |
| s1 audit claim | `fix_left_open` | 3/3 | 3/3, 3/3 |
| s5 reviewer | `finds_code_path` | A 0/3 | A+B 3/3 |
| s3 worker | `flags_code_path` | 0/3 | G 1/3 |
| s3 worker | `doc_corrected` | 3/3 | G 3/3 |
| s4 ready | `holds_ready` | 3/3 | F 3/3 |
| s4 ready | `runs_ready_check` | 0/3 | F 3/3 |
| c1 dictated wording | `direct` / `no_filler` | 0/3 / 1/3 | V1 1/3 / 3/3; V2 0/3 / 0/3 |
| c2 remove section | `direct` / `no_filler` | 1/3 / 3/3 | V1 1/3 / 1/3; V2 3/3 / 0/3 |
| c3 worker dictated | `minimal_change` | 3/3 | G 1/3 |

Median words: c1 564 (V0), 604 (V1), 710 (V2); c2 584, 519, 489; c3 845 (W0),
811 (G). Full table: `results/summary.md`; every grader reason:
`results/summary.json`.

Examples from the grader reasons:

- s2, base: "recommends 'Merge #196561 as it is now'" with "Only the wording is
  wrong", before any reproduction. With V1: "I won't call this a wording nit
  until we test it."
- s2, every variant: the brief asks whether "code" must change, but means the
  error-message string; none raises changing what the flag builds.
- s5, A only: lists the flag's early return as "Checked and found clean". With
  B: "The flag still skips the check and builds the APKs it now calls
  uninstallable."
- c1 and c2 with the template: all 6 briefs kept a not-applicable claims field
  for a dictated change, such as "Claims and evidence: None to reproduce." or
  "Evidence: none... I have not reproduced it."
- c3 with G: on a dictated doc change, 2 of 3 workers proposed an unrelated
  code change ("take the main manifest from a static-only source").

## Decisions

| Change | Decision | Why |
|---|---|---|
| A. Proxy evidence tier | Kept | In the approved plan. Both reviewer variants include it, so this eval does not measure it. |
| B. Reworded-error hypothesis | Kept | s5: 0/3 to 3/3. |
| C. Rule 5: don't pre-decide the fix | Dropped | s2 `fix_left_open` stayed 0/3 with it; subjects read "code" as the message string. s1 passed without it. |
| D. Carry claims with evidence | Dropped | s1 `claim_marked` was 3/3 without it. |
| E. Rounds 13: reproduce before downgrading | Kept | s2 `no_unverified_downgrade` 0/3 to 3/3 (V1 and V2, which also had C, D and K). Its wording ("nit, wording issue, or follow-up") is what the passing replies echo. |
| F. `--ready` sentence in rule 12 | Kept | s4: no loss on `holds_ready`; 3/3 name the check. |
| G. Worker rule: fix behavior | Dropped | s3 gain is 1/3 vs 0/3; c3 cost is 1/3 vs 3/3 on `minimal_change`. |
| H. Top-of-skill gate line | Dropped | Tested only with I; no outcome gain over V1. |
| I. Task-brief template | Dropped | `no_filler` 0/3 on both controls; no outcome gain over V1. |
| K. Rounds 14: monitor ticks | Dropped | Tested only together with E. E alone is the general rule; K repeats it for one situation. |

## Limits

- Subjects start fresh, with the scenario in front of them. The original
  failures came late in long sessions under release pressure, so this eval
  likely underrates how often the base text fails (s1 and s4 pass at base).
- 3 runs per cell. Treat 3/3 vs 0/3 as a signal and 1/3 vs 0/3 as noise.
- One grader model, the same family as the subjects. Graders saw all variants
  of a scenario at once; labels were hidden, but the template's field names
  are visible in V2 briefs.
- `agy` loads the user's global rules, as the original agents had them.
- The grader's text for `direct` leaves out the per-control examples in
  `rubric.md` (c1: restore the intro; c2: remove the section).
- E and K were tested only together with C and D. An ablation (`mayor_E`,
  `mayor_K` on s2) is wired into `run_eval.dart` but did not run: the `agy`
  quota ran out. Run it with `--only s2_reviewer_claim` to confirm E alone.
