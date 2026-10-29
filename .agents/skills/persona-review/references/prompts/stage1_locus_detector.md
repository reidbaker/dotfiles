# Stage 1: Persona-Aligned Multi-Lens Locus Detector (v2)

You find the places in a pull request that the reviewer in `{{PERSONA_PROMPT}}`
would most likely comment on. You do NOT write review comments and you do NOT
invent bug hypotheses. You output neutral candidate loci for Stage 2.

## Inputs

- `{{PERSONA_PROMPT}}`: the reviewer's system prompt.
- `{{PASS_INPUT}}`: the PR header, the PR description as it was when the
  reviewer reviewed, and, for later passes, the prior review threads with the
  author's replies.
- `{{DIFF}}`: the diff to scan. On a later pass this is the diff since the
  last review; the full PR diff is in `{{FULL_DIFF}}` for reference.
- `{{HUNKS}}`: `hunks.jsonl` from `dart run bin/review_pipeline.dart loci-prep`.
  Each line has `id`, `path`, `domain`, `new_start`, `new_end`, `hunk`, and
  `context` (±50 numbered lines of the file at the reviewed commit).

## Persona-Aligned Lens Categories

Scan **every file and hunk** across the diff. Weight persona-structural,
process, test, and clarity lenses **ahead of** generic runtime bug-hunting:

1. `architecture-placement` — file/class/method placement, helper vs. inline
   logic, logic or constants duplicated across files instead of shared, static
   vs. variable outputs, `.yaml` vs. `.yml` repo conventions.
2. `script-vs-dart-tooling` — multi-line bash/shell in GitHub Actions workflows
   instead of unit-tested Dart, or fragile regex parsing where structured APIs
   or command outputs exist.
3. `scope-hygiene` — changes already landed in another PR that should drop out
   of the diff, unagreed `TODO` deferrals, unneeded files/gates, or creating a
   new test file when an existing test file already covers that unit.
4. `test-assertion-fidelity` — changed logic without unit tests, change-detector
   tests, unclear test/group names, `expect(..., reason:)` messages that invert
   or obscure the check, flaky network calls in tests, or unexplained empty/dry-run
   arguments.
5. `api-naming-ergonomics` — ambiguous pronouns (`it`, `this`) or wordy prose in
   descriptions/docs/comments, unclear variable/parameter/rule names, or error
   messages that lack actionable remediation links/commands.
6. `ci-workflow-safety` — workflows that duplicate steps from existing CI
   instead of calling reusable workflows/actions, overly verbose path filters,
   or string-comparison status checks.
7. `config-sdk-bounds` — SDK/AGP/Gradle/Kotlin version bounds, rule defaults in
   registry vs. rule class, `CODEOWNERS` comment grouping, `.pubignore` and
   published-package file checks.
8. `bug-lifecycle` — concrete functional defects, dual parsers that drift,
   silent failures, or broken resource/lock lifecycles.

On a later pass, ALWAYS add a locus for any prior review thread where the
author's reply explains non-obvious behavior that is not in a code comment, or
where the new commit works around the earlier architectural point.

## Output (De-Biased Neutral Hand-off)

Return 8–15 loci as a JSON list. **Do NOT include a prescriptive `why` or bug
theory**—pass only the `lens` category and a neutral 3–6 word topic noun phrase
(`what_is_here`) naming the code/doc construct at that locus:

```json
[
  {
    "path": "...",
    "line": 42,
    "line_range": [35, 55],
    "lens": "architecture-placement|script-vs-dart-tooling|scope-hygiene|test-assertion-fidelity|api-naming-ergonomics|ci-workflow-safety|config-sdk-bounds|bug-lifecycle",
    "what_is_here": "<neutral 3-6 word noun phrase naming the construct>",
    "prior_thread": "<optional: quoted prior thread if following up>"
  }
]
```

`line` must be a line in the new file that appears in the diff. Spread loci
across all changed files (at most 3 loci per file unless the file dominates the
diff) so small config, doc, and test files are never skipped.
