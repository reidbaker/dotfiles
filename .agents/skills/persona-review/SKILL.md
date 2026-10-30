---
name: persona-review
description: >-
  Orchestrates engineering persona reviews across early design/RFC stages and
  implementation PRs using the 3-stage pipeline (Locus Detector -> Focused
  Persona Critic -> Stratified Ranker) plus a whole-PR architecture/duplication
  pass. Use when reviewing design docs, plans, or pull requests with engineering
  personas (Reid, Stuart, Lasse, Jake, David, Codefu, Loic, Chris, Paul, Jenn,
  Flutter Tools Ghost).
---

# Persona Review Skill

Pair orthogonal engineering personas by development stage, and run implementation reviews on focused loci through the 3-stage pipeline rather than dumping a whole PR diff into one monolithic prompt.

---

## 1. Stage-Based Persona Routing

Never ask a single generic reviewer to check everything. Select orthogonal personas whose blind spots do not overlap based on the development stage and file domain:

### Stage A: Early Design Docs, Specs, RFCs & PR Architecture (Before Code)
Pair **one Scope & Simplicity Challenger** with **one Formal Invariant & Ergonomics Architect**:

1. **Scope & Simplicity Challengers (pick 1):**
   - **Reid** ([`references/personas/reid.md`](references/personas/reid.md)) — Challenges why a feature, file, or dependency exists at all; enforces a single canonical parser/source of truth; guards SDK/Gradle/AGP compatibility and future-maintainer breadcrumbs.
   - **David** ([`references/personas/david.md`](references/personas/david.md)) — Asks whether a simpler data model or zero-codegen approach already solves the problem; enforces a small, precise set of nouns; unblocks pragmatic incremental shipping with `TODO`s.
   - **Stuart** ([`references/personas/stuart.md`](references/personas/stuart.md)) — Pressure-tests package/plugin boundaries, Chesterton's fence on removed behavior, cross-platform federated plugin contracts, and migration plans.
   - **Jenn** ([`references/personas/jenn.md`](references/personas/jenn.md)) — Splits oversized PRs along natural boundaries; defines project-specific acronyms/nouns inline at first mention; writes a settled-thread summary conclusion when a doc thread reaches consensus.
2. **Formal Invariant & Ergonomics Architects (pick 1):**
   - **Jake** ([`references/personas/jake.md`](references/personas/jake.md)) — Audits developer ergonomics, `pubspec.yaml`/`build.yaml` configuration boundaries, build/serialization performance, and macro/codegen contracts.
   - **Paul** ([`references/personas/paul.md`](references/personas/paul.md)) — Stress-tests state transitions, exhaustiveness, type-inference/promotion edge cases, and analyzer/CFE invariants.

### Stage B: Implementation PR Review (By File Domain)
Once the implementation is drafted, route changed files to domain-specific critics plus a single whole-PR architecture/duplication pass:

| File Domain | Primary Persona Critics | What They Catch |
| :--- | :--- | :--- |
| **`packages/flutter_tools/**` & Flutter CLI** | **Flutter Tools Ghost** ([`references/personas/flutter-tools-ghost.md`](references/personas/flutter-tools-ghost.md)) + **Jenn** ([`references/personas/jenn.md`](references/personas/jenn.md)) | Constructor DI over `globals.*`, `testWithoutContext` + `BufferLogger`/`FakeProcessManager`/`MemoryFileSystem`, `throwToolExit` vs. unhandled exceptions, `Target` inputs/outputs caching invariants, and iOS/macOS parity (`build_ios.dart` + `build_macos.dart`). |
| **Dart Libraries & Tests (`*.dart`)** | **Lasse** ([`references/personas/lasse.md`](references/personas/lasse.md)) + **`adversarial-review`** (+ **Reid** [`references/personas/reid.md`](references/personas/reid.md) for CLI/tooling/test craft) | Dart language semantics, API contracts, `///` DartDoc precision, single-pass collection/bit-manipulation idioms, and falsification-tested defects. *(Note: Do not run Jake on line-by-line Dart files; keep Jake on Stage A design/ergonomics or `pubspec`/`build.yaml` changes.)* |
| **CI Workflows, Release Scripts & Infra (`.github/workflows/*.yaml`, `.ci.yaml`, `*.sh`)** | **Codefu** ([`references/personas/codefu.md`](references/personas/codefu.md)) + **Reid** ([`references/personas/reid.md`](references/personas/reid.md)) | Moving multi-line bash into unit-tested Dart, reusable workflow deduplication, release-branch/merge-queue safety, concurrency races, and `start()`/`stop()` resource lifecycle pairing across failure branches. |
| **Android & Gradle (`*.gradle*`, `*.kt`, `*.java`)** | **Reid** ([`references/personas/reid.md`](references/personas/reid.md)) + **`adversarial-review`** | `minSdk`/`compileSdk`/AGP/Gradle/Kotlin version bounds, Gradle task ordering (`dependsOn`), Robolectric `@Config`, Java `assert` stripping in release builds, and fallback behavior on unknown platform values. |
| **C/C++ Engine, Windows & Embedder ABI (`*.cc`, `*.cpp`, `*.h`, `CMakeLists.txt`)** | **Loïc** ([`references/personas/loic.md`](references/personas/loic.md)) + **Chris** ([`references/personas/chris.md`](references/personas/chris.md)) | C embedder ABI (`struct_size`), multi-window/multi-view ownership, platform vs. raster thread affinity, `std::move`/UAF hazards, and ready-to-commit ```` ```suggestion ```` blocks. |
| **iOS/macOS Darwin Embedder, Xcode, CocoaPods & SwiftPM (`*.mm`, `*.m`, `*.swift`, `Podfile`, `*.pbxproj`, `BUILD.gn`)** | **Chris** ([`references/personas/chris.md`](references/personas/chris.md)) + **Jenn** ([`references/personas/jenn.md`](references/personas/jenn.md)) + **Stuart** ([`references/personas/stuart.md`](references/personas/stuart.md)) | Objective-C++ ARC / `scoped_nsobject` lifecycles, `UIGestureRecognizer` vs. `UIResponder` touch forwarding, `Podfile` `use_frameworks!`/`use_modular_headers!` matrix, `Info.plist`/`xcscheme` parity, Swift `@MainActor` vs. release-build `assert` stripping, and workaround issue-link breadcrumbs. |
| **Analyzer, CFE & Type System (`pkg/analyzer/**`, `pkg/_fe_analyzer_shared/**`)** | **Paul** ([`references/personas/paul.md`](references/personas/paul.md)) + **Lasse** ([`references/personas/lasse.md`](references/personas/lasse.md)) | Unhandled AST/`Element2` visitor variants, `Type.substitute` null returns, CFE/Analyzer shared parity, and `@Deprecated(message)` + `CHANGELOG.md` completeness. |
| **Markdown, Docs, Skills & Policy (`*.md`, `SKILL.md`, `CHANGELOG.md`)** | **Stuart** ([`references/personas/stuart.md`](references/personas/stuart.md)) + **Reid** ([`references/personas/reid.md`](references/personas/reid.md)) | Doc-vs-code accuracy, ambiguous pronouns (`it`, `this`), over-long prose, missing remediation links, and `CHANGELOG.md` user-impact clarity. |

---

## 2. The 3-Stage Focused Review Pipeline (Implementation PRs)

Handing a multi-file PR diff to a single monolithic prompt collapses recall from `60.9%` on focused hunks down to `8%–17%` on whole PRs, and naive severity ranking lets generic structural questions crowd out signature craft catches (ambiguous pronouns, inlining one-off constants, explaining non-obvious dry-run test literals, single-pass Dart rewrites).

For single-file or small changes, review **one changed file at a time** with surrounding file context. For multi-file PRs, run the decoupled 3-stage pipeline from the devconfig repository root (this skill lives at `<root>/.agents/skills/persona-review`):

### Stage 0: Prepare Commit-Exact Hunks (`loci-prep`)
Extract unified diff hunks with `±50` numbered lines of surrounding file context at the reviewed commit:
```bash
dart run bin/review_pipeline.dart loci-prep \
  --diff <path/to/pr.diff> \
  --repo <path/to/checkout> \
  --out <scratch_dir>/pipeline
```

### Stage 1: De-Biased Locus Detector
Run a subagent with [`references/prompts/stage1_locus_detector.md`](references/prompts/stage1_locus_detector.md), passing the target persona prompt and `hunks.jsonl`.
- Stage 1 flags **8–15 specific `(path, line, line_range, lens, what_is_here)` loci** spread across changed files.
- **De-biasing invariant:** Stage 1 must **never** pass a prescriptive `why` or bug theory to Stage 2—only the neutral `what_is_here` noun phrase and `lens` tag (enforced by `sanitizeLocusForStage2`).

### Stage 2: Focused Persona Critic (Per Locus)
For each locus (or per changed file on small PRs), run a subagent with [`references/prompts/stage2_focused_critic.md`](references/prompts/stage2_focused_critic.md) and the persona's system prompt from `references/personas/<persona>.md`.
- Give the critic the hunk with `±50` lines of context and read-only access to the repository checkout at `HEAD`.
- Each critic emits up to **2 candidates** (`candidate_1` for the primary structural/correctness observation, `candidate_2` for a secondary craft, DartDoc, idiom, or test-clarity catch). Write all outputs to `<scratch_dir>/pipeline/critic_outputs.jsonl`.

### Stage 3: Stratified Ranker (`rank`)
Filter, deduplicate, and rank the Stage 2 candidates using the Stage 3 Stratified Ranker:
```bash
dart run bin/review_pipeline.dart rank \
  --person <reid|stuart|lasse|jake|david|codefu|loic|chris|paul|jenn|flutter-tools-ghost> \
  --in <scratch_dir>/pipeline/critic_outputs.jsonl \
  --out <scratch_dir>/pipeline/ranked_review.json \
  --pass-index <1|2|3>
```
- **What Stage 3 does:** Merges same-file duplicates on the same identifier, collapses cross-file pattern repeats into `"Same here."`, caps output at `K` comments (`K=5` default, `K=8` for Lasse), and **reserves 1–2 dedicated slots on distinct files for high-signal `minor`/`nit` signature craft catches** so major structural questions never crowd out craft and maintainability checks. The verdict depends only on the pass and the severities, never on who authored the PR.

---

## 3. Whole-PR Architecture & Duplication Pass (`arch_stuart`)

In addition to per-locus / per-file critics, run **one whole-PR architecture & duplication pass (`arch_stuart`)** per PR using [`references/personas/stuart.md`](references/personas/stuart.md) with the full PR diff and a searchable checkout of the repository:

> *"This pass is NOT a line-by-line review: other reviewers cover each changed file on its own. Look only for problems that need more than one file to see: (1) new code that duplicates existing code in the repository (search with grep/rg for similar functions, regexes, parsers, helpers, constants and test setup), or that should reuse an existing helper, model, or a well-known Dart package or tool; (2) code, classes or tests placed in the wrong file or layer, or a new file where an existing one should be extended; (3) other places in the repository (docs, examples, skills, README, RULES.md, tests, config) that should change with this PR but did not; (4) interactions with existing features, rules, options or configuration that the PR does not handle; (5) boundary & single-source-of-truth leaks across files: raw `Map<String, dynamic>`, untyped `Object?` option bags, boolean origin flags (`isFromFile`), or 3+ positional `String` parameters passed across internal layers instead of typed domain records/classes, or default parameter values/constants duplicated between a registry/caller and a rule class. For each finding, cite the existing code as evidence (`path:line` in the repo, or the package/tool name)."*

---

## 4. Model Routing & Falsification Gate

- **Model selection:**
  - Run `arch_stuart` on **Argon (`gemini-launch-candidate-low`)** (highest precision and highest value per 10 findings in our `v5` benchmark).
  - Run Stage 1/2 persona critics on **Argon (`gemini-launch-candidate-low`)** or `inherit` (or `opus-5.5-low` when explicitly requested and quota is healthy).
  - **Never use `flash` models (`gemini-*-flash*`) for code or design review** (`36%` harmful rate and `80%` silent approvals in `v5` evals).
- **Weigh findings on their merits:** Verify citations against the actual tree before acting on or reporting any finding. Drop findings that contradict repository conventions (such as configured `page_width` in `analysis_options.yaml` or explicit user instructions). When a finding or the PR says a test enforces a rule, open the test and find the assertion that does it.
- **Removal audit for deletion-heavy PRs:** When a PR removes or consolidates text, list each removed instruction, example or check, and mark it moved, already covered elsewhere (cite the file), or dropped on purpose. Report every unmarked removal as a finding.
- **Review is not done until fixes are reviewed:** After fixes for findings land, run the falsification gate again on the fix diff (`git diff <reviewed-sha>..<head>`). Report the review state exactly (for example, "reviewed at `<sha>`, fixes not re-reviewed"), never just "ready".

---

## 5. One Tone, and Never Speak as the Human

- **Never switch tone on who authored the PR.** People often share one GitHub account with their agents, so "agent PR" vs. "human PR" can't be detected reliably. Every persona uses its polite, human-facing tone on every PR. Soften the wording, never the issue.
- **Score issues, not tone.** When you evaluate a persona, count a comment as a match if it raises the same issue as the real reviewer, even if it is phrased more softly.
- **Never post persona output as the human.** Persona comments go to the mayor or the worker as drafts. Never post them from the human's account, and label any persona review that is shared as an agent persona review (see the worker brief).

## 6. Author Briefs (Before Code Is Written)

Some reviewers have an author brief in `references/author-briefs/`, for example [`reid.md`](references/author-briefs/reid.md). It lists the issues that reviewer most often asks agents to fix after the fact. When a worker writes code that this reviewer will review, paste the brief into the worker's prompt together with the worker brief, so the issues are prevented on the first pass instead of found in review.
