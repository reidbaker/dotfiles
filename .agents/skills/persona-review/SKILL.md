---
name: persona-review
description: >-
  Orchestrates engineering persona reviews (`persona-review`, `/persona-review`,
  or "panel review") across early design/RFC stages and implementation PRs
  using the 3-stage pipeline (Locus Detector -> Focused Persona Critic ->
  Stratified Ranker) plus a pair-routed whole-PR architecture/duplication pass.
---

# Persona Review Skill (Moved to `ai-team`)

> **IMPORTANT FOR ACTIVE AGENTS:** To maintain a single source of truth, `persona-review` and all 11 persona prompts now live in `/Users/reidbaker/Documents/ai-sandbox/scratch/ai-team/skills/persona-review/`.
> - Read `/Users/reidbaker/Documents/ai-sandbox/scratch/ai-team/skills/persona-review/SKILL.md` and load all persona prompts from `/Users/reidbaker/Documents/ai-sandbox/scratch/ai-team/skills/persona-review/references/personas/<name>.md`.
> - Run the 3-stage pipeline via `dart run /Users/reidbaker/Documents/ai-sandbox/scratch/ai-team/bin/persona_tools.dart review-pipeline <loci-prep|rank> ...` (the wrapper `dart run bin/review_pipeline.dart` in `devconfig` also remains available while active agents finish).

See:
- [`references/personas/reid.md`](references/personas/reid.md)
- [`references/personas/stuart.md`](references/personas/stuart.md)
- [`references/personas/lasse.md`](references/personas/lasse.md)
- [`references/personas/jake.md`](references/personas/jake.md)
- [`references/personas/david.md`](references/personas/david.md)
- [`references/personas/codefu.md`](references/personas/codefu.md)
- [`references/personas/loic.md`](references/personas/loic.md)
- [`references/personas/chris.md`](references/personas/chris.md)
- [`references/personas/paul.md`](references/personas/paul.md)
- [`references/personas/jenn.md`](references/personas/jenn.md)
- [`references/personas/flutter-tools-ghost.md`](references/personas/flutter-tools-ghost.md)
- [`references/prompts/stage1_locus_detector.md`](references/prompts/stage1_locus_detector.md)
- [`references/prompts/stage2_focused_critic.md`](references/prompts/stage2_focused_critic.md)
- [`references/author-briefs/reid.md`](references/author-briefs/reid.md)
