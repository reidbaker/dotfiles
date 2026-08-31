---
name: write-prose
description: Master prose writing and orchestration skill for creating clear, plain, accessible, and natural human writing. Synthesizes ISO 24495-1:2023, W3C Cognitive Accessibility Guidance, Plain Writing Act, and Simplified Technical English (STE) standards. Use when writing, drafting, editing, or reviewing reports, PR descriptions, commit messages, API docstrings, READMEs, architecture documents, user guides, prompts, or general technical prose.
---

# Master Prose Writing & Orchestration (`write-prose`)

> [!CAUTION]
> **MANDATORY LINGUISTIC AUDIT**
> Language models naturally default to pre-trained "AI-isms" (*delve, leverage, seamless, robust, pivotal, testament, not only... but also*). You MUST actively execute the Two-Pass Drafting Protocol below to audit and replace machine-generated fluff before producing final output.

> [!IMPORTANT]
> **WORKSPACE HYGIENE & FILE LOCATION RULES**
> - **No Repo Pollution**: Never create temporary draft files or scratch markdown in the project repository root or source directories.
> - **Temporary Drafts**: Store intermediate drafts, multi-pass review files, or scratch notes in the conversation scratch directory: `<appDataDir>/brain/<conversation-id>/scratch/`.
> - **Final Artifacts**: Write persistent, user-facing markdown reports or documents to the conversation artifacts directory: `<appDataDir>/brain/<conversation-id>/`.

This skill serves as the central source of truth for clear, plain, accessible, and natural human writing across all technical and non-technical documents.

## Procedural Workflow

### Step 1: Context & Audience Inference
Determine the target audience and document format:
1. **Pull Requests / Commits**: Engineering peers -> Focus on "Why" over "How", factual tone, no fluff.
2. **API Documentation / Docstrings**: API consumers -> Third-person singular verbs, concise summaries, clear parameter prose.
3. **User Guides / Tutorials**: End users -> Direct second-person ("you"), short critical paths, explicit step-by-step instructions.
4. **Architecture / Design RFCs**: Team leads & stakeholders -> Clear tradeoffs, decision-first layout, plain language.
5. **Prompts & System Instructions**: AI Agents / LLMs -> Imperative tone, XML tags, positive directives, assume high baseline knowledge (name concepts without explaining them).

Evaluate **Reader Knowledge & Conceptual Boundaries**:
- Strip external meta-task framing (e.g. plan phase numbers, task milestone labels, execution option names, or prompt scoping structures) unless the concept is explicitly defined *within* the document itself.
- Ensure the document stands alone without assuming the reader has access to prompt history, conversation transcripts, or external planning documents.

For detailed audience templates and tone matrices, see [references/audiences.md](references/audiences.md).

---

### Mode A: Writing & Drafting New Prose

#### Step 2: Sub-Skill Coordination
For specialized document types, delegate content gathering to domain skills while enforcing `write-prose` quality standards:
- **Code & API Documentation**: Refer to [code-documentation](../code-documentation/SKILL.md) for docstring and tag conventions.

#### Step 3: Apply Plain Writing & Accessibility Standards
Before writing, consult [references/standards.md](references/standards.md) to apply core principles from:
- **ISO 24495-1:2023**: Ensure content is relevant, findable, understandable, and usable.
- **W3C Cognitive Accessibility (COGA)**: Use clear words, literal language, short text, separate steps, short critical paths, and no reliance on memory.
- **Plain Writing Act**: Ensure immediate first-reading clarity using active voice and short sentences (15–20 words max).
- **Simplified Technical English (STE / ASD-STE100)**: Use controlled vocabulary, explicit sequential steps, max 3 nouns per cluster, and warnings before actions.

#### Step 4: Two-Pass Drafting & Anti-AI-ism Self-Correction
Execute this mandatory two-pass procedure before finalizing output:

1. **Pass 1 (Content Draft)**: Draft the response focusing on technical accuracy, structure, and domain content.
2. **Pass 2 (Linguistic Inspection & Rewrite)**:
   - Scan the draft line-by-line against the **Positive Replacement Pairs** in [references/standards.md](references/standards.md#61-positive-replacement-pairs-banned-words-mappings).
   - Flag and replace any banned verbs (*delve, leverage, foster, cultivate, maximize, democratize, resonate, encompass, bridge, underscore*).
   - Replace vague adjectives (*robust, seamless, pivotal, crucial, holistic, intuitive*) with **specific physical/technical behaviors** (e.g. replace *"robust error handling"* with *"retries failed HTTP requests up to 3 times"*).
   - Eliminate copula substitutions (*"serves as"* -> *"is"*), negative parallelism (*"not only... but also"*), and rule-of-three lists.
   - **Conceptual & Frame Isolation Check**: Run the **Fresh Reader Test** ([references/standards.md](references/standards.md#8-standalone-readability--meta-context-isolation)). Remove un-defined meta-task labels (plan tiers, phase numbers), ephemeral subagent IDs, conversation turn references, and absolute local system paths.
3. **Output**: Present only the polished, post-audit prose.
