---
name: adversarial-review
description: Adversarial code review that produces findings which survive verification. Use when asked for an adversarial review, CLAR (cross lab adversarial review) to poke holes in a change, to stress-test a diff, or to be told what is actually wrong with a PR. Enforces an evidence standard - documentation is read from disk or fetched rather than recalled, performance claims are measured, and every finding survives a falsification pass before it is reported.
---

# Adversarial review

## Why this exists

Ordinary review skills optimize for thoroughness: find everything. Adversarial
review has a different failure mode. When a reviewer is told to attack a change,
it will find something, whether or not something is there. The result is a report
full of confident, specific, wrong findings.

A false finding costs more than a missed one. The author spends an hour
disproving it, or worse, "fixes" working code. Fabricated citations are the most
expensive of all, because a quoted API doc reads as settled fact and almost
nobody opens the source to check.

So optimize for the survival rate of findings, not the count. Three findings that
all hold up is a better review than nine findings where one holds up.

## The evidence standard

Every claim carries an evidence tier. State the tier in the finding.

- **Executed** - you ran a command and are showing its real output. Always acceptable.
- **Read** - literal text you opened from disk this session, cited by path and
  line. Always acceptable.
- **Fetched** - content you retrieved over the network this session, quoted
  verbatim. Always acceptable.
- **Deduction** - a step-by-step logic, control-flow, or state trace through
  code read this session. Acceptable for Blocker or Major when each step cites
  exact file and line numbers.
- **Speculation** - reasoning over incomplete information, unmeasured
  performance claims, or hunches. Acceptable for Question or Minor only.
- **Recalled** - you remember it being this way. Never acceptable.

Two consequences are worth stating separately, because they are the two rules
most often broken:

**Never quote documentation from memory.** If a finding depends on what an API
guarantees, open the documentation. For a dependency, inspect its source
definitions, declarations, or official reference docs. For a web-hosted doc,
fetch the page. Paste the text you actually saw. If you cannot get to the text,
you do not have the finding - you have a Question.

**Measure performance claims.** "This will be slow", "this will time out",
"this adds minutes to CI" are all Executed-tier claims. Run it and time it. If
you cannot run it, file a Question asking the author for the number.

## Severity is capped by evidence

- **Blocker** - ship this and something breaks. Requires Executed, Read, or a
  verifiable Deduction trace.
- **Major** - real defect, real cost, not fatal. Requires Executed, Read, or a
  verifiable Deduction trace.
- **Minor** - worth fixing, low cost either way. Speculation or incomplete
  deduction is enough.
- **Nit** - style or taste.
- **Question** - you suspect something and cannot prove it. No evidence needed.

Question exists so that you never have to inflate a hunch into a Major to make
it visible. A well-phrased Question is a legitimate review outcome. A Blocker
resting on unverified speculation is not.

## Workflow

### 1. Establish scope precisely

Know exactly which commits you are reviewing before you read any code.

```bash
git log --oneline <base>..HEAD
git diff --stat <base>..HEAD
```

Prefer an explicit merge base over a branch name, so the scope does not drift
while you work. Reviewing an extra commit produces findings the author already
fixed.

### 2. Write hypotheses before investigating

Write down what you are going to try to prove, before you go looking. This is
what keeps the review from degenerating into a search for anything sayable.

Each hypothesis should be a specific claim that some command or file could
falsify:

- The removed explicit dependency is not replaced by an implicit one, so the
  tasks can run out of order.
- The new test passes against a deliberately broken implementation.
- The retry path can be entered twice concurrently and double-increments.
- The deleted test's coverage does not exist anywhere else in the tree.

Not a hypothesis: "the error handling could be better." Nothing falsifies that,
so investigating it produces opinion, not findings.

### 3. Investigate against the tree, not the diff

The diff is a pointer to where to look. It is not the state of the code.

- A deletion hunk does not mean the code is gone. It may have moved. Grep HEAD
  before claiming removal.
- Before recommending an assertion on some construct, confirm the production
  code actually uses that construct. Recommending a stricter assertion on an
  argument the caller never passes is a wasted round trip.
- Before claiming a behavior is undocumented or incidental, read the docs. It is
  common for "this test pins accidental behavior" to be exactly backwards - the
  behavior is contractual and the test is right.

### 4. Falsification pass

Before writing anything up, take each surviving hypothesis and actively try to
disprove it. A finding is only ready to report when you have exhausted the
cheapest ways to falsify it.

Questions to resolve:

1. What is the cheapest check or command that would prove me wrong? Run it.
2. Am I quoting anything I did not read this session? Remove it or read it.
3. Is the defect demonstrable via a deterministic code path trace, or is it a
   hunch?
4. Would existing tests or compiler/type checks catch this if mutated?
5. Does the severity match the evidence tier I can actually supply?
6. If the author asks for proof, do I have a concrete reproduction or
   line-by-line trace to show?

Generic falsification techniques:

- **Mutation testing** - if claiming a test is tautological or misses an edge
  case, mentally or locally invert the condition, delete the guard, or alter
  the return value. If the test still passes, the finding holds; if it fails,
  the behavior was already guarded.
- **Call-site audit** - before asserting an argument is omitted or an API is
  misused, search all invocations across the codebase to verify whether callers
  actually use the construct in practice.
- **Dependency contract inspection** - check the actual source code, header
  declarations, or formal type signatures of dependencies rather than assuming
  behavior from memory or search summaries.
- **Execution graph query** - for questions about ordering, dependencies, or
  lifecycle hooks, query the toolchain's dry-run, execution plan, or task
  graph mode rather than speculating from static files.

Findings that do not survive this pass get dropped, or downgraded to Question.

### 5. Report worst first

Use a consistent block per finding:

```
[Severity] Short claim
Location:   path:line
Evidence:   <tier>
            <the command and its output, or the quoted text with its source>
Why:        the concrete consequence
Fix:        the smallest change that resolves it
```

Two sections are mandatory:

**Verdict** - your overall call, in one or two sentences.

**Checked and found clean** - the hypotheses you investigated and disproved.
This is not filler. It tells the author which risks are covered so nobody
re-reviews them, and it shows that your findings are the survivors of a process
rather than everything you thought of.

### 6. Leave the workspace as you found it

Keep scratch files outside the repository under review. Untracked build,
lockfile, or environment configuration files can silently alter toolchain
behavior and test execution. Before finishing, check for untracked files you
created and remove them.

## Orchestration: working as several reviewers

For a large change, split the work across parallel reviewers. Split by attack
surface, not by file count - overlapping surfaces produce duplicate findings and
gaps produce misses:

- Production behavior vs. test quality
- Runtime correctness vs. build, packaging, and dependency wiring
- Code that changed vs. code that was deleted or moved

Tell each reviewer which surface belongs to someone else, so they do not report
into it.

Every reviewer inherits the evidence standard, the severity caps, and the
falsification pass. Use [references/reviewer-brief.md](references/reviewer-brief.md)
as the delegation template.

Reviewers must not modify the code under review. A reviewer that edits the tree
invalidates the other reviewers' evidence.

### The Orchestrator Falsification Gate

The orchestrator must never act as a passive relay for subagent reports. When
subagents return findings:

- **Audit citations first** - open at least one quoted document, symbol, or
  cited line number from each reviewer. If a single citation is fabricated,
  treat the reviewer's entire report as unverified.
- **Challenge unverified claims** - run the cheapest disproof check or inspect
  the tree before bubbling up any Blocker or Major.
- **Require explicit retractions** - send refuting evidence back to the
  reviewer and require an explicit retraction before compiling the final
  report.
- **Synthesize verified survivors** - present only the findings that survive the
  gate, along with the disproved hypotheses in "Checked and found clean".

## Receiving findings

When findings come back from a reviewer, whether human or agent:

**Spot-check the citations first.** Open one quoted document and one cited line
number before evaluating any argument. If a single citation is fabricated, treat
the entire report as unverified and re-check every claim in it - a reviewer that
invented one quote will have invented others.

**Push back with evidence, not with disagreement.** Confronting a reviewer with
a command and its output gets a fast, honest retraction. Saying "I don't think
that's right" gets a restatement. State the specific observation, cite the
actual tool output or source text that contradicts the finding, and ask the
reviewer directly to reconcile the contradiction or formally retract the
finding.

**Ask for explicit retractions.** "You're right, withdrawing finding 3" is a
clean signal. A reviewer that responds by pivoting to a new argument is still
holding the original claim, and you will trip over it again later.

## Anti-patterns

- **Fabricated citation** - quoting a javadoc or spec from memory, with
  plausible wording that does not exist.
- **Diff-only reasoning** - concluding code was removed from a deletion hunk,
  without checking HEAD.
- **Phantom target** - recommending an assertion on a parameter or overload the
  caller never uses.
- **Unmeasured performance** - "this will time out in CI", with no measurement.
- **Category error** - "already covered", citing tests at a different layer than
  the one under review.
- **Severity inflation** - a Blocker resting on inference, because Question felt
  too weak to bother filing.
- **Silent skip** - dropping an assigned hypothesis without saying it was
  dropped.
- **Assertion-flipping** - "this test pins incidental behavior, loosen it", when
  the behavior is in fact documented.

The last one cuts both ways and is worth dwelling on. Sometimes an observed
behavior really is contractual. When it is, the right outcome is not to loosen
the assertion - it is to keep it and quote the contract in a comment above it,
so the next reviewer does not raise the same objection.
