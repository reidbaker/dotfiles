---
name: pr-triage
description: Identifies and prioritizes GitHub Pull Requests that need attention across authored work and review queues. Use when the user asks "what PR needs my attention next", "check my PRs", "triage my pull requests", or wants to review active pull request queues.
---

# PR Triage Skill

## Purpose
Finds open GitHub pull requests across the configured accounts plus incoming
review requests, sorts them by how urgently they need an action, highlights the
**Top 3 for My Work** and **Top 3 for Review Queue** in chat, and writes the
full categorized report to an artifact.

Tier names and action strings below are generated from the enums in
`lib/src/pr_triage/models/triaged_item.dart`. That file is the source of truth;
if the two disagree, the code wins.

---

## Setup

Config lives at `.agents/skills/pr-triage/resources/config.yaml`, which is
gitignored. On a fresh clone it does not exist, so create it first:

```bash
cp .agents/skills/pr-triage/resources/config.example.yaml \
   .agents/skills/pr-triage/resources/config.yaml
```

Then set `accounts:` to the GitHub logins to triage. Without a config the tool
falls back to the single authenticated `gh` user and prints a warning to
stderr.

Resolving `team_orgs` into teammate logins needs a token with the `read:org`
scope. Without it the run still succeeds, falls back to the static
`team_members` list, and warns.

---

## Priority Ordering Rules

Ranks are queue-local. A My Work rank 4 is not more or less urgent than a
Review Queue rank 4; the two queues are ordered independently and never
interleaved.

### 1. My Work (Authored Pull Requests)
1. **Ready to Merge**: Approved, CI positively green, zero unresolved threads,
   not merge-blocked, not draft.
   * *Action*: `[Action: Merge]`
2. **Waiting on CI/CD Task / Action Required**: An author action is needed
   before CI can run or finish. Three distinct causes: the repository gates
   presubmits behind a trigger label (see `presubmit_triggers`) and no checks
   have run; a `cicd_blocked_labels` label is applied; or a check run needs
   manual approval. Never fires on drafts.
   * *Action*: `[Action: Unblock CI/CD task / Trigger CI]`
3. **Draft Ready for Review (CI Green)**: Draft with green CI, no conflicts,
   no blocking reviews, and no unresolved threads. It looks finished and was
   probably never marked ready.
   * *Action*: `[Action: Mark ready for review & notify reviewers]`
4. **Failing CI (Flaky candidate)**: Every failing check matches
   `flaky_test_keywords`.
   * *Action*: `[Action: Investigate/re-run flaky CI]`
5. **Approved with Minor Comments**: Approved but with unresolved threads.
   * *Action*: `[Action: Address minor comments & land]`
6. **Failing CI (Needs Code Fix)**: CI failures that are not flaky.
   * *Action*: `[Action: Fix failing tests/lints]`
7. **Substantial Review Feedback / Changes Requested**: An active blocking
   review, or several unresolved threads.
   * *Action*: `[Action: Address review feedback]`
8. **Stalled in Review (>= threshold business days)**: Awaiting review for at
   least `stale_review_business_days`, counted from the most recent review
   request.
   * *Action*: `[Action: Ping reviewer(s)]`
9. **In Review (within normal window)**: Awaiting review, still inside the
   threshold.
   * *Action*: `[Action: Awaiting review]`
10. **Active Draft / WIP (Primary Repo)**: A draft that is genuinely
    unfinished. The reason string names what is outstanding.
    * *Action*: `[Action: Resume development / Work in progress]`
11. **Repository Not In primary_orgs**: A real repository, just not one being
    tracked. Distinct from a personal fork.
    * *Action*: `[Action: Low urgency - add repo to primary_orgs to promote]`
12. **Personal Fork / POC**: The repository is owned by one of the configured
    accounts, or the PR was demoted via `pr_overrides`.
    * *Action*: `[Action: POC / Fork - Low urgency]`

### 2. Review Queue (Other People's Work)
Terms used below:
* **Asked by name**: one of the configured accounts is in the PR's requested
  reviewers. GitHub's `review-requested:` search also matches requests sent to
  a team you belong to; those are not asked by name.
* **Team-only**: only a team was asked. You were not asked by name, are not an
  assignee, and have never reviewed the PR.
* **Crowded**: at least `crowded_review_threshold` distinct people other than
  you (requested reviewers plus human reviewers; default 4).
* **Merge conflicts** rule a PR out of tiers 3, 4 and 6; it falls to Other with
  a "needs rebase" reason instead.
* **@-mentioned**: a human other than you wrote a PR conversation comment
  (one of the last 15) that mentions one of the configured accounts. Emails
  and team mentions such as `@flutter/android-reviewers` do not count; bot
  comments do not count.

1. **Re-Review Ready (Feedback Addressed)**: You reviewed before, the author
   has pushed since, and a re-review is requested.
   * *Action*: `[Action: Re-review and sign off]`
2. **Asked for Your Review (@-mention)**: You were @-mentioned within
   `mention_window_business_days` (default 10) and have not reviewed since
   that comment. Checked before every rule except repository scoping,
   overrides and Re-Review Ready, so draft state, merge conflicts, failing CI
   and team-only requests do not bury it. The reason quotes the comment and
   lists approvals from others, draft state, conflicts and CI failures. Days
   are counted from the comment.
   * *Action*: `[Action: Review - you were asked directly]`
3. **Teammate PR Review**: Author is a teammate, resolved from `team_orgs` or
   listed in `team_members`. CI not failing, no merge conflicts.
   * *Action*: `[Action: Review team PR]`
4. **Clean External Contributor PR**: Non-teammate, CLA signed, not draft, no
   blockers, CI not failing, no merge conflicts.
   * *Action*: `[Action: Review external PR]`
5. **Waiting on Author Response**: A `waiting_labels` label is applied.
   * *Action*: `[Action: Awaiting author updates]`
6. **Draft PR in Review Queue**: Review requested on something not yet marked
   ready, without merge conflicts.
   * *Action*: `[Action: Deprioritized - Awaiting author to mark ready for review]`
7. **Co-Reviewer Stalled**: You were asked by name, and another reviewer asked
   by name has not reviewed for at least `stale_co_reviewer_business_days`
   (default 10), counted from the most recent review request. Not draft, no
   merge conflicts. Failing CI does not rule it out.
   * *Action*: `[Action: Ping co-reviewer(s) or reassign]`
8. **Blocked External PR (CLA/Blockers)**: Unsigned CLA or blocking reviews
   from the team.
   * *Action*: `[Action: Low priority (blocked)]`
9. **Review in Repository Not In primary_orgs**
   * *Action*: `[Action: Low urgency - add repo to primary_orgs to promote]`
10. **Personal Fork Review**
    * *Action*: `[Action: Fork Review - Low urgency]`
11. **Other / Backlog**: Includes PRs with merge conflicts or failing CI.
    * *Action*: `[Action: Monitor]`
12. **Team-Only Request (not addressed to you)**: Checked before every rule
    except repository scoping, overrides and the @-mention rule, so a
    team-only draft cannot rank as tier 6. Uncrowded requests sort before
    crowded ones. The reason names the teams, the head count, and any
    conflicts or draft state.
    * *Action*: `[Action: Low priority - team request, not addressed to you]`

---

## Execution Procedure

### Step 1: Run the CLI
From the repository root:
```bash
dart run bin/pr_triage.dart
```
Useful flags: `--config` for an alternate config path, `--limit` to change how
many PRs are fetched per query (GitHub caps this at 100), `--top` to change how
many highlights each queue reports.

### Step 2: Parse the results
JSON goes to stdout; warnings go to stderr. Read stderr too, it is where an
incomplete run announces itself.

* `top_my_work` and `top_review_queue`: compact references for the highlights,
  each already carrying `tier_name`, `action_prompt`, `reason`, and
  `business_days_elapsed`. Sorted by tier rank, then by days elapsed
  descending.
* `my_work` and `review_queue`: the full lists with complete PR detail.
* `truncated`: true when GitHub had more matches than `query_limit` returned.
  Say so in the report rather than presenting a partial queue as complete.
* `warnings`: non-fatal problems worth repeating to the user.

### Step 3: Present both Top 3 lists in chat
* **Do not use tables.**
* **Section 1: Top 3 for My Work (Authored PRs)**
  * Numbered Markdown links: `[#123: Title](https://github.com/org/repo/pull/123)`.
  * Include the action prompt verbatim from `action_prompt`.
  * Add the tier and a one-sentence rationale from `reason`.
* **Section 2: Top 3 for Review Queue (Other People's Work)**
  * Same format. If empty, say *No pending review requests found.*

### Step 4: Write the full report artifact
Save the breakdown to `pr_triage_report.md` in the conversation artifact
directory, with:
* **My Work Queue**: full list grouped by tier.
* **Review Queue**: full list grouped by tier.
* **Configuration**: accounts, primary orgs, resolved teammate count,
  thresholds, and any warnings or truncation.

### Step 5: Offer next actions
* Ready to merge: offer to merge.
* Blocked on a CI/CD task: offer to apply the trigger label or approve the run.
* Flaky failure: offer to inspect logs and re-run.
* Stalled: offer to draft a ping comment.
