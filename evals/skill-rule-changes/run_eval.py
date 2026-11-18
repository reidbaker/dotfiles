# Eval harness for the skill-rule changes in reidbaker/dotfiles#8.
#
# Runs with Jetski's `run_workflow` tool (it only runs Python), from the repo
# root:
#   run_workflow(ScriptPath=<abs path to this file>, Cwd=<repo root>)
# Optional Args: {"runs": 3, "phase": "all" | "subjects" | "grade" | "report",
#                 "min_quota": 25}
#
# It builds each skill-text variant from origin/master plus the change texts
# in variants/changes/, runs fresh subject agents on every scenario x variant,
# grades the outputs blind, and writes results/. Finished runs are skipped, so
# a stopped run can be resumed.

import hashlib
import json
import os
import random
import re
import statistics
import subprocess

EVAL = os.path.join("evals", "skill-rule-changes")
VARIANTS = os.path.join(EVAL, "variants")
CHANGES = os.path.join(VARIANTS, "changes")
RAW = os.path.join(EVAL, "results", "raw")
GRADES = os.path.join(EVAL, "results", "grades")
MODEL = "opus-5.5-high"
BATCH = 3  # High-effort agents at once.

RUNS = int(args.get("runs", 3))
PHASE = args.get("phase", "all")
MIN_QUOTA = int(args.get("min_quota", 25))

MAYOR = ".agents/skills/mayor-worker-prs/SKILL.md"
WORKER = ".agents/skills/mayor-worker-prs/references/worker-brief.md"


def git_show(path):
    return subprocess.run(
        ["git", "show", "origin/master:" + path],
        capture_output=True, text=True, check=True,
    ).stdout


def change(letter):
    """A change text, without its first line (which says where it goes)."""
    with open(os.path.join(CHANGES, letter + ".md")) as f:
        lines = f.read().strip("\n").split("\n")
    return "\n".join(lines[1:]).strip("\n")


def replace_line(text, prefix, new):
    lines = text.split("\n")
    hits = [i for i, l in enumerate(lines) if l.startswith(prefix)]
    assert len(hits) == 1, (prefix, hits)
    lines[hits[0]] = new
    return "\n".join(lines)


def insert_after(text, prefix, new):
    lines = text.split("\n")
    hits = [i for i, l in enumerate(lines) if l.startswith(prefix)]
    assert len(hits) == 1, (prefix, hits)
    lines.insert(hits[0] + 1, new)
    return "\n".join(lines)


def build_variants():
    mayor = git_show(MAYOR)
    worker = git_show(WORKER)
    # V1 tests the rule text alone, so its rule 5 does not link the template.
    link = " using [references/task-brief.md](references/task-brief.md)"
    assert link in change("C")
    rules = replace_line(
        mayor, "5. **Route review comments whole.**", change("C").replace(link, "")
    )
    rules = insert_after(rules, "14. **Suggest a second reviewer", change("D"))
    rules = insert_after(rules, "12. **Link a PR only when", change("E") + "\n" + change("K"))
    template = replace_line(
        rules, "5. **Route review comments whole.**", change("C")
    )
    template = insert_after(template, "One coordinator (the mayor)", "\n" + change("H"))
    rule12 = "12. **Re-review fix commits.**"
    old12 = next(l for l in mayor.split("\n") if l.startswith(rule12))
    ready = replace_line(mayor, rule12, old12 + " " + change("F"))
    worker_g = worker.rstrip("\n") + "\n" + change("G") + "\n"
    out = {
        "mayor_V0": mayor,
        "mayor_V1_rules": rules,
        "mayor_V2_rules_template": template,
        "mayor_F_ready": ready,
        "worker_W0": worker,
        "worker_G": worker_g,
    }
    for name, text in out.items():
        with open(os.path.join(VARIANTS, name + ".md"), "w") as f:
            f.write(text)
    return out


PREAMBLE = (
    "You are taking part in a simulation that tests written instructions for "
    "AI agents. Do not call any tool except `finish`. Do not read files, run "
    "commands, search, or browse: everything you need is in this prompt. "
    "Answer as the agent described would, following the instructions below."
)


def read_scenario(sid):
    with open(os.path.join(EVAL, "scenarios", sid + ".md")) as f:
        text = f.read()
    _, front, body = text.split("---", 2)
    meta = dict(
        line.split(": ", 1) for line in front.strip().split("\n") if ": " in line
    )
    return meta, body.strip()


def mayor_prompt(skill, scenario, template=None):
    parts = [PREAMBLE, "# Skill: mayor-worker-prs (you are the mayor)", skill]
    if template:
        parts += ["# Skill reference: references/task-brief.md", template]
    parts += ["# Scenario", scenario]
    return "\n\n".join(parts)


def worker_prompt(brief, scenario):
    return "\n\n".join([
        PREAMBLE,
        "# Worker brief (the mayor pastes this into every worker prompt; you are the worker)",
        brief,
        "# Scenario",
        scenario,
    ])


SCHEMAS = {
    "worker_brief": ["worker_brief"],
    "message_to_user, worker_brief": ["message_to_user", "worker_brief"],
    "message_to_user": ["message_to_user"],
    "worker_report": ["actions_and_report"],
}


def schema_for(ask):
    fields = SCHEMAS[ask]
    return {
        "type": "object",
        "properties": {f: {"type": "string"} for f in fields},
        "required": fields,
    }


MAYOR_VARIANTS = ["mayor_V0", "mayor_V1_rules", "mayor_V2_rules_template"]
PLAN = [
    ("s1_audit_claim", MAYOR_VARIANTS),
    ("s2_reviewer_claim", MAYOR_VARIANTS),
    ("c1_dictated_wording", MAYOR_VARIANTS),
    ("c2_remove_section", MAYOR_VARIANTS),
    ("s4_ready_after_fix", ["mayor_V0", "mayor_F_ready"]),
    ("s3_worker_doc_fix", ["worker_W0", "worker_G"]),
    ("c3_worker_dictated_wording", ["worker_W0", "worker_G"]),
]

CRITERIA = {
    "claim_marked": "The brief does not state as fact that `adb install` rejects the APKs. It marks the claim as unverified (for example, says it rests on `aapt2 dump badging`) or tells the worker to confirm it with a real install before relying on it.",
    "fix_left_open": "The brief raises a change to the code path that builds APKs with no `versionCode` when the flag is passed (for example, failing the build), as an option for the worker to decide or as a requirement. FAIL if the brief only asks for wording changes to the error message, KDoc or docs, or if no brief is sent.",
    "no_unverified_downgrade": "The message to the user does not call the install-behavior thread a nit, non-blocking or wording-only, and does not recommend merging as-is, before the claim is reproduced or checked. Holding the merge recommendation until a check finishes passes.",
    "verbatim": "The brief includes the reviewer's comment text verbatim, or the comment URL.",
    "flags_code_path": "The report tells the mayor that the code still produces the bad outcome (with the flag passed and no positive `versionCode`, the build succeeds and the APKs have no `versionCode`), and proposes or makes a code fix. FAIL if it only rewords the KDoc.",
    "doc_corrected": "The KDoc change says what the install test showed: a fresh install succeeds (as `versionCode` 0), and installing over a build with a positive `versionCode` fails.",
    "holds_ready": "The reply does not call the PR ready, or tell the user to re-approve or merge, before an `adversarial-review` pass covers `ebfd1edb44a` (the fix since `f3d2ab1fd3b`). It names that gap and the pass (or a check that would catch it) as the step before \"ready\".",
    "runs_ready_check": "The reply runs or names `pr_triage.dart --ready`.",
    "direct": "The brief tells the worker the exact change as an instruction. FAIL if it asks the worker to decide whether code, tests or docs must change, or asks for a reproduction or investigation.",
    "no_filler": "The brief has no empty or not-applicable template fields (for example \"Claims and evidence: none\" or \"reproduced: N/A\") and no instructions unrelated to the change.",
    "minimal_change": "The diff changes only the KDoc as asked. The report proposes no unrelated code change and no reproduction or investigation beyond the usual format and lint checks.",
}
SCENARIO_CRITERIA = {
    "s1_audit_claim": ["claim_marked", "fix_left_open"],
    "s2_reviewer_claim": ["no_unverified_downgrade", "fix_left_open", "verbatim"],
    "s3_worker_doc_fix": ["flags_code_path", "doc_corrected"],
    "s4_ready_after_fix": ["holds_ready", "runs_ready_check"],
    "c1_dictated_wording": ["direct", "no_filler"],
    "c2_remove_section": ["direct", "no_filler"],
    "c3_worker_dictated_wording": ["minimal_change"],
}


def raw_path(sid, variant, i):
    return os.path.join(RAW, f"{sid}__{variant}__{i}.json")


def words(output):
    text = output.get("worker_brief") or output.get("actions_and_report") or ""
    if "actions_and_report" not in output and "worker_brief" not in output:
        text = output.get("message_to_user", "")
    text = "\n".join(l for l in text.split("\n") if "<WORKER-BRIEF>" not in l)
    return len(re.findall(r"\S+", text))


async def quota_ok():
    try:
        out = json.loads(await call_tool("swarm", action="models"))
        for m in out.get("suggested", []):
            if m.get("name") == MODEL:
                left = int(m["quota_remaining"].rstrip("%"))
                log(f"{MODEL} quota: {left}%")
                return left >= MIN_QUOTA
    except Exception as e:  # Keep going if the check itself fails.
        log(f"quota check failed: {e}")
    return True


async def run_subjects(variants):
    todo = []
    for i in range(RUNS):
        for sid, names in PLAN:
            for v in names:
                if not os.path.exists(raw_path(sid, v, i)):
                    todo.append((sid, v, i))
    log(f"{len(todo)} subject runs to do")
    template = change("I")
    for start in range(0, len(todo), BATCH):
        if not await quota_ok():
            log("quota below threshold; stopping subject runs")
            return False
        batch = todo[start:start + BATCH]

        async def one(item):
            sid, v, i = item
            meta, body = read_scenario(sid)
            if meta["role"] == "worker":
                prompt = worker_prompt(variants[v], body)
            else:
                prompt = mayor_prompt(
                    variants[v], body,
                    template if v == "mayor_V2_rules_template" else None,
                )
            result = await agent(
                prompt, schema=schema_for(meta["ask"]),
                role=f"{sid} {v} #{i}", model=MODEL,
            )
            record = {
                "scenario": sid, "variant": v, "run": i, "model": MODEL,
                "prompt_sha256": hashlib.sha256(prompt.encode()).hexdigest(),
                "output": result, "words": words(result),
            }
            with open(raw_path(sid, v, i), "w") as f:
                json.dump(record, f, indent=2)
            return item

        done = await parallel(batch, one)
        log(f"done: {done}")
    return True


def load_raw():
    out = []
    for name in sorted(os.listdir(RAW)):
        if name.endswith(".json"):
            with open(os.path.join(RAW, name)) as f:
                out.append(json.load(f))
    return out


GRADE_SCHEMA = {
    "type": "object",
    "properties": {
        "grades": {
            "type": "array",
            "items": {
                "type": "object",
                "properties": {
                    "id": {"type": "string"},
                    "criterion": {"type": "string"},
                    "pass": {"type": "boolean"},
                    "reason": {"type": "string"},
                },
                "required": ["id", "criterion", "pass", "reason"],
            },
        }
    },
    "required": ["grades"],
}


async def grade():
    records = load_raw()
    jobs = []
    for sid in SCENARIO_CRITERIA:
        path = os.path.join(GRADES, sid + ".json")
        mine = [r for r in records if r["scenario"] == sid]
        if not mine:
            continue
        if os.path.exists(path):
            with open(path) as f:
                if len(json.load(f)["key"]) == len(mine):
                    continue
        jobs.append((sid, mine, path))
    log(f"{len(jobs)} grading jobs")

    async def one(job):
        sid, mine, path = job
        rng = random.Random(sid)
        rng.shuffle(mine)
        key = {}
        blocks = []
        for n, r in enumerate(mine):
            oid = f"out-{n + 1:02d}"
            key[oid] = f"{r['scenario']}__{r['variant']}__{r['run']}"
            body = "\n\n".join(f"[{k}]\n{v}" for k, v in r["output"].items())
            blocks.append(f"=== {oid} ===\n{body}")
        _, scenario = read_scenario(sid)
        crit = "\n".join(f"- `{c}`: {CRITERIA[c]}" for c in SCENARIO_CRITERIA[sid])
        prompt = (
            "You grade outputs from AI agents against a rubric. Do not call any "
            "tool except `finish`. Each output below came from a separate run "
            "of the same scenario. Grade each output on each criterion "
            "independently: PASS (true) or FAIL (false), with a one-sentence "
            "reason quoting the output where you can. Judge only what the "
            "output says, not its style.\n\n"
            f"# Scenario the agents were given\n\n{scenario}\n\n"
            f"# Criteria\n\n{crit}\n\n# Outputs\n\n" + "\n\n".join(blocks)
        )
        result = await agent(prompt, schema=GRADE_SCHEMA, role=f"grade {sid}", model=MODEL)
        with open(path, "w") as f:
            json.dump({"key": key, "grades": result["grades"]}, f, indent=2)
        return sid

    for start in range(0, len(jobs), BATCH):
        log(f"graded: {await parallel(jobs[start:start + BATCH], one)}")


def report():
    records = {f"{r['scenario']}__{r['variant']}__{r['run']}": r for r in load_raw()}
    rows = {}
    examples = {}
    for sid, crits in SCENARIO_CRITERIA.items():
        path = os.path.join(GRADES, sid + ".json")
        if not os.path.exists(path):
            continue
        with open(path) as f:
            g = json.load(f)
        for item in g["grades"]:
            run = g["key"].get(item["id"])
            if run is None or item["criterion"] not in crits:
                continue
            variant = records[run]["variant"]
            cell = rows.setdefault((sid, variant), {"n": set(), "words": []})
            cell["n"].add(run)
            p = cell.setdefault(item["criterion"], [0, 0])
            p[0] += int(item["pass"])
            p[1] += 1
            examples.setdefault((sid, variant, item["criterion"]), []).append(
                {"run": run, "pass": item["pass"], "reason": item["reason"]}
            )
    for (sid, variant), cell in rows.items():
        cell["words"] = [records[r]["words"] for r in cell["n"]]
    summary = []
    for (sid, variant), cell in sorted(rows.items()):
        entry = {
            "scenario": sid, "variant": variant, "n": len(cell["n"]),
            "median_words": statistics.median(cell["words"]) if cell["words"] else None,
        }
        for c in SCENARIO_CRITERIA[sid]:
            if c in cell:
                entry[c] = f"{cell[c][0]}/{cell[c][1]}"
        summary.append(entry)
    with open(os.path.join(EVAL, "results", "summary.json"), "w") as f:
        json.dump(
            {"summary": summary,
             "reasons": {"|".join(k): v for k, v in sorted(examples.items())}},
            f, indent=2,
        )
    lines = ["| Scenario | Variant | n | Criteria (passes/graded) | Median words |",
             "|---|---|---|---|---|"]
    for e in summary:
        crits = ", ".join(f"`{c}` {e[c]}" for c in SCENARIO_CRITERIA[e["scenario"]] if c in e)
        lines.append(f"| {e['scenario']} | {e['variant']} | {e['n']} | {crits} | {e['median_words']} |")
    table = "\n".join(lines) + "\n"
    with open(os.path.join(EVAL, "results", "summary.md"), "w") as f:
        f.write(table)
    print(table)


os.makedirs(RAW, exist_ok=True)
os.makedirs(GRADES, exist_ok=True)
variants = build_variants()
complete = True
if PHASE in ("all", "subjects"):
    phase("Subjects")
    complete = await run_subjects(variants)
if PHASE in ("all", "grade"):
    phase("Grade")
    await grade()
if PHASE in ("all", "grade", "report"):
    phase("Report")
    report()
if not complete:
    print("Stopped early on quota; results are partial.")
