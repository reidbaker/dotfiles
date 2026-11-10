# Cross-machine sync (Mac and Cloudtop)

- When you modify shared skills, rules, personas, or sync tooling, keep both the Mac and the Cloudtop in sync without waiting to be asked:
  1. **`~/devconfig` (portable skills, rules, `pr_triage`, `sync_agents`)**: once landed on `master`, push `master` to `origin` and `cloudtop`, fast-forward `~/devconfig` on the other machine, and run `dart run bin/sync_agents.dart` on both machines. Scope machine-specific entries with `"machines": ["mac"]` or `["cloudtop"]` in `.agents/skills/skills_config.json` or `.agents/rules/rules_config.json`.
  2. **`~/Documents/ai-sandbox/scratch` (`ai-team` / `persona-review`)**: push `main` to `origin` (the Cloudtop bare repo) and fast-forward the checkout on the other machine. When gitignored `ai-team/*/raw` or `ai-team/shared/raw` caches change, also run `dart run ~/Documents/ai-sandbox/scratch/ai-team/bin/persona_tools.dart sync-raw` (`--pull` from the receiving side).
  3. **External skill links (`~/.gemini/config/external_skills/skills_config.json`)**: update both machines' `external_skills/skills_config.json` when adding or removing an external skill root, then run `dart run bin/sync_agents.dart` on both machines.
- Test SSH reachability with `ssh -o BatchMode=yes` first; if the shared SSH connection or `gcert` is expired, tell the user the one command needed to refresh it rather than hanging.
