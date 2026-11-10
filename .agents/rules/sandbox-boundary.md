# Sandbox & Credential Boundary (`reidbaker` vs `reidbaker-agent`)

- Agents operate as `reidbaker-agent` (default `~/.config/gh`) and do their repository checkout/PR work inside `~/reidbaker-agent-sandbox/`.
- Agents MUST NEVER read, write, `cd` into, or run commands targeting `~/reidbaker-sandbox/` or `~/.config/gh-reidbaker/`, nor set `GH_CONFIG_DIR` to `~/.config/gh-reidbaker`. Those directories are strictly reserved for the human user (`reidbaker`).
