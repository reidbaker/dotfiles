---
trigger: always_on
---

# Secret & Credential Hygiene

- Never run `gh auth token`, `gh auth status --show-token`, or print secret fields from `hosts.yml`, `mcp_config.json`, or environment variables into command output or chat transcripts.
- Never pass raw personal access tokens or API keys inline in `run_command` strings.
- Check authentication status using `gh auth status` (without `--show-token`) and inspect file permissions with `ls -l`, never `cat`.
