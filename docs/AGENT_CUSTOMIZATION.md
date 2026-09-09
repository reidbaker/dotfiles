# Gemini / Antigravity Customization & Sync Guide

This document outlines how the Gemini Agent runtime (Antigravity) discovers agents, skills, and personas, the root causes for missing or stalled customizations, and how to configure `AgentSyncer` in `dotfiles`.

---

## 1. Customization Discovery

The agent runtime searches for customizations across specific locations depending on the environment:

### Global (Machine-Local) Discovery
* **Customization Root**: `~/.gemini/config/` (Note: older versions used `~/.gemini/skills`, but recent versions migrated all global configurations to `~/.gemini/config/`).
* **Declarative Agents (YAML)**:
  * Scans subdirectories in `~/.gemini/config/agents/<agent_name>/` containing `agent.json` (manifest) and `config.yaml` (spec).
  * Scans external directories registered in `~/.gemini/config/agents.json`.
* **Skills**:
  * Scans subdirectories in `~/.gemini/config/skills/<skill_name>/` (containing `SKILL.md`).
  * Scans external directories registered in `~/.gemini/config/skills.json`.
* **Plugins**:
  * Scans subdirectories in `~/.gemini/config/plugins/<plugin_name>/` (containing `plugin.json`).

### Workspace / Repository Discovery
* **Git Repositories**: Scans `.agents/` or `_agents/` at repository root or walks up the directory hierarchy.

---

## 2. Declarative Agent Format (agent.json + config.yaml)

Declarative agents use a structured directory layout:
```text
.agents/agents/<agent_name>/
├── agent.json        # Manifest (AgentScriptItem proto)
└── config.yaml       # Configuration (CustomAgentSpec proto)
```

### Manifest (`agent.json`)
`agent.json` defines agent metadata and UI visibility. `mainAgent: true` belongs here:
```json
{
  "name": "reidbaker-agent",
  "description": "An agent configured with the 'Expert' persona for maximum rigor and candor.",
  "mainAgent": true,
  "configPath": {
    "relativePathToConfig": "config.yaml"
  }
}
```

### Configuration (`config.yaml`)
`config.yaml` maps directly to `CustomAgentSpec`. Do NOT include `mainAgent: true` here, as `CustomAgentSpec` does not define this field and strict YAML-to-proto parsing will fail:
```yaml
coding_agent:
  google_mode: true
  agentic_mode: true
command_execution_policy: auto

prompt_section_customization:
  append_prompt_sections:
    - title: "Expert Communication Guidelines"
      content: |
        You are a world class expert in all domains...
```

---

## 3. Critical Configuration Requirements & Root Causes

1. **Target Directory Mismatch (`~/.agents` vs `~/.gemini/config`)**:
   * `sync_agents.dart` links agents to `~/.agents/agents/` and skills to `~/.agents/skills/`.
   * While `~/.agents/` is a standard cross-runtime directory for tools like Claude/Codex, the runtime only scans `~/.gemini/config/` by default.
   * Without symlinks or JSON registration in `~/.gemini/config/`, the runtime does not index `~/.agents/` directly.

2. **Placement of `mainAgent: true`**:
   * `mainAgent: true` is a field on the `AgentScriptItem` manifest (`agent.json` or frontmatter for `.md` agents).
   * It tells the UI to include the agent in the main chat persona dropdown.
   * It MUST NOT be placed inside `config.yaml` (which maps to `CustomAgentSpec`).

3. **`google_mode` Setting**:
   * When running in Google environments, `coding_agent.google_mode: true` ensures backend authentication routing and internal tools operate properly.

4. **Section Title Collision**:
   * Do not append sections with the title `"identity"`, as `<identity>` is an internal built-in prompt section. Use a distinct title like `"Expert Communication Guidelines"`.

---

## 4. Recommended Updates for `AgentSyncer` (`lib/agent_syncer.dart`)

To automate this workflow on future `dart run bin/sync_agents.dart` runs:

### A. Add Target Paths for `~/.gemini/config/`
```dart
String get geminiConfigDir => pathContext.join(homeDir, '.gemini', 'config');
String get geminiAgentsDir => pathContext.join(geminiConfigDir, 'agents');
String get geminiConfigSkillsDir => pathContext.join(geminiConfigDir, 'skills');
```

### B. Update Sync Targets in `AgentSyncer`
1. **Skills**:
   * Replace legacy `~/.gemini/skills` target with `~/.gemini/config/skills/` (or symlink `~/.gemini/config/skills` directly to `~/.agents/skills`).
2. **Agents**:
   * Link custom agent directories into both `~/.agents/agents/<name>` and `~/.gemini/config/agents/<name>`.
3. **JSON Configs (Optional / Redundant backup)**:
   * Write `~/.gemini/config/agents.json` and `~/.gemini/config/skills.json` manifests pointing to `~/.agents/`.
