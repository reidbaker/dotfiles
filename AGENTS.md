# AGENTS.md

## Stack & Layout
- **Language**: Dart (SDK >= 3.13).
- **Custom Skills**: `.agents/skills/<skill-name>/`
- **Third-Party Skills**: `third_party/<repo>/skills/<skill-name>/` (must include `LICENSE`).
- **Agent Personas**: `.agents/agents/<agent-name>/`

## Commands & Workflow
- **Lint Skills**: `dart run skills_lint`
- **Static Analysis**: `dart analyze --fatal-infos`
- **Check Complexity**: `dart run cognitive_complexity --fail-threshold 15 lib bin test`
- **Run Tests**: `dart test`
- **Sync System**: `dart run bin/sync_agents.dart`

## Hard Rules
- All Dart code must pass `dart analyze --fatal-infos` with zero issues.
- All Dart code must maintain cognitive complexity <= 15 per declaration.
- Keep static context dense and minimal; procedural task knowledge belongs in dynamic skills (`SKILL.md`).
- Commit messages must concisely state what changed and why.
