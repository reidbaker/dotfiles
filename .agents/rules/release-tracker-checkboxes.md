---
trigger: always_on
---

# Release Tracker Checkbox Updates

- On Flutter release tracker issues (`flutter/flutter` release checklist issues), you do **not** need to ask the user before checking off a checklist item (`- [ ]` → `- [x]`) as `reidbaker` **if and only if**:
  1. You have fresh, direct proof (merged PR URL, commit SHA, build ID, live archive URL, etc.) that the step happened and succeeded.
  2. No one else has edited or modified that checklist line from its expected template/previous state.
- Still never close a release tracker issue yourself unless explicitly asked.
