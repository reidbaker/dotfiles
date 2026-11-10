---
trigger: always_on
---

# Keep-awake processes (macOS)

- On macOS, when you start `caffeinate` (or any keep-awake process) for unattended work, always pass `-t <seconds>` sized to the expected run, so it ends even if you forget it.
- Record the spawned PID (`pid=$!`) and stop only your own `caffeinate` process (`kill "$pid"`) as soon as your work is done (never `pkill -x caffeinate`, which terminates sibling sessions' keep-awake processes).
- Keep it running across turns only while your own agents, workflows or commands are still working, or if the user asks you to keep it on.
