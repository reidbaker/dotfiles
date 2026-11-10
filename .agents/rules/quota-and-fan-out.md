---
trigger: always_on
---

# Quota and fan-out

- Before starting more than 3 agents, check quota with `swarm{action:"models"}`. If the current model is below 25% of its daily quota, don't start new agents; tell the user and wait.
- Size a fan-out by the effort its agents run at: up to 12 agents at once on low effort, up to 6 on medium, up to 3 on high, and only 1 on the highest effort level. The user can override this for a task.
- Wait for CI with one background `gh pr checks <n> --watch`, never with repeated `schedule` timers, which resend your whole context on every wake-up.
