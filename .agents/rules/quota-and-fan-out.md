---
trigger: always_on
---

# Quota and fan-out

- Before starting more than 3 agents, check quota with `swarm{action:"models"}`. If work that began above 10% of the current model's daily quota has dropped below 10%, don't start new agents for it; tell the user and wait. Work the user starts while quota is already low is important enough to run.
- Size a fan-out by the effort its agents run at: up to 12 agents at once on low effort, up to 6 on medium, up to 3 on high, and only 1 on the highest effort level. The user can override this for a task.
- Wait for CI with one background `gh pr checks <n> --watch`, never with repeated `schedule` timers, which resend your whole context on every wake-up.
