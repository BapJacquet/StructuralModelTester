# Agents

## Live UI control

While `run_app()` is running and **one** browser tab is connected, control the session with `smt_cmd()` from a second R process (`pkgload::load_all` on this tree). Prefer that over browser automation.

```r
pkgload::load_all(".")
smt_cmd("ping")
smt_cmd_actions()
```

Full protocol, paths, ownership rules, and action reference: **[docs/agent-live-commands.md](docs/agent-live-commands.md)**.

## Quick rules

1. `ping` / `status` before mutating; check `ok` on every result.
2. Serialize commands (`wait = TRUE`); ~1 command per 400 ms on the app side.
3. Keep a single browser owner tab; do not reconnect mid-batch.
4. Expected effects: `set_edge_prior`; sim knobs: `set_node_sim`; kinds: `set_sim_var_kind`.
5. Confirm model state with `get_model` (UI Expected-b fields update only when that edge is selected).
6. Shared root is `<pkg>/.smt` (pinned by `run_app()`). Timeouts ⇒ app down, no owner tab, or wrong root.

## Debug UI / reactive bugs

Instrument with `smt_debug()` / JS `smt_debug_event`, read `smt-debug.log`, then apply the smallest fix. See `.cursor/rules/debug-with-logs.mdc`.
