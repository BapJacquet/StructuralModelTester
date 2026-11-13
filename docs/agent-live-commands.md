# Agent guide: live `smt_cmd` control

Drive a **running** Structural Model Tester Shiny session from a second R process. Prefer this over browser automation for model edits, expected effects, simulate/run, and status checks — the UI updates in the open tab.

## Preconditions

1. App is running (`run_app(...)`, typically `port = 3838`).
2. **Exactly one** browser tab is connected to that app (the command bus is owned by one Shiny session).
3. Agent R session has the package loaded from the **same source tree**:

```r
pkgload::load_all("/path/to/Structural Model Tester")
# or: library(StructuralModelTester) after install_local of that tree
```

4. Shared command root resolves to `<pkg>/.smt` (see [Paths](#paths)).

Verify before mutating:

```r
smt_cmd("ping")     # expect ok=TRUE, message="pong"
smt_cmd("status")   # n_nodes, n_edges, has_data, has_result, …
smt_cmd_actions()   # authoritative action list
```

If `ping` times out: no owning session is polling — open/refresh the browser tab, confirm `run_app()` is up, and that client/server share the same `.smt` root.

## Paths

| Location | Role |
|----------|------|
| `.smt/inbox/*.json` | Commands written by `smt_cmd()` |
| `.smt/results/<id>.json` | Results written by the owning Shiny session |
| `.smt/owner.json` | Active session token + heartbeat |

Resolution order for the root: option `smt.cmd.root` → env `SMT_CMD_ROOT` → `<package>/.smt`.  
`run_app()` pins `options(smt.cmd.root)` to the package `.smt` so Shiny’s `inst/app` working directory does not fork a second inbox.

`.smt/` is gitignored. Do not commit inbox/results.

## Protocol (agents)

- **One command at a time** with `wait = TRUE` (default). The app processes ~1 command / 400 ms.
- Check `ok` on every result. On `ok = FALSE`, read `message` and recover (do not assume state changed).
- Prefer **labels** (`from = "X", to = "Y"`) or node `id`/`label` interchangeably for nodes; edges accept `id =` or `from=`/`to=` (optional `level` / `to_level`).
- After structural edits, call `status` or `get_model` before simulate/run.
- Do **not** open a second browser tab to the same app while commanding — ownership would split or steal mid-batch.
- Prefer `smt_cmd` over clicking the UI for reproducible agent workflows; use the browser only to **observe** (screenshot / user view).

## Typical workflow

```r
pkgload::load_all(".")

smt_cmd("ping")

smt_cmd("add_node", label = "X", kind = "observed")
smt_cmd("add_node", label = "Y", kind = "observed")
smt_cmd("add_edge", from = "X", to = "Y", type = "confirmatory")

# Expected unstandardized b (used by Simulate / Power)
smt_cmd("set_edge_prior", from = "X", to = "Y", mean = 0.5, sd = 0.2)

# Exogenous X ~ N(mean, sd); DV residual / intercept on outcome scale
smt_cmd("set_node_sim", node = "X", mean = 0, sd = 1)
smt_cmd("set_node_sim", node = "Y", intercept = 0, resid_sd = 1)

smt_cmd("set_sim_var_kind", id = "Y", kind = "continuous")  # or binary / categorical + levels=

smt_cmd("simulate", n = 200, seed = 1)
# or, with mapped data: smt_cmd("run")

st <- smt_cmd("status")
mod <- smt_cmd("get_model")  # $json is project JSON
```

Load a saved project or replace the whole model:

```r
smt_cmd("load_project", path = "path/to/model.json")
smt_cmd("set_model", path = "path/to/model.json")
# smt_cmd("set_model", json = json_string)
smt_cmd("open_data", path = "path/to/data.csv")
```

## Action reference

All actions return a list with at least `id`, `ok`, `action`, and usually `message`.

| Action | Arguments | Notes |
|--------|-----------|--------|
| `ping` | — | Liveness |
| `status` | — | Counts + flags (`has_data`, `has_result`, `simulated`, …) |
| `get_model` | — | `$json` = full project JSON |
| `load_project` | `path` | Existing project JSON file |
| `set_model` | `json` and/or `path` | Replace model state |
| `open_data` | `path` | CSV / Excel / RDS |
| `add_node` | `label`, optional `kind` (`observed`), `observed_column` | |
| `add_latent` | `label` | |
| `add_interaction` | — | Adds × node |
| `remove_node` | `id` or `node` | id or label |
| `add_edge` | `from`, `to`, optional `type`, `level`, `to_port`, `to_level` | `type`: fixed / confirmatory / exploratory / measurement |
| `remove_edge` | `id` | Edge id required |
| `map_column` | `id`/`node`, `column` | Map data column onto node |
| `set_sim_var_kind` | `id`/`node`, `kind`, optional `levels` | `continuous` / `binary` / categorical kinds; `levels` sets coding |
| `set_node_sim` | `id`/`node`, optional `mean`, `sd`, `intercept`, `resid_sd`, `min`, `max` | Only provided fields change |
| `set_edge_prior` | `id` **or** `from`+`to`, `mean`, `sd`, optional `family` | Expected effect; default family `normal` |
| `set_edge_rel_kind` | `id` **or** `from`+`to`, `rel_kind` | e.g. `linear` |
| `clear_fit` | — | Drop fit / power results |
| `simulate` | optional `n`, `seed` | Simulate from priors + fit |
| `run` | — | Fit current analysis data |
| `cancel_power` | — | Request cancel of in-flight power |

List at runtime: `smt_cmd_actions()`.

## Results and timeouts

```r
smt_cmd("ping", timeout_sec = 10)
smt_cmd("simulate", n = 500, seed = 1, timeout_sec = 120)

# Fire-and-forget then await:
q <- smt_cmd("run", wait = FALSE)
smt_cmd_await(q$id, timeout_sec = 120)
```

Timeouts usually mean: app down, no browser owner, or wrong `.smt` root. Inspect `.smt/inbox/` (stuck files) and `.smt/owner.json` (heartbeat age).

## Ownership (multi-tab)

Only the session recorded in `.smt/owner.json` consumes the inbox. A new tab steals ownership on connect; a heartbeat older than ~2 s is treated as abandoned. Agents should keep **one** live tab and avoid reconnecting mid-command batch.

## Debugging

- App / agent traces: `smt_debug(...)` → gitignored `smt-debug.log` (see `.cursor/rules/debug-with-logs.mdc`).
- Command handling logs lines like `smt_cmd | action=… ok=…`.
- UI toast / status string often mirrors the last command `message`.

## What not to do

- Do not treat browser DevTools clicks as the primary control channel when `smt_cmd` works.
- Do not assume Expected b UI fields update unless that edge is selected; **state** is updated regardless — confirm with `get_model`.
- Do not run parallel `smt_cmd` waiters against the same app; serialize commands.
- Do not point `load_all` / install at a different clone than the running `run_app()` (diverging `.smt` paths).
