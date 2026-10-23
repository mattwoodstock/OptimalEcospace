# Live Solver

An interactive (Shiny) front end for the OptimalEcospace optimization loop. You pick an existing Ecospace model in this repository, set the constraints for an artificial-structure **installation**, **decommissioning**, or **mixed** strategy, and the solver runs Ecospace scenario after scenario. Scores, the cost frontier and the best layout update after every run.

## Requirements

* R (≥ 4.1) with `shiny`, `bslib`, `DT`, `processx`, `terra`, `xml2`, `jsonlite`:
  ```r
  install.packages(c("shiny", "bslib", "DT", "processx", "terra", "xml2", "jsonlite"))
  ```
* The Ecospace console (`EwEClientConsole.exe`, Windows) for real runs. Without it, you can still use **dry run** mode to test constraints and the workflow. Dry-run objectives are synthetic placeholders, not model output.

## Launch

From the repository root:

```r
shiny::runApp("R/LiveSolver")
```

The app looks for models at `Model/<region>/*.eweaccdb`. For each model it expects the matching inputs in `Inputs/<region>/`: an `*STConfig.xml`, a command-file template (`*CommandFile*.txt`), `basemap.asc` and the habitat `.asc` layers. Add a new region by following the same layout, and it shows up in the model list.

To run without the UI (for example overnight), save a configuration from the app (**Setup → Save / load configuration**), then run:

```bash
cd R/LiveSolver
Rscript run_headless.R path/to/config.json
```

## What you can configure

| Section | Parameters |
|---|---|
| **Model & run** | model, console path, Ecosim/Ecospace scenario index, run length, time steps per year, spin-up on/off and length, threads, per-run timeout, which STConfig layer holds the artificial structures, which natural habitat layers are displaced or restored, basemap, existing-structures map, pristine map source (current Inputs or `Backup_Base_Maps`), extra raw command-file lines (vulnerabilities, dispersal, environmental responses, …) |
| **Strategy** | install, decommission, or mixed; for mixed, a shared or separate budget |
| **Installation** | budget; fill the budget or pick a random count; min/max number of new structures; cell coverage per structure; cost from a cost map (× multiplier), from depth (USD/m) or a fixed cost, plus a fixed per-structure cost and optional scaling by coverage; exclusions map; extra allowed-area mask; depth range; lon/lat bounding box; min spacing between new structures; buffer around existing structures; avoid cells rich in chosen natural habitats (threshold); placement pattern (random, clustered around hubs, prefer cheaper cells, prefer deeper cells); how natural habitat under a new structure is reduced (proportionally, subtract as in the legacy script, or unchanged) |
| **Decommissioning** | removal budget; min/max removals; min structures retained; full or partial removal fraction; minimum coverage for a structure to be eligible; cost (fixed, depth-based or cost map); depth range; bounding box; selection preference (random, cheapest, costliest, shallowest, deepest); what the freed area becomes (natural habitats proportionally, a single named habitat, or nothing) |
| **Objectives** | editable table (name, output-file pattern, target group, `species`/`sum`/`biodiversity`, `max`/`min`, weight); average over the last *N* output rows; which objective is used for ROI per dollar |
| **Search** | iterations; random seed; the two baselines (no change, and every eligible cell regardless of budget); random search or evolutionary search (random warm-up, then mutations of the current best run); how maps reach Ecospace; dry run |

**Sample a candidate** draws one layout under the current constraints so you can check feasibility before you spend console time. The preview map shows which cells are eligible for installation and which are eligible for decommissioning.

## While it runs

* **Pause after this run**, **Resume**, **Stop & restore**.
* **Add iterations** extends a session that is going well.
* **Re-score** applies edited objective weights and directions to every run so far. The evolutionary search then follows the new ranking.
* The composite score is the weighted sum of min–max normalised objectives over all finished runs. A run is on the *cost frontier* when no other run scores at least as high for the same or lower cost.

## Outputs

Each session writes to `Outputs/<region>/LiveSolver_<timestamp>/`:

* `optimization_results.csv`: one row per run, with structures added and removed, costs, objectives, composite score and frontier flag
* `Run_XXXX/`: `Maps/` (every habitat layer used), `candidate.json` (cell indices), `cmd.txt`, console logs and the Ecospace CSVs
* `Best_Maps/`: the best run's layers
* `solver_config.json`: the exact configuration. Load it in the app or pass it to `run_headless.R` to reproduce the session.
* `solver_log.txt`

## How maps reach Ecospace

* **Overwrite Inputs** (default, as in `structure_optimization.R`): each run's layers are copied over the files in `Inputs/<region>/`. The solver points the console at a session copy of the STConfig file whose paths lead to those Inputs files, so it works wherever the repository is cloned. Before the first run, the original maps are snapshotted. They are restored when the session finishes, is stopped, or the browser tab closes. If R crashes mid-session, a `.livesolver_lock` file stays in the Inputs folder, and the next session (app or headless) restores the snapshot automatically before it starts.
* **Per-run STConfig**: the Inputs folder is never touched. Each run gets its own STConfig file pointing at that run's `Maps/` folder.

The engine (`solver_engine.R`) has no Shiny dependency, so other scripts can reuse it (`ls_build_problem`, `ls_generate_candidate`, `ls_apply_candidate`, `ls_evaluate_objectives`, `ls_run_session`, …).
