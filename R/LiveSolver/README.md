# Live Solver

An interactive (Shiny) front end for the OptimalEcospace optimization loop. You pick an existing Ecospace model in this repository, set the constraints for an artificial-structure **installation**, **decommissioning**, or **mixed** strategy, and the solver runs Ecospace scenario after scenario. Scores, the cost frontier and the best layout update after every run.

## Getting started (no R experience needed)

1. Install **R** from https://cloud.r-project.org. RStudio (https://posit.co/download/rstudio-desktop/) is optional.
2. For real scenarios, install the **Ecospace console** (`EwEClientConsole.exe`, Windows). Without it the app still runs in **test mode**, which uses made-up scores so you can learn the interface.
3. Download **[OptimalEcospace-LiveSolver.zip](https://github.com/mattwoodstock/OptimalEcospace/releases/download/live-solver/OptimalEcospace-LiveSolver.zip)** (about 1.5 MB) and unzip it, ideally to a short path such as `C:\OptimalEcospace`. It contains only what the Live Solver needs (see *Building the download* below). Cloning the whole repository works too.
4. Start the app:
   * **Windows:** double-click `Start Live Solver.bat` in the repository folder.
   * **RStudio:** open `R/LiveSolver/launch.R` and click **Source**.
   * **Any R console:** `source("R/LiveSolver/launch.R")` from the repository folder.

The first start installs the R packages it needs (`shiny`, `bslib`, `DT`, `processx`, `terra`, `xml2`, `jsonlite`), which takes a few minutes. The app opens in your web browser; keep the R window open while you use it.

In the app, the **Start here** tab explains the workflow and checks that the console is found. You can also enter the console's location there. If you put the console in a `console/` folder inside the repository, it is found automatically.

Developers can still launch directly with `shiny::runApp("R/LiveSolver")` from the repository root.

The app looks for models at `Model/<region>/*.eweaccdb`. For each model it expects the matching inputs in `Inputs/<region>/`: an `*STConfig.xml`, a command-file template (`*CommandFile*.txt`), `basemap.asc` and the habitat `.asc` layers. Add a new region by following the same layout, and it shows up in the model list.

To run without the UI (for example overnight), save a configuration from the app (**Setup → Save / load configuration**), then run:

```bash
cd R/LiveSolver
Rscript run_headless.R path/to/config.json
```

## What you can configure

| Section | Parameters |
|---|---|
| **Start here** | location of the Ecospace console (`EwEClientConsole.exe`) |
| **Model & run** | model, Ecosim/Ecospace scenario index, run length, time steps per year, spin-up on/off and length, threads, per-run timeout, which STConfig layer holds the artificial structures, which natural habitat layers are displaced or restored, basemap, existing-structures map, pristine map source (current Inputs or `Backup_Base_Maps`), the cover of each existing structure cell (default: the same as one new structure, because structure maps are often presence = 1), extra raw command-file lines (vulnerabilities, dispersal, environmental responses, …) |
| **Strategy** | install, decommission, or mixed; for mixed, a shared or separate budget |
| **Installation** | budget (default $500,000); a random number of new structures per scenario between the min (default 1) and max (default 500), capped by what the budget can buy, or optionally always fill the budget; how much of its cell one structure covers (a fixed share, or a footprint area in km² divided by each cell's area); cost from a cost map (× multiplier), from depth (USD/m) or a fixed cost, plus a fixed per-structure cost and optional scaling by coverage; exclusions map; extra allowed-area mask; depth range; lon/lat bounding box; min spacing between new structures; buffer around existing structures; avoid cells rich in chosen natural habitats (threshold); placement pattern (random, clustered around hubs, prefer cheaper cells, prefer deeper cells); how natural habitat under a new structure is reduced (proportionally, subtract as in the legacy script, or unchanged) |
| **Decommissioning** | removal budget; min/max removals; min structures retained; full or partial removal fraction; minimum coverage for a structure to be eligible; cost (fixed, depth-based or cost map); depth range; bounding box; selection preference (random, cheapest, costliest, shallowest, deepest); what the freed area becomes (natural habitats proportionally, a single named habitat, or nothing) |
| **Objectives** | editable table (name, output-file pattern, target group, `species`/`sum`/`biodiversity`, `max`/`min`, weight); average over the last *N* output rows; which objective is used for ROI per dollar |
| **Search** | random seed; run 1 is always the no-change status quo that every scenario is compared against; random search or evolutionary search (random warm-up, then mutations of the current best run) |
| **Next to the Start button** | number of scenarios to run; test mode (no Ecospace, made-up scores) |

Every field has a default and a blank field uses it. Depth limits and the lon/lat box default to the map's depth range and extent.

Structure layers hold the **proportion of each cell that is structure** (0-1), which is what Ecospace reads. All maps shade structure cells by that proportion. New structures are outlined in orange and decommissioned ones in magenta with a cross, so changes stay visible at any cover. Partial removal leaves the remaining share in the cell.

**Sample a candidate** draws one layout under the current constraints so you can check feasibility before you spend console time. The preview map shows which cells are eligible for installation and which are eligible for decommissioning.

## While it runs

* **Pause after this run**, **Resume**, **Stop & restore**.
* **Add iterations** extends a session that is going well.
* **Re-score** applies edited objective weights and directions to every run so far. The evolutionary search then follows the new ranking.
* The composite score is the weighted sum of min–max normalised objectives over all finished runs. A run is on the *cost frontier* when no other run scores at least as high for the same or lower cost.

## Building the download

`build_package.R` assembles `OptimalEcospace-LiveSolver.zip` with only the files a run needs:

* the app and launcher (`Start Live Solver.bat`, `R/LiveSolver/*.R`)
* for each region in `Inputs/`, the model named by `<EWE_MODEL_FILE>` in its command-file template
* the region's STConfig file, command-file template, and the rasters the solver reads or rewrites: every habitat layer in the STConfig, `basemap.asc`, `exclusions.asc` and `cost_map.asc`

Sediment source data (`Inputs/dbSeaBed`), model backups, logs, `Backup_Base_Maps` and other working files are left out. Any new region or habitat layer added to an STConfig is picked up automatically.

The *Live Solver package* GitHub Actions workflow builds the zip on every push that touches the app, `Inputs/` or `Model/`. It then unzips the package into an empty folder on a Windows runner and runs a test-mode session there (`check_package.R`), to prove the package works on its own. On `main` it publishes the zip to the `live-solver` release, so the download link above always serves the current version. To build it locally, run `Rscript R/LiveSolver/build_package.R` from the repository root; the zip goes to `dist/` (on Windows it uses PowerShell if no zip program is installed).

## Comparing scenarios

The **Results** tab shows, for the top scenarios, the percent change of every outcome compared with the no-change baseline. Blue bars are improvements in the direction you chose for that outcome; orange bars are worse. Click any scenario in the table to see its map and a table of its outcomes. The same percent changes are saved as `<objective>_vs_status_quo_pct` columns in `optimization_results.csv`.

## Outputs

Each session writes to `Outputs/<region>/LiveSolver_<timestamp>/`:

* `optimization_results.csv`: one row per run, with structures added and removed, costs, objectives, composite score and frontier flag
* `Run_XXXX/`: `Maps/` (every habitat layer used), `candidate.json` (cell indices), `cmd.txt`, console logs and the Ecospace CSVs
* `Best_Maps/`: the best run's layers
* `solver_config.json`: the exact configuration. Load it in the app or pass it to `run_headless.R` to reproduce the session.
* `solver_log.txt`

## How maps reach Ecospace

Ecospace reads one fixed STConfig file and the map files it lists, so the solver works the same way `structure_optimization.R` does: before every run it overwrites those map files in `Inputs/<region>/` with the run's layers. A copy of each run's layers is also kept in its `Maps/` folder.

* The region's own STConfig (e.g. `Inputs/NGoM/NGoMSTConfig.xml`) is used, with its file paths repaired in place to point at this folder's `Inputs` maps. This matters because the stored paths can come from another computer.
* Before the first run, the original maps and STConfig are snapshotted. They are restored when the session finishes, is stopped, or the browser tab closes. If R crashes mid-session, a `.livesolver_lock` file stays in the Inputs folder, and the next session (app or headless) restores the snapshot automatically before it starts.
* **Ecospace may read a different set of map files.** The console can follow the spatial configuration saved inside the model rather than the one it is given (for example, your original working folder). After the first run, the solver reads Ecospace's spatial log, which lists exactly which map files were loaded. If they are elsewhere, it switches to updating those files (snapshotting them first and restoring them afterwards), tells you where they are, and repeats the status quo run with its own maps. The first run is kept as a `probe` and is not scored.
* Each copy is verified. If a map can't be overwritten (for example because it is open in another program), the session stops with a message.
* After each run, the solver keeps Ecospace's spatial log (`<model>_spatiallog.txt`) in the run folder. It warns in the log and in the app if Ecospace did not load the overwritten structure map, or if a scenario that changed structures produced outcomes identical to the status quo.

The engine (`solver_engine.R`) has no Shiny dependency, so other scripts can reuse it (`ls_build_problem`, `ls_generate_candidate`, `ls_apply_candidate`, `ls_evaluate_objectives`, `ls_run_session`, …).
