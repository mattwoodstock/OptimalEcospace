# Live Solver

An interactive (Shiny) front end for the OptimalEcospace optimization loop. It follows the same process as `structure_optimization.R` (reef modules, random budgets, natural habitat making room for reef, objectives scored against the status quo), so a non-modeller can run it from a browser. You pick an existing Ecospace model in this repository, set the constraints for an artificial-structure **installation**, **decommissioning**, or **mixed** strategy, and the solver runs Ecospace scenario after scenario. Scores, the cost frontier and the best layout update after every run.

## Getting started (no R experience needed)

1. Install **R** from https://cloud.r-project.org. RStudio (https://posit.co/download/rstudio-desktop/) is optional.
2. For real scenarios, install the **Ecospace console** (`EwEClientConsole.exe`, Windows). Without it the app still runs in **test mode**, which uses made-up scores so you can learn the interface.
3. Download **[OptimalEcospace-LiveSolver.zip](https://github.com/mattwoodstock/OptimalEcospace/releases/download/live-solver/OptimalEcospace-LiveSolver.zip)** (about 1.5 MB) and unzip it, ideally to a short path such as `C:\OptimalEcospace`. It contains only what the Live Solver needs (see *Building the download* below). Cloning the whole repository works too.
4. Start the app:
   * **RStudio:** open `Start Live Solver.R` (in the unzipped folder) and click **Source**.
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
| **Model & run** | model, Ecosim/Ecospace scenario index, run length, time steps per year, spin-up on/off and length, threads (all filled in from the region's command-file template), per-run timeout, which STConfig layer holds the artificial structures, which natural habitat layers make room for reef, basemap, existing-structures map, where the pristine maps come from (`Initial Base Mapps` when present, the current Inputs, or `Backup_Base_Maps`), how existing structure cells are read (fractional maps kept, presence 0/1 maps given one module's cover), extra raw command-file lines (vulnerabilities, dispersal, environmental responses, …) |
| **Strategy** | install, decommission, or mixed; for mixed, a shared or separate budget |
| **Installation: reef modules** | as in `structure_optimization.R`: each scenario draws a random budget up to the maximum (default $6,000,000) and spends it in drops of up to 100 modules per cell, with a batch discount of up to 30% for a full drop. Module cost (default $2,500), footprint (6 m²), height (3 m), minimum water above the crest (6 m), halo radius (500 m, which grows with neighbouring reef), maximum reef share of a cell, depth cost factor and reference depth, optional connectivity preference. Reef cover in a cell grows as `cap − (cap − cover)·exp(−increment/cap)`. Where modules may go: exclusions map, extra allowed-area mask, depth range, lon/lat box, buffer from existing structures, avoid cells rich in chosen natural habitats. Natural habitat under new reef makes room for it (natural + reef ≤ 1) or is left unchanged |
| **Decommissioning** | removal budget; min/max removals; min structures retained; full or partial removal fraction; minimum coverage for a structure to be eligible; cost (fixed, depth-based or cost map); depth range; bounding box; selection preference (random, cheapest, costliest, shallowest, deepest); what the freed area becomes (natural habitats proportionally, a single named habitat, or nothing) |
| **Objectives** | four defaults, all outcomes to increase: red snapper biomass and catch (`red snapper adult`), biomass of the other federally managed reef fish, and fishery revenue. More can be added from a list: other managed reef fish catch, all managed reef fish biomass or catch, commercial and recreational catch, commercial revenue, total biomass and catch, biodiversity, lionfish biomass (lower is better), and each managed species' biomass and catch. Custom objectives can also be added, and every entry is editable (output-file pattern, target groups, fleet, type, direction, weight; weight 0 = reported only). Values are averaged over the last *N* years (default 5). Choose which objective gives pounds gained per module and per dollar, and which gives revenue gain and payback |
| **Economics** | pounds per tonne, dockside price per species ($/lb, default prices from the script), default price for other groups, fleet multipliers |
| **Search** | random seed; run 1 is always the no-change status quo that every scenario is compared against; random search, or evolutionary search (random warm-up, then a parent from the Pareto front of the weighted objectives loses 10–40% of its reef cells and the freed budget is spent again) |
| **Next to the Start button** | number of scenarios to run; test mode (no Ecospace, made-up scores) |

Every field has a default and a blank field uses it. Depth limits and the lon/lat box default to the map's depth range and extent. Cells are also excluded where the module would leave less than the minimum clearance above its crest.

Structure layers hold the **proportion of each cell that is structure** (0-1), which is what Ecospace reads. All maps shade structure cells by that proportion. New structures are outlined in orange and decommissioned ones in magenta with a cross, so changes stay visible at any cover. Partial removal leaves the remaining share in the cell.

**Sample a candidate** draws one layout under the current constraints so you can check feasibility before you spend console time. The preview map shows which cells are eligible for installation and which are eligible for decommissioning.

## While it runs

* **Pause after this run**, **Resume**, **Stop & restore**.
* **Add iterations** extends a session that is going well.
* **Re-score** applies the current objective table to every run so far, also while the session is running: edited weights and directions, deleted objectives (no longer scored), and added ones, which are computed for each finished run from its saved Ecospace output. The evolutionary search and later runs then use the new objectives.
* The score plots show the status quo level (0) as a gray dashed line.
* The composite score is the weighted sum of each objective's relative change from the status quo (`weight × (value − status quo) / status quo`, with the sign flipped for `min` objectives). Each objective adds at most ±100% × its weight, so one whose status quo is near zero can't swamp the others. The `_vs_status_quo_pct` columns keep the full change. The status quo scores 0. A run is on the *cost frontier* when no other run scores at least as high for the same or lower cost.

## Building the download

`build_package.R` assembles `OptimalEcospace-LiveSolver.zip` with only the files a run needs:

* the app and launcher (`Start Live Solver.R`, `R/LiveSolver/*.R`). The download contains no programs or batch files, so browsers have nothing to flag.
* for each region in `Inputs/`, the model named by `<EWE_MODEL_FILE>` in its command-file template
* the region's STConfig file, command-file template, and the rasters the solver reads or rewrites: every habitat layer in the STConfig, `basemap.asc`, `exclusions.asc`, `cost_map.asc`, and the pristine copies of the habitat layers in `Inputs/<region>/Initial Base Mapps/` when that folder exists

Sediment source data (`Inputs/dbSeaBed`), model backups, logs, `Backup_Base_Maps` and other working files are left out. Any new region or habitat layer added to an STConfig is picked up automatically.

The *Live Solver package* GitHub Actions workflow builds the zip on every push that touches the app, `Inputs/` or `Model/`. It then unzips the package into an empty folder on a Windows runner and runs a test-mode session there (`check_package.R`), to prove the package works on its own. On `main` it publishes the zip to the `live-solver` release, so the download link above always serves the current version. To build it locally, run `Rscript R/LiveSolver/build_package.R` from the repository root; the zip goes to `dist/` (on Windows it uses PowerShell if no zip program is installed).

## Comparing scenarios

The **Results** tab shows, for the top scenarios, the percent change of every outcome compared with the no-change baseline. Blue bars are improvements in the direction you chose for that outcome; orange bars are worse. Click any scenario in the table to see its map and a table of its outcomes. The same percent changes are saved as `<objective>_vs_status_quo_pct` columns in `optimization_results.csv`.

## Outputs

Each session writes to `Outputs/<region>/LiveSolver_<timestamp>/`:

* `optimization_results.csv`: one row per run, with the drawn budget, modules and reef cells, average cost per module, modules per cell, contiguity, spatial footprint, removals, costs, every objective and its `_vs_status_quo_pct`, net pounds gained, pounds per module and per dollar, revenue gain and payback, composite score, Pareto and cost-frontier flags
* `Run_XXXX/`: `Maps/` (every habitat layer used), `candidate.json` (cell indices), `cmd.txt`, console logs and the Ecospace CSVs
* `Best_Maps/`: the best run's layers
* `solver_config.json`: the exact configuration. Load it in the app or pass it to `run_headless.R` to reproduce the session.
* `solver_log.txt`

## How maps reach Ecospace

Ecospace reads one fixed STConfig file and the map files it lists, so the solver works the same way `structure_optimization.R` does. Every scenario starts from the pristine maps (`Inputs/<region>/Initial Base Mapps/` when present; put your original habitat layers there, under the same file names). Before every run the solver writes the scenario's layers straight over the map files in `Inputs/<region>/` and writes a command file that differs from the template only in the output folder and the model and STConfig paths. A copy of each run's layers is also kept in its `Maps/` folder.

* **Existing reefs.** As in `structure_optimization.R`, when `Inputs/reeflocations.csv` (the FWC artificial reef deployments) is present, the existing reef cover is built from it. Each reef covers a halo of the installation panel's halo radius (default 500 m), and a cell's cover is the share of the cell inside those halos. The Alabama reef zone has no point data. Its reef cells (where `Initial Base Mapps/structures_init.asc` has reef, in the first 19 columns) get the mean cover of the Florida reef cells. Land and excluded cells are set to 0. Choose *From the structure map* under **Model & run → Existing reefs** to use `structures_all.asc` as it is.
* **Map coordinates.** Ecospace places each map on its grid by the coordinates in the file's header. The NGoM model grid's top-left corner is at 31.4647° N, 88.2306° W (bottom edge `yllcorner` 30.2535), so every map the solver writes into Inputs keeps the header of `Inputs/<region>/basemap.asc`, as `structure_optimization.R` does. Some maps carry other coordinates; `Initial Base Mapps` uses 29.28, the true latitude, which is also used to place the reef CSV and to draw the app's maps. They are matched by row and column, never resampled. After the first run, the solver compares the written header with the grid in Ecospace's spatial log, and warns if they don't line up.
* The region's own STConfig (e.g. `Inputs/NGoM/NGoMSTConfig.xml`) is used, with its file paths repaired in place to point at this folder's `Inputs` maps. This matters because the stored paths can come from another computer.
* Before the first run, the original maps and STConfig are snapshotted. They are restored when the session finishes, is stopped, or the browser tab closes. If R crashes mid-session, a `.livesolver_lock` file stays in the Inputs folder, and the next session (app or headless) restores the snapshot automatically before it starts.
* **Ecospace may read a different set of map files.** The console can follow the spatial configuration saved inside the model rather than the one it is given (for example, your original working folder). After the first run, the solver reads Ecospace's spatial log, which lists exactly which map files were loaded. If they are elsewhere, it switches to updating those files (snapshotting them first and restoring them afterwards), tells you where they are, and repeats the status quo run with its own maps. The first run is kept as a `probe` and is not scored.
* **Ecospace's spatial data cache.** Ecospace keeps converted copies of every external habitat layer in `%APPDATA%\<console name>\Cache\Spatial\<dataset ID>\`. The cache is keyed only by the layer's dataset ID (GUID in the STConfig), extent, cell size and date, so a rewritten map file never refreshes it on its own. Before every run, the solver finds that folder itself and deletes the entries for this model's layers, so Ecospace reads the maps again. Nothing needs to be set up. The spatial log says "Loaded from cache" even then, because Ecospace reads the maps through the cache it has just filled. If an entry can't be deleted (for example because Ecospace is open in another program), the run is not scored. If that happens on the status quo run, the session stops and says so.
* Each copy is verified. If a map can't be overwritten (for example because it is open in another program), the session stops with a message.
* After each run, the solver keeps Ecospace's spatial log (`<model>_spatiallog.txt`) in the run folder. It warns in the log and in the app if Ecospace did not load the overwritten structure map, or if a scenario that changed structures produced outcomes identical to the status quo.

The engine (`solver_engine.R`) has no Shiny dependency, so other scripts can reuse it (`ls_build_problem`, `ls_generate_candidate`, `ls_apply_candidate`, `ls_evaluate_objectives`, `ls_run_session`, …).
