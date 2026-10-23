#####################################################
### Live Solver: headless runner                  ###
###                                               ###
### Purpose: Run a configuration saved from the   ###
###  Live Solver app (or written by hand) without ###
###  the UI, e.g. overnight or on a server.       ###
###                                               ###
### Usage (from R/LiveSolver):                    ###
###   Rscript run_headless.R my_config.json       ###
### or in R:                                      ###
###   source("run_headless.R")                    ###
#####################################################

source("solver_engine.R")

repo_root <- normalizePath(Sys.getenv("OPTIMAL_ECOSPACE_ROOT", "../.."), winslash = "/", mustWork = TRUE)

args     <- commandArgs(trailingOnly = TRUE)
cfg_file <- if (length(args)) args[1] else ""

cfg <- if (nzchar(cfg_file)) ls_load_config(cfg_file, repo_root) else ls_default_config(repo_root)

S <- ls_run_session(cfg, repo_root)

if (!is.null(S$results)) {
  top <- S$results[order(-S$results$Composite_Score), ]
  print(head(top[, c("run", "kind", "n_installed", "n_decommissioned", "total_cost", "Composite_Score", "Pareto")], 10))
}
message("Results: ", S$results_file)
