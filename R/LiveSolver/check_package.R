#####################################################
### Live Solver: package smoke test               ###
###                                               ###
### Purpose: Run a short test-mode session from   ###
###  an unpacked download to prove it contains    ###
###  everything the Live Solver needs.            ###
###                                               ###
### Usage:                                        ###
###   Rscript check_package.R <unpacked folder>   ###
#####################################################

args <- commandArgs(trailingOnly = TRUE)
pkg_root <- normalizePath(args[1], winslash = "/", mustWork = TRUE)
setwd(file.path(pkg_root, "R/LiveSolver"))
source("solver_engine.R")

cfg <- ls_default_config(pkg_root)
stopifnot(nrow(ls_discover_models(pkg_root)) >= 1)
cfg$search$iterations <- 5
cfg$search$dry_run <- TRUE
S <- ls_run_session(cfg, pkg_root)
stopifnot(nrow(S$results) == 7, all(S$results$finished))

invisible(shiny::shinyAppDir("."))
cat("Package runs on its own:", nrow(S$results), "test-mode runs finished\n")
