#####################################################
### Start the OptimalEcospace Live Solver         ###
###                                               ###
### Open this file in RStudio and click "Source"  ###
### (top right of this editor pane). The app      ###
### opens in your web browser. The first start    ###
### installs a few R packages, which takes a few  ###
### minutes. Keep RStudio open while you use it.  ###
#####################################################

local({
  here <- NULL
  for (i in rev(seq_len(sys.nframe()))) {
    f <- sys.frame(i)$ofile
    if (!is.null(f)) { here <- dirname(normalizePath(f)); break }
  }
  if (is.null(here) && requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    p <- rstudioapi::getSourceEditorContext()$path
    if (nzchar(p)) here <- dirname(normalizePath(p))
  }
  if (is.null(here)) here <- getwd()
  launcher <- file.path(here, "R", "LiveSolver", "launch.R")
  if (!file.exists(launcher)) stop("Cannot find R/LiveSolver/launch.R next to this file. Unzip the whole download and open this file from there.")
  source(launcher)
})
