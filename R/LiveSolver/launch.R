#####################################################
### Live Solver launcher                          ###
###                                               ###
### Purpose: One-step start for people who are    ###
###  new to R. Installs any missing packages the  ###
###  first time, then opens the Live Solver in    ###
###  the web browser.                             ###
###                                               ###
### Use one of:                                   ###
###  - RStudio: open "Start Live Solver.R"        ###
###    (repository root) and press "Source"       ###
###  - RStudio: open this file and press "Source" ###
###  - R console: source("R/LiveSolver/launch.R") ###
#####################################################

# Locate this file whether it is run with Rscript, source() or RStudio's Source button
fn.this_dir <- function() {
  arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(arg)) return(dirname(normalizePath(sub("^--file=", "", arg[1]))))
  for (i in rev(seq_len(sys.nframe()))) {
    f <- sys.frame(i)$ofile
    if (!is.null(f)) return(dirname(normalizePath(f)))
  }
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    p <- rstudioapi::getSourceEditorContext()$path
    if (nzchar(p)) return(dirname(normalizePath(p)))
  }
  if (file.exists("app.R")) return(normalizePath("."))
  if (file.exists("R/LiveSolver/app.R")) return(normalizePath("R/LiveSolver"))
  stop("Cannot find the Live Solver folder. Open launch.R from R/LiveSolver and run it again.")
}

app_dir   <- fn.this_dir()
repo_root <- normalizePath(file.path(app_dir, "..", ".."), winslash = "/")

# Packages ----
needed  <- c("shiny", "bslib", "DT", "processx", "terra", "xml2", "jsonlite")
missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  message("First start: installing ", paste(missing, collapse = ", "), " (this can take a few minutes)...")
  lib <- Sys.getenv("R_LIBS_USER")
  if (nzchar(lib) && !dir.exists(lib)) dir.create(lib, recursive = TRUE)
  if (nzchar(lib)) .libPaths(c(lib, .libPaths()))
  install.packages(missing, repos = "https://cloud.r-project.org",
                   type = if (.Platform$OS.type == "windows") "binary" else getOption("pkgType"))
  still <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
  if (length(still)) stop("Could not install: ", paste(still, collapse = ", "), ". Install them manually and try again.")
}

# Launch ----
Sys.setenv(OPTIMAL_ECOSPACE_ROOT = repo_root)
message("Starting the Live Solver. It opens in your web browser; keep this window open while you use it.")
shiny::runApp(app_dir, launch.browser = TRUE)
