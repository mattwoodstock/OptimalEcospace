#####################################################
### Live Solver: build the download package       ###
###                                               ###
### Purpose: Assemble a zip with only the files   ###
###  the Live Solver needs to run: the app, the   ###
###  launcher, each region's model file and the   ###
###  rasters/configs used to rebuild habitats.    ###
###  Sediment source data, model backups and      ###
###  other working files are left out.            ###
###                                               ###
### Usage (from the repository root):             ###
###   Rscript R/LiveSolver/build_package.R [out]  ###
### Writes <out>/OptimalEcospace-LiveSolver.zip   ###
### (default out = "dist").                       ###
#####################################################

args <- commandArgs(trailingOnly = TRUE)
repo_root <- normalizePath(".", winslash = "/")
if (!file.exists(file.path(repo_root, "R/LiveSolver/solver_engine.R"))) stop("Run this from the repository root.")
out_dir  <- if (length(args)) args[1] else "dist"
pkg_name <- "OptimalEcospace-LiveSolver"

source(file.path(repo_root, "R/LiveSolver/solver_engine.R"))

# App and launcher ----
files <- c("Start Live Solver.R",
           file.path("R/LiveSolver", c("app.R", "solver_engine.R", "launch.R", "run_headless.R", "README.md")))

# Per region: the model named in the command template + every map/config a run reads ----
defaults <- ls_default_config(repo_root)
for (reg in list.dirs(file.path(repo_root, "Inputs"), full.names = FALSE, recursive = FALSE)) {
  inputs_dir <- file.path(repo_root, "Inputs", reg)
  info <- ls_region_info(inputs_dir)
  if (is.na(info$cmd_template) || is.na(info$stconfig)) next

  tmpl  <- readLines(info$cmd_template, warn = FALSE)
  mline <- grep("^\\s*<EWE_MODEL_FILE>\\s*,", tmpl, value = TRUE)
  model <- if (length(mline)) basename(gsub("\\\\", "/", trimws(strsplit(mline[1], ",")[[1]][2]))) else NA
  model_rel <- file.path("Model", reg, model)
  if (is.na(model) || !file.exists(file.path(repo_root, model_rel))) {
    message("Skipping region ", reg, ": model named in the command template was not found (", model_rel, ")")
    next
  }

  rel <- function(f) sub(paste0("^", repo_root, "/"), "", normalizePath(f, winslash = "/"))
  maps <- c(info$layers$file[info$layers$exists],
            file.path(inputs_dir, c(defaults$layers$basemap_file,
                                    defaults$install$exclusions_file,
                                    defaults$decom$cost_map_file)))
  # Pristine copies of the habitat layers ("Initial Base Mapps"), read before every scenario
  pdir <- ls_pristine_dir(inputs_dir, "auto")
  if (!is.null(pdir)) maps <- c(maps, file.path(pdir, c(basename(info$layers$file), defaults$layers$basemap_file,
                                                        defaults$install$exclusions_file,
                                                        defaults$layers$baseline_fill_map)))
  maps <- maps[file.exists(maps)]
  files <- c(files, model_rel, rel(info$stconfig), rel(info$cmd_template), vapply(maps, rel, ""))
}
# Reef locations used to build the existing reef baseline (Inputs/ or Inputs/<region>/)
files <- c(files, sub(paste0("^", repo_root, "/"), "",
                      list.files(file.path(repo_root, "Inputs"), "^reeflocations\\.csv$", recursive = TRUE, full.names = TRUE)))
files <- unique(files)

# Assemble and zip ----
stage <- file.path(tempfile("lspkg"), pkg_name)
for (f in files) {
  dir.create(dirname(file.path(stage, f)), recursive = TRUE, showWarnings = FALSE)
  if (!file.copy(file.path(repo_root, f), file.path(stage, f))) stop("Could not copy ", f)
}

# Version stamp shown in the app and logs, so a downloaded copy can be identified
sha <- Sys.getenv("GITHUB_SHA")
if (!nzchar(sha)) sha <- tryCatch(system2("git", c("rev-parse", "HEAD"), stdout = TRUE), error = function(e) "unknown")
writeLines(sprintf("%s (built %s)", substr(sha[1], 1, 7), format(Sys.time(), "%Y-%m-%d %H:%M UTC", tz = "UTC")),
           file.path(stage, "R/LiveSolver/VERSION"))

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
zip_path <- file.path(normalizePath(out_dir, winslash = "/"), paste0(pkg_name, ".zip"))
unlink(zip_path)
if (nzchar(Sys.which("zip"))) {
  old <- setwd(dirname(stage))
  status <- utils::zip(zip_path, pkg_name, flags = "-r9Xq")
  setwd(old)
} else if (.Platform$OS.type == "windows") {
  # Windows without a zip program: PowerShell keeps the top-level package folder
  status <- system2("powershell", c("-NoProfile", "-Command",
    shQuote(sprintf("Compress-Archive -Path '%s' -DestinationPath '%s' -Force",
                    normalizePath(stage), normalizePath(zip_path, mustWork = FALSE)))))
} else {
  stop("No zip program found.")
}
if (status != 0 || !file.exists(zip_path)) stop("zip failed (status ", status, ")")

sizes <- file.size(file.path(repo_root, files))
message(paste(sprintf("%-60s %8.1f KB", files, sizes / 1024), collapse = "\n"))
message(sprintf("\n%d files, %.1f MB unpacked -> %s (%.1f MB)",
                length(files), sum(sizes) / 2^20, zip_path, file.size(zip_path) / 2^20))
