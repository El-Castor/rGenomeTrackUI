#!/usr/bin/env Rscript
# =============================================================================
# rGenomeTrackUI — R package installer
# =============================================================================
# Version      : 1.0.0
# Last updated : 2026-05-12
#
# Description:
#   Installs all R packages required by rGenomeTrackUI that are not available
#   via conda, notably rGenomeTracks (Bioconductor) and any CRAN packages
#   not present in the active conda environment.
#
#   This script MUST be run with the conda R, NOT the system R.
#   On macOS, /usr/local/bin/Rscript may shadow the conda Rscript.
#   Use the full path:
#     $(conda info --base)/envs/rgenometrackui/bin/Rscript install.R
#
#   It will:
#     1. Detect the active R environment (should be conda rgenometrackui)
#     2. Install BiocManager if missing
#     3. Install rGenomeTracks from Bioconductor
#     4. Install additional CRAN packages not in conda
#     5. Verify all packages load correctly
#     6. Write an installation report
# =============================================================================

cat("=============================================================================\n")
cat("rGenomeTrackUI — R Package Installer\n")
cat("=============================================================================\n\n")

# =============================================================================
# 1. Verify the active R environment
# =============================================================================

cat("--- Step 1: Verifying R environment ---\n")

r_home  <- Sys.getenv("R_HOME", unset = R.home())
r_lib   <- .libPaths()[1]
conda_env <- Sys.getenv("CONDA_DEFAULT_ENV", unset = "<not set>")
conda_prefix <- Sys.getenv("CONDA_PREFIX", unset = "<not set>")

cat(sprintf("  R version     : %s\n", R.version.string))
cat(sprintf("  R home        : %s\n", r_home))
cat(sprintf("  R library     : %s\n", r_lib))
cat(sprintf("  Conda env     : %s\n", conda_env))
cat(sprintf("  Conda prefix  : %s\n", conda_prefix))

# Safety check: warn if not running in the expected conda environment
if (!grepl("rgenometrackui", conda_env, ignore.case = TRUE)) {
  warning(paste0(
    "\n[WARNING] The active Conda environment is '", conda_env, "'.\n",
    "  Expected: 'rgenometrackui'.\n",
    "  Packages will be installed in: ", r_lib, "\n",
    "  To ensure isolation, run:\n",
    "    conda activate rgenometrackui && Rscript install.R\n"
  ))
}
cat("\n")

# =============================================================================
# 2. Install BiocManager (required to install rGenomeTracks)
# =============================================================================

cat("--- Step 2: Checking BiocManager ---\n")

install_if_missing <- function(pkg, repo = "CRAN", bioc = FALSE, github = NULL) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat(sprintf("  [INSTALLING] %s ...\n", pkg))
    tryCatch({
      if (!is.null(github)) {
        remotes::install_github(github, quiet = TRUE)
      } else if (bioc) {
        BiocManager::install(pkg, ask = FALSE, update = FALSE, quiet = TRUE)
      } else {
        install.packages(pkg, repos = "https://cloud.r-project.org", quiet = TRUE)
      }
      if (requireNamespace(pkg, quietly = TRUE)) {
        cat(sprintf("  [OK] %s installed successfully.\n", pkg))
      } else {
        stop(sprintf("Installation of '%s' failed silently.", pkg))
      }
    }, error = function(e) {
      cat(sprintf("  [ERROR] Failed to install %s: %s\n", pkg, conditionMessage(e)))
    })
  } else {
    cat(sprintf("  [OK] %s is already installed.\n", pkg))
  }
}

install_if_missing("BiocManager")
cat("\n")

# =============================================================================
# 3. Install rGenomeTracks from Bioconductor
# =============================================================================

cat("--- Step 3: Checking rGenomeTracks (Bioconductor) ---\n")

if (!requireNamespace("rGenomeTracks", quietly = TRUE)) {
  cat("  [INSTALLING] rGenomeTracks from Bioconductor (with all dependencies) ...\n")
  tryCatch({
    # Install with Depends + Imports only (NOT Suggests — avoids imager/X11 issue)
    BiocManager::install("rGenomeTracks", ask = FALSE, update = FALSE,
                         dependencies = c("Depends", "Imports"))
    if (requireNamespace("rGenomeTracks", quietly = TRUE)) {
      cat("  [OK] rGenomeTracks installed successfully.\n")
    } else {
      cat("  [ERROR] rGenomeTracks installation failed silently.\n")
    }
  }, error = function(e) {
    cat(sprintf("  [ERROR] Failed to install rGenomeTracks: %s\n", conditionMessage(e)))
    cat("  Hint: ensure BiocManager is installed and Bioconductor is reachable.\n")
    cat("  If 'imager' fails: run  conda install -n rgenometrackui -c conda-forge r-imager\n")
  })
} else {
  cat("  [OK] rGenomeTracks is already installed.\n")
}
cat("\n")

# =============================================================================
# 4. Install rGenomeTracksData (optional example data package, Bioconductor)
# =============================================================================

cat("--- Step 4: Checking rGenomeTracksData (optional, Bioconductor) ---\n")
install_if_missing("rGenomeTracksData", bioc = TRUE)
cat("\n")

# =============================================================================
# 5. Install CRAN packages not typically bundled in conda r-essentials
# =============================================================================

cat("--- Step 5: Checking additional CRAN packages ---\n")

cran_packages <- c(
  "shiny",          # Shiny web framework
  "bslib",          # Bootstrap theming
  "shinyjs",        # JS utilities for Shiny
  "colourpicker",   # Color inputs for Shiny
  "shinyFiles",     # File browser for Shiny
  "DT",             # Interactive tables
  "jsonlite",       # JSON I/O
  "yaml",           # YAML I/O
  "fs",             # File system utilities
  "processx",       # External process execution
  "callr",          # Call R from R
  "reticulate",     # Python-R interface
  "testthat",       # Unit testing
  "devtools",       # Package development
  "remotes",        # Install from GitHub/Bioconductor
  "optparse",       # CLI argument parsing
  "stringr",        # String utilities
  "glue",           # String interpolation
  "purrr",          # Functional programming
  "dplyr",          # Data manipulation
  "lubridate",      # Date/time handling
  "digest",         # Hashing
  "zip",            # ZIP archive creation
  "sortable",       # Drag-and-drop UI (Shiny)
  "waiter",         # Loading screens (Shiny)
  "shinyFeedback",  # Input feedback (Shiny)
  "shinyWidgets"    # Extended Shiny widgets
)

for (pkg in cran_packages) {
  install_if_missing(pkg)
}
cat("\n")

# =============================================================================
# 6. Verify that all critical packages load correctly
# =============================================================================

cat("--- Step 6: Verifying critical packages ---\n")

critical_packages <- c(
  "shiny", "bslib", "shinyjs", "DT",
  "jsonlite", "yaml", "fs",
  "processx", "callr", "reticulate",
  "testthat", "BiocManager"
)

all_ok <- TRUE
for (pkg in critical_packages) {
  ok <- tryCatch({
    library(pkg, character.only = TRUE, quietly = TRUE)
    TRUE
  }, error = function(e) FALSE)
  status <- if (ok) "[OK]    " else "[FAIL]  "
  if (!ok) all_ok <- FALSE
  cat(sprintf("  %s %s\n", status, pkg))
}

# rGenomeTracks is critical but may fail if pyGenomeTracks is not yet configured
rgt_ok <- tryCatch({
  library(rGenomeTracks, quietly = TRUE)
  TRUE
}, error = function(e) FALSE)
cat(sprintf("  %s rGenomeTracks\n", if (rgt_ok) "[OK]    " else "[WARN]  "))
if (!rgt_ok) {
  cat("  Hint: rGenomeTracks may require pyGenomeTracks to be installed.\n")
  cat("  Run install_pyGenomeTracks() from within R if needed.\n")
}
cat("\n")

# =============================================================================
# 7. Write installation report
# =============================================================================

cat("--- Step 7: Writing installation report ---\n")

report_dir <- if (dir.exists("logs")) "logs" else "."
report_file <- file.path(report_dir, "install_r_report.txt")

tryCatch({
  report_lines <- c(
    "=============================================================================",
    "rGenomeTrackUI — R Installation Report",
    paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    "=============================================================================",
    "",
    paste0("R version     : ", R.version.string),
    paste0("R home        : ", r_home),
    paste0("R library     : ", r_lib),
    paste0("Conda env     : ", conda_env),
    paste0("Conda prefix  : ", conda_prefix),
    "",
    "--- Installed packages ---"
  )

  # List all installed packages
  installed <- installed.packages()[, c("Package", "Version")]
  pkg_lines <- apply(installed, 1, function(row) {
    sprintf("  %-40s %s", row["Package"], row["Version"])
  })
  report_lines <- c(report_lines, pkg_lines, "", "--- sessionInfo ---", "")

  writeLines(report_lines, report_file)

  # Append sessionInfo
  sink(report_file, append = TRUE)
  sessionInfo()
  sink()

  cat(sprintf("  [OK] Report written to: %s\n", report_file))
}, error = function(e) {
  cat(sprintf("  [WARN] Could not write report: %s\n", conditionMessage(e)))
})
cat("\n")

# =============================================================================
# Summary
# =============================================================================

cat("=============================================================================\n")
if (all_ok && rgt_ok) {
  cat("STATUS: OK — All critical R packages are installed and loadable.\n")
} else if (all_ok && !rgt_ok) {
  cat("STATUS: WARNING — Core packages OK, but rGenomeTracks failed to load.\n")
  cat("  Action: Check pyGenomeTracks installation and run install_pyGenomeTracks()\n")
} else {
  cat("STATUS: ERROR — Some critical packages failed. Check output above.\n")
}
cat("=============================================================================\n")
