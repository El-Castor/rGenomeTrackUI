#!/usr/bin/env Rscript
# =============================================================================
# rGenomeTrackUI — R dependency checker
# =============================================================================
# Version      : 1.0.0
# Last updated : 2026-05-12
#
# Description:
#   Verifies that all R packages required by rGenomeTrackUI are installed
#   and loadable in the current R environment.
#   Also checks rGenomeTracks availability and reports sessionInfo.
#
# Usage:
#   conda activate rgenometrackui
#   Rscript scripts/check_r_dependencies.R
#
# Output:
#   - Prints a validation report to stdout
#   - Writes the report to logs/dependency_check_r.txt
#     (or ./dependency_check_r.txt if logs/ does not exist)
# =============================================================================

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

status_ok   <- function(msg) cat(sprintf("  [OK]    %s\n", msg))
status_warn <- function(msg) cat(sprintf("  [WARN]  %s\n", msg))
status_fail <- function(msg) cat(sprintf("  [FAIL]  %s\n", msg))
status_info <- function(msg) cat(sprintf("  [INFO]  %s\n", msg))

check_pkg <- function(pkg, required = TRUE) {
  available <- requireNamespace(pkg, quietly = TRUE)
  if (available) {
    ver <- tryCatch(
      as.character(packageVersion(pkg)),
      error = function(e) "unknown"
    )
    status_ok(sprintf("%-30s version: %s", pkg, ver))
    return(list(pkg = pkg, ok = TRUE, version = ver))
  } else {
    if (required) {
      status_fail(sprintf("%-30s NOT INSTALLED", pkg))
    } else {
      status_warn(sprintf("%-30s not installed (optional)", pkg))
    }
    return(list(pkg = pkg, ok = FALSE, version = NA))
  }
}

# ---------------------------------------------------------------------------
# Header
# ---------------------------------------------------------------------------

cat("\n")
cat("=============================================================================\n")
cat("rGenomeTrackUI — R Dependency Check\n")
cat(sprintf("Timestamp: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
cat("=============================================================================\n\n")

# ---------------------------------------------------------------------------
# 1. R version and environment info
# ---------------------------------------------------------------------------

cat("--- 1. R Environment ---\n")
status_info(sprintf("R version     : %s", R.version.string))
status_info(sprintf("R home        : %s", R.home()))
status_info(sprintf("Library path  : %s", paste(.libPaths(), collapse = " | ")))
status_info(sprintf("Conda env     : %s", Sys.getenv("CONDA_DEFAULT_ENV", unset = "<not set>")))
status_info(sprintf("Conda prefix  : %s", Sys.getenv("CONDA_PREFIX", unset = "<not set>")))

# Check R version is >= 4.2
r_ver <- numeric_version(paste(R.version$major, R.version$minor, sep = "."))
if (r_ver >= "4.2.0") {
  status_ok(sprintf("R >= 4.2.0 (%s)", R.version.string))
} else {
  status_warn(sprintf("R version %s is below recommended 4.2.0", R.version.string))
}
cat("\n")

# ---------------------------------------------------------------------------
# 2. Core Shiny application packages
# ---------------------------------------------------------------------------

cat("--- 2. Core Shiny packages ---\n")
results <- list()

shiny_pkgs <- c("shiny", "bslib", "shinyjs", "shinyFiles", "DT",
                "sortable", "waiter", "shinyWidgets", "shinyFeedback")
for (pkg in shiny_pkgs) {
  results[[pkg]] <- check_pkg(pkg, required = TRUE)
}
cat("\n")

# ---------------------------------------------------------------------------
# 3. Data I/O packages
# ---------------------------------------------------------------------------

cat("--- 3. Data I/O packages ---\n")
io_pkgs <- c("jsonlite", "yaml", "fs", "readr", "zip")
for (pkg in io_pkgs) {
  results[[pkg]] <- check_pkg(pkg, required = TRUE)
}
cat("\n")

# ---------------------------------------------------------------------------
# 4. Process execution packages
# ---------------------------------------------------------------------------

cat("--- 4. Process execution packages ---\n")
proc_pkgs <- c("processx", "callr")
for (pkg in proc_pkgs) {
  results[[pkg]] <- check_pkg(pkg, required = TRUE)
}
cat("\n")

# ---------------------------------------------------------------------------
# 5. Python interop (required by rGenomeTracks)
# ---------------------------------------------------------------------------

cat("--- 5. Python interop (reticulate) ---\n")
results[["reticulate"]] <- check_pkg("reticulate", required = TRUE)

# Test Python availability via reticulate
if (results[["reticulate"]]$ok) {
  python_ok <- tryCatch({
    library(reticulate, quietly = TRUE)
    py_available(initialize = FALSE)
  }, error = function(e) FALSE)

  if (python_ok) {
    py_ver <- tryCatch(py_version(), error = function(e) "unknown")
    py_path <- tryCatch(
      reticulate::py_config()$python,
      error = function(e) "<unknown>"
    )
    status_ok(sprintf("Python available via reticulate: %s (%s)", py_ver, py_path))
  } else {
    status_warn("Python not yet configured for reticulate.")
    status_warn("Ensure the conda env is activated or call reticulate::use_condaenv('rgenometrackui')")
  }
}
cat("\n")

# ---------------------------------------------------------------------------
# 6. Development and testing packages
# ---------------------------------------------------------------------------

cat("--- 6. Development and testing packages ---\n")
dev_pkgs <- c("testthat", "devtools", "remotes", "optparse", "BiocManager")
for (pkg in dev_pkgs) {
  results[[pkg]] <- check_pkg(pkg, required = TRUE)
}
cat("\n")

# ---------------------------------------------------------------------------
# 7. Utility packages
# ---------------------------------------------------------------------------

cat("--- 7. Utility packages ---\n")
util_pkgs <- c("stringr", "glue", "purrr", "dplyr", "lubridate", "digest")
for (pkg in util_pkgs) {
  results[[pkg]] <- check_pkg(pkg, required = FALSE)
}
cat("\n")

# ---------------------------------------------------------------------------
# 8. rGenomeTracks (Bioconductor — CRITICAL)
# ---------------------------------------------------------------------------

cat("--- 8. rGenomeTracks (Bioconductor) ---\n")
rgt_result <- check_pkg("rGenomeTracks", required = TRUE)
results[["rGenomeTracks"]] <- rgt_result

if (rgt_result$ok) {
  # Try to load it
  load_ok <- tryCatch({
    library(rGenomeTracks, quietly = TRUE)
    TRUE
  }, error = function(e) {
    status_warn(sprintf("rGenomeTracks installed but failed to load: %s", conditionMessage(e)))
    FALSE
  })
  if (load_ok) {
    status_ok("rGenomeTracks loaded successfully.")
  }
} else {
  status_fail("rGenomeTracks is NOT installed.")
  cat("  Hint: Run install.R or manually:\n")
  cat("    BiocManager::install('rGenomeTracks')\n")
}
cat("\n")

# ---------------------------------------------------------------------------
# 9. Optional: rGenomeTracksData
# ---------------------------------------------------------------------------

cat("--- 9. rGenomeTracksData (optional example data, Bioconductor) ---\n")
results[["rGenomeTracksData"]] <- check_pkg("rGenomeTracksData", required = FALSE)
cat("\n")

# ---------------------------------------------------------------------------
# 10. Session info
# ---------------------------------------------------------------------------

cat("--- 10. Session Information ---\n")
session <- sessionInfo()
cat(sprintf("  Platform  : %s\n", session$platform))
cat(sprintf("  OS        : %s\n", session$running))
cat(sprintf("  Locale    : %s\n", session$locale))
cat("\n")

# ---------------------------------------------------------------------------
# 11. Summary
# ---------------------------------------------------------------------------

all_results  <- do.call(rbind, lapply(results, function(x) {
  data.frame(pkg = x$pkg, ok = x$ok, version = ifelse(is.na(x$version), "", x$version),
             stringsAsFactors = FALSE)
}))

n_ok   <- sum(all_results$ok)
n_fail <- sum(!all_results$ok)

# Distinguish required failures from optional
required_pkgs <- c(
  "shiny", "bslib", "shinyjs", "colourpicker", "shinyFiles", "DT",
  "jsonlite", "yaml", "fs", "processx", "callr",
  "reticulate", "testthat", "BiocManager", "rGenomeTracks",
  "devtools", "remotes"
)
n_critical_fail <- sum(!all_results$ok & all_results$pkg %in% required_pkgs)

cat("=============================================================================\n")
cat(sprintf("  Packages checked : %d\n", nrow(all_results)))
cat(sprintf("  OK               : %d\n", n_ok))
cat(sprintf("  Failed           : %d\n", n_fail))
cat(sprintf("  Critical failures: %d\n", n_critical_fail))
cat("\n")

if (n_critical_fail == 0 && n_fail == 0) {
  cat("  STATUS: OK — All R dependencies are satisfied.\n")
} else if (n_critical_fail == 0) {
  cat("  STATUS: WARNING — Some optional packages missing. Core OK.\n")
} else {
  cat("  STATUS: FAIL — Critical packages missing. Run install.R.\n")
}
cat("=============================================================================\n\n")

# ---------------------------------------------------------------------------
# 12. Write report to file
# ---------------------------------------------------------------------------

report_dir <- if (dir.exists("logs")) "logs" else "."
report_file <- file.path(report_dir, "dependency_check_r.txt")

tryCatch({
  sink(report_file)
  cat("=============================================================================\n")
  cat("rGenomeTrackUI — R Dependency Check Report\n")
  cat(sprintf("Generated: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
  cat("=============================================================================\n\n")
  cat(sprintf("R version    : %s\n", R.version.string))
  cat(sprintf("Conda env    : %s\n", Sys.getenv("CONDA_DEFAULT_ENV", "<not set>")))
  cat(sprintf("Conda prefix : %s\n\n", Sys.getenv("CONDA_PREFIX", "<not set>")))

  cat("--- Package status ---\n")
  for (r in lapply(results, function(x) x)) {
    status <- if (r$ok) "OK  " else "FAIL"
    ver    <- if (!is.na(r$version)) r$version else "N/A"
    cat(sprintf("  [%s] %-30s %s\n", status, r$pkg, ver))
  }

  cat("\n--- sessionInfo ---\n")
  print(sessionInfo())
  sink()
  cat(sprintf("  Report written to: %s\n", report_file))
}, error = function(e) {
  cat(sprintf("  Warning: could not write report: %s\n", conditionMessage(e)))
})

# Exit with appropriate code
quit(status = if (n_critical_fail > 0) 1L else 0L)
