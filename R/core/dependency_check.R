# =============================================================================
# dependency_check.R — Runtime dependency verification
# =============================================================================

#' Check if an R package is available
#'
#' @param pkg package name
#' @return logical
#' @export
check_r_package <- function(pkg) {
  requireNamespace(pkg, quietly = TRUE)
}

#' Check if a system command is available in PATH
#'
#' @param cmd command name (e.g. "pyGenomeTracks", "bedtools")
#' @return logical
#' @export
check_command_available <- function(cmd) {
  result <- tryCatch(
    system2("which", args = cmd, stdout = TRUE, stderr = FALSE),
    error = function(e) NULL
  )
  !is.null(result) && length(result) > 0 && nchar(trimws(result[1])) > 0
}

#' Get the filesystem path of a command
#'
#' @param cmd command name
#' @return path string or NA
#' @export
get_command_path <- function(cmd) {
  result <- tryCatch(
    system2("which", args = cmd, stdout = TRUE, stderr = FALSE),
    error = function(e) character(0)
  )
  if (length(result) > 0) trimws(result[1]) else NA_character_
}

#' Get pyGenomeTracks version string
#'
#' @return version string or NA
#' @export
get_pygenometracks_version <- function() {
  result <- tryCatch(
    system2("pyGenomeTracks", args = "--version", stdout = TRUE, stderr = TRUE),
    error = function(e) character(0)
  )
  if (length(result) > 0) trimws(paste(result, collapse = " ")) else NA_character_
}

#' Check that rGenomeTracks is loadable
#'
#' @return logical
#' @export
check_rgenometracks <- function() {
  check_r_package("rGenomeTracks")
}

#' Check that BEDTools is in PATH
#'
#' @return logical
#' @export
check_bedtools <- function() {
  check_command_available("bedtools")
}

#' Check the active Conda environment
#'
#' @param expected_env expected environment name
#' @return logical TRUE if the expected env appears to be active
#' @export
check_conda_env <- function(expected_env = "rgenometrackui") {
  active <- Sys.getenv("CONDA_DEFAULT_ENV", unset = "")
  grepl(expected_env, active, ignore.case = TRUE)
}

#' Run a full dependency check and return a structured report
#'
#' @return named list with status, paths, versions, and messages
#' @export
run_dependency_check <- function() {
  messages <- character(0)
  status   <- "OK"

  r_path     <- normalizePath(file.path(R.home("bin"), "R"), mustWork = FALSE)
  python_path <- get_command_path("python")
  pgt_path    <- get_command_path("pyGenomeTracks")
  pgt_version <- tryCatch(get_pygenometracks_version(), error = function(e) NA_character_)

  rgt_ok      <- check_rgenometracks()
  bedtools_ok <- check_bedtools()
  pgt_ok      <- check_command_available("pyGenomeTracks")
  conda_ok    <- check_conda_env()

  # Detect PATH conflict: conda active but system R takes priority
  path_conflict <- FALSE
  conda_prefix  <- Sys.getenv("CONDA_PREFIX", unset = "")
  if (nchar(conda_prefix) > 0) {
    conda_r <- file.path(conda_prefix, "bin", "R")
    if (file.exists(conda_r) && !grepl(conda_prefix, r_path, fixed = TRUE)) {
      path_conflict <- TRUE
      messages <- c(messages, paste0(
        "PATH conflict: system R (", r_path, ") takes priority over conda R (", conda_r, "). ",
        "Launch with: conda run -n rgenometrackui Rscript app.R"
      ))
      if (status == "OK") status <- "WARNING"
    }
  }

  if (!rgt_ok) {
    hint <- if (path_conflict)
      " → Run: conda run -n rgenometrackui Rscript install.R  then relaunch with conda run."
    else
      " → Run: Rscript install.R  (with conda env active)"
    messages <- c(messages, paste0("rGenomeTracks is NOT available in the current R session.", hint))
    status   <- "ERROR"
  }
  if (!pgt_ok) {
    messages <- c(messages, "pyGenomeTracks not found in PATH")
    status   <- "ERROR"
  }
  if (!bedtools_ok) {
    messages <- c(messages, "bedtools not found in PATH")
    if (status == "OK") status <- "WARNING"
  }
  if (!conda_ok) {
    messages <- c(messages, sprintf("Conda env 'rgenometrackui' does not appear to be active (found: '%s')",
                                    Sys.getenv("CONDA_DEFAULT_ENV", unset = "<none>")))
    if (status == "OK") status <- "WARNING"
  }

  # Check critical R packages
  required_pkgs <- c("shiny", "bslib", "DT", "jsonlite", "yaml", "processx", "shinyjs")
  missing_pkgs  <- required_pkgs[!vapply(required_pkgs, check_r_package, logical(1))]
  if (length(missing_pkgs) > 0) {
    messages <- c(messages, sprintf("Missing R packages: %s", paste(missing_pkgs, collapse=", ")))
    status <- "ERROR"
  }

  if (length(messages) == 0) messages <- c("All checks passed.")

  list(
    status               = status,
    r_path               = r_path,
    python_path          = python_path,
    pygenometracks_path  = pgt_path,
    pygenometracks_version = pgt_version,
    rGenomeTracks        = rgt_ok,
    bedtools             = bedtools_ok,
    pyGenomeTracks       = pgt_ok,
    conda_active         = Sys.getenv("CONDA_DEFAULT_ENV", unset = "<none>"),
    messages             = messages,
    checked_at           = format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
  )
}
