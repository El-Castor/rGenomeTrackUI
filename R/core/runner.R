# =============================================================================
# runner.R — Execute pyGenomeTracks and rGenomeTracks runs
# =============================================================================

#' Update run status in run_metadata.json
#'
#' @param run_path path to run directory
#' @param status new status string
#' @return invisible NULL
#' @export
update_run_status <- function(run_path, status) {
  meta <- load_run_metadata(run_path)
  meta$status <- status
  if (status %in% c("completed", "failed")) {
    meta$completed_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
  }
  save_run_metadata(meta, run_path)
  invisible(NULL)
}

#' Read run log files
#'
#' @param run_path path to run directory
#' @return list(stdout, stderr, dependency_check, sessionInfo)
#' @export
read_run_logs <- function(run_path) {
  read_log <- function(name) {
    path <- file.path(run_path, "logs", name)
    if (file.exists(path)) paste(readLines(path, warn = FALSE), collapse = "\n") else ""
  }
  list(
    stdout           = read_log("stdout.log"),
    stderr           = read_log("stderr.log"),
    dependency_check = read_log("dependency_check.txt"),
    sessionInfo      = read_log("sessionInfo.txt")
  )
}

#' Execute rGenomeTracks via Rscript in a subprocess
#'
#' @param run_path path to run directory
#' @return list(status, stdout, stderr, exit_code)
#' @export
run_rgenometracks <- function(run_path) {
  script_path <- file.path(run_path, "scripts", "run_rGenomeTracks.R")
  if (!file.exists(script_path)) stop("run_rGenomeTracks.R not found. Run prepare_run_files() first.")

  log_info(run_path, "Starting rGenomeTracks run")
  update_run_status(run_path, "running")

  stdout_file <- file.path(run_path, "logs", "stdout.log")
  stderr_file <- file.path(run_path, "logs", "stderr.log")

  result <- tryCatch({
    processx::run(
      command = file.path(R.home("bin"), "Rscript"),
      args    = script_path,
      stdout  = stdout_file,
      stderr  = stderr_file,
      error_on_status = FALSE,
      wd      = run_path
    )
  }, error = function(e) {
    list(status = -1, stdout = "", stderr = e$message)
  })

  final_status <- if (!is.null(result$status) && result$status == 0) "completed" else "failed"
  if (final_status == "completed") {
    update_run_status(run_path, "completed")
    log_info(run_path, "rGenomeTracks run completed successfully")
  } else {
    update_run_status(run_path, "failed")
    log_error(run_path, sprintf("rGenomeTracks run failed (exit code: %s)", result$status %||% "?"))
  }
  write_run_debug_log(
    run_path  = run_path,
    renderer  = "rGenomeTracks",
    command   = file.path(R.home("bin"), "Rscript"),
    args      = script_path,
    exit_code = result$status %||% -1,
    status    = final_status
  )
  list(status = final_status, exit_code = result$status %||% -1)
}

#' Execute pyGenomeTracks via shell script in a subprocess
#'
#' @param run_path path to run directory
#' @return list(status, exit_code)
#' @export
run_pygenometracks <- function(run_path) {
  script_path <- file.path(run_path, "scripts", "run_pyGenomeTracks.sh")
  if (!file.exists(script_path)) stop("run_pyGenomeTracks.sh not found. Run prepare_run_files() first.")

  log_info(run_path, "Starting pyGenomeTracks run")
  update_run_status(run_path, "running")

  stdout_file <- file.path(run_path, "logs", "stdout.log")
  stderr_file <- file.path(run_path, "logs", "stderr.log")

  # Resolve conda env bin dir so subprocess can find pyGenomeTracks
  conda_bin <- tryCatch({
    cb <- Sys.getenv("CONDA_PREFIX", unset = "")
    if (nchar(cb) > 0 && dir.exists(file.path(cb, "bin"))) file.path(cb, "bin")
    else {
      pg <- Sys.which("pyGenomeTracks")
      if (nchar(pg) > 0) dirname(pg) else ""
    }
  }, error = function(e) "")

  base_path <- Sys.getenv("PATH", unset = "/usr/bin:/bin")
  augmented_path <- if (nchar(conda_bin) > 0 && !grepl(conda_bin, base_path, fixed = TRUE))
    paste0(conda_bin, ":", base_path)
  else base_path

  # processx env replaces env entirely; merge current env + overrides
  current_env <- as.list(Sys.getenv())
  current_env[["MPLBACKEND"]] <- "Agg"
  current_env[["PATH"]] <- augmented_path
  proc_env <- unlist(current_env)

  result <- tryCatch({
    processx::run(
      command = "bash",
      args    = script_path,
      stdout  = stdout_file,
      stderr  = stderr_file,
      error_on_status = FALSE,
      wd      = run_path,
      env     = proc_env
    )
  }, error = function(e) {
    list(status = -1, stderr = e$message)
  })

  final_status <- if (!is.null(result$status) && result$status == 0) "completed" else "failed"
  if (final_status == "completed") {
    update_run_status(run_path, "completed")
    log_info(run_path, "pyGenomeTracks run completed successfully")
  } else {
    update_run_status(run_path, "failed")
    log_error(run_path, sprintf("pyGenomeTracks run failed (exit code: %s)", result$status %||% "?"))
  }
  write_run_debug_log(
    run_path  = run_path,
    renderer  = "pyGenomeTracks",
    command   = "bash",
    args      = script_path,
    exit_code = result$status %||% -1,
    status    = final_status
  )
  list(status = final_status, exit_code = result$status %||% -1)
}

#' Run analysis with the configured renderer
#'
#' @param run_path path to run directory
#' @param renderer "rGenomeTracks", "pyGenomeTracks", or "both"
#' @return list of results per renderer
#' @export
run_analysis <- function(run_path, renderer) {
  validate_renderer(renderer)
  results <- list()
  if (renderer == "rGenomeTracks") {
    results$rGenomeTracks <- run_rgenometracks(run_path)
  } else if (renderer == "pyGenomeTracks") {
    results$pyGenomeTracks <- run_pygenometracks(run_path)
  } else if (renderer == "both") {
    results$pyGenomeTracks <- run_pygenometracks(run_path)
    results$rGenomeTracks  <- run_rgenometracks(run_path)
  }
  results
}

#' Create a zip archive of the run directory
#'
#' @param run_path path to run directory
#' @return path to zip file
#' @export
zip_run <- function(run_path) {
  run_id   <- basename(run_path)
  zip_path <- file.path(dirname(run_path), paste0(run_id, ".zip"))
  original_wd <- getwd()
  setwd(dirname(run_path))
  on.exit(setwd(original_wd))
  utils::zip(zipfile = zip_path, files = run_id)
  invisible(zip_path)
}
