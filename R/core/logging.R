# =============================================================================
# logging.R — Run-scoped logging utilities
# =============================================================================

#' Get current timestamp string
#'
#' @return formatted timestamp
#' @export
timestamp_now <- function() {
  format(Sys.time(), "[%Y-%m-%d %H:%M:%S]")
}

#' Append a message to a log file
#'
#' @param path log file path
#' @param message message string
#' @return invisible NULL
#' @export
append_log <- function(path, message) {
  ensure_dir(dirname(path))
  cat(paste0(message, "\n"), file = path, append = TRUE)
  invisible(NULL)
}

#' Log an INFO message to a run's stdout.log
#'
#' @param run_path path to run directory
#' @param message message string
#' @return invisible NULL
#' @export
log_info <- function(run_path, message) {
  line <- sprintf("%s [INFO] %s", timestamp_now(), message)
  cat(line, "\n")
  append_log(file.path(run_path, "logs", "stdout.log"), line)
}

#' Log a WARNING message to a run's stdout.log
#'
#' @param run_path path to run directory
#' @param message message string
#' @return invisible NULL
#' @export
log_warning <- function(run_path, message) {
  line <- sprintf("%s [WARNING] %s", timestamp_now(), message)
  message(line)
  append_log(file.path(run_path, "logs", "stdout.log"), line)
}

#' Log an ERROR message to a run's stderr.log
#'
#' @param run_path path to run directory
#' @param message message string
#' @return invisible NULL
#' @export
log_error <- function(run_path, message) {
  line <- sprintf("%s [ERROR] %s", timestamp_now(), message)
  message(line)
  append_log(file.path(run_path, "logs", "stderr.log"), line)
}

log_prepare_perf <- function(project_config, run_id, step, duration_sec,
                             region = "", n_tracks = 0L, n_signal_tracks = 0L,
                             cache_hit = NA, status = "ok") {
  project_path <- project_config$project_path %||% "."
  path <- file.path(project_path, "logs", "prepare_run_perf.tsv")
  ensure_dir(dirname(path))
  row <- data.frame(
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
    run_id = run_id %||% "",
    step = step,
    duration_sec = round(as.numeric(duration_sec), 3),
    region = region %||% "",
    n_tracks = as.integer(n_tracks %||% 0L),
    n_signal_tracks = as.integer(n_signal_tracks %||% 0L),
    cache_hit = if (is.na(cache_hit)) "" else as.character(isTRUE(cache_hit)),
    status = status %||% "ok",
    stringsAsFactors = FALSE
  )
  write.table(row, path, sep = "\t", row.names = FALSE, col.names = !file.exists(path),
              append = file.exists(path), quote = FALSE)
  invisible(path)
}

time_step <- function(label, expr, run_path = NULL, project_config = NULL,
                      run_id = "", region = "", n_tracks = 0L,
                      n_signal_tracks = 0L, cache_hit = NA, status = "ok") {
  t0 <- Sys.time()
  result <- force(expr)
  dt <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 3)
  line <- sprintf("[PERF] %s: %.3f sec", label, dt)
  if (!is.null(run_path)) log_info(run_path, line) else message(line)
  if (!is.null(project_config)) {
    log_prepare_perf(project_config, run_id, label, dt, region, n_tracks, n_signal_tracks, cache_hit, status)
  }
  result
}

#' Write a structured debug log for a completed run
#'
#' Writes to `<run_path>/logs/run_debug.log` and appends a summary line
#' to `<app_root>/logs/run_debug.log` for cross-run traceability.
#'
#' @param run_path path to run directory
#' @param renderer renderer name used ("pyGenomeTracks", "rGenomeTracks", or "both")
#' @param command command or executable used to run the analysis
#' @param args arguments passed to command (character vector or single string)
#' @param exit_code numeric exit code returned by the subprocess
#' @param status final status string ("completed" or "failed")
#' @return invisible NULL
#' @export
write_run_debug_log <- function(run_path, renderer, command, args, exit_code, status) {
  out_dir <- file.path(run_path, "outputs", "multi_region")
  pngs    <- if (dir.exists(out_dir))
    list.files(out_dir, pattern = "\\.png$") else character(0)

  lines <- c(
    "=== Run Debug Log ===",
    sprintf("Timestamp     : %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    sprintf("run_path      : %s", run_path),
    sprintf("renderer      : %s", renderer),
    sprintf("command       : %s %s", command, paste(as.character(args), collapse = " ")),
    sprintf("exit_code     : %s", exit_code),
    sprintf("status        : %s", status),
    sprintf("ini_path      : %s", file.path(run_path, "config", "tracks.ini")),
    sprintf("out_dir       : %s", out_dir),
    sprintf("figures       : %s", if (length(pngs) > 0) paste(pngs, collapse = ", ") else "none"),
    ""
  )

  # Write to run-scoped log
  run_debug_path <- file.path(run_path, "logs", "run_debug.log")
  writeLines(lines, run_debug_path)

  # Append one-liner to global logs/run_debug.log
  app_root <- tryCatch(get_app_root(), error = function(e) NULL)
  if (!is.null(app_root)) {
    global_log <- file.path(app_root, "logs", "run_debug.log")
    ensure_dir(dirname(global_log))
    summary_line <- sprintf("%s [%s] %s | exit=%s | figures=%s",
                            format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"),
                            status, basename(run_path), exit_code,
                            if (length(pngs) > 0) length(pngs) else 0)
    cat(paste0(summary_line, "\n"), file = global_log, append = TRUE)
  }

  invisible(NULL)
}
