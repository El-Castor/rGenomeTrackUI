# =============================================================================
# run_manager.R — Run creation and management
# =============================================================================

#' Create a new run for a project
#'
#' @param project_config project config list (from load_project)
#' @param run_name human-readable run name
#' @param region region string e.g. "chr1:100000-250000"
#' @param renderer "rGenomeTracks", "pyGenomeTracks", or "both"
#' @return list with run metadata
#' @export
create_run <- function(project_config, run_name, region, renderer = "pyGenomeTracks") {
  validate_renderer(renderer)
  sanitize_name(run_name)

  run_slug  <- slugify(run_name)
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  run_id    <- paste0(timestamp, "_", run_slug)

  project_path <- project_config$project_path
  run_path     <- file.path(project_path, "runs", run_id)

  if (dir.exists(run_path))
    stop(sprintf("Run directory already exists: %s", run_path))

  create_run_dirs(run_path)

  metadata <- list(
    run_id         = run_id,
    run_name       = run_name,
    run_slug       = run_slug,
    project_slug   = project_config$project_slug,
    project_name   = project_config$project_name,
    project_path   = project_path,
    run_path       = run_path,
    region         = region,
    renderer       = renderer,
    status         = "created",
    created_at     = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
    completed_at   = NULL,
    app_version    = APP_VERSION
  )

  save_run_metadata(metadata, run_path)
  message(sprintf("[run_manager] Run '%s' created at: %s", run_id, run_path))
  invisible(metadata)
}

#' Create the standard directory structure for a run
#'
#' @param run_path path for the run directory
#' @return invisible run_path
#' @export
create_run_dirs <- function(run_path) {
  dirs <- c(
    run_path,
    file.path(run_path, "config"),
    file.path(run_path, "scripts"),
    file.path(run_path, "logs"),
    file.path(run_path, "outputs", "multi_region"),
    file.path(run_path, "preview")
  )
  for (d in dirs) ensure_dir(d)
  # Create empty log files
  for (f in c("stdout.log", "stderr.log", "sessionInfo.txt", "dependency_check.txt")) {
    log_file <- file.path(run_path, "logs", f)
    if (!file.exists(log_file)) file.create(log_file)
  }
  invisible(run_path)
}

#' List all runs in a project
#'
#' @param project_path project directory path
#' @return data.frame of runs sorted by date descending
#' @export
list_runs <- function(project_path) {
  runs_dir <- file.path(project_path, "runs")
  if (!dir.exists(runs_dir)) return(data.frame())

  dirs <- list.dirs(runs_dir, recursive = FALSE, full.names = TRUE)
  results <- lapply(dirs, function(d) {
    meta_path <- file.path(d, "run_metadata.json")
    if (!file.exists(meta_path)) return(NULL)
    meta <- tryCatch(read_json_safe(meta_path), error = function(e) NULL)
    if (is.null(meta)) return(NULL)
    data.frame(
      run_id         = meta$run_id         %||% basename(d),
      run_name       = meta$run_name       %||% "",
      region         = meta$region         %||% "",
      renderer       = meta$renderer       %||% "",
      status         = meta$status         %||% "unknown",
      created_at     = meta$created_at     %||% "",
      completed_at   = meta$completed_at   %||% "",
      run_path       = d,
      stringsAsFactors = FALSE
    )
  })
  results <- Filter(Negate(is.null), results)
  if (length(results) == 0) return(data.frame())
  df <- do.call(rbind, results)
  df[order(df$created_at, decreasing = TRUE), ]
}

#' Load run metadata
#'
#' @param run_path path to run directory
#' @return list with run metadata
#' @export
load_run_metadata <- function(run_path) {
  meta_path <- file.path(run_path, "run_metadata.json")
  if (!file.exists(meta_path))
    stop(sprintf("No run_metadata.json found at: %s", run_path))
  read_json_safe(meta_path)
}

#' Save run metadata
#'
#' @param metadata list
#' @param run_path path to run directory
#' @return invisible path
#' @export
save_run_metadata <- function(metadata, run_path) {
  meta_path <- file.path(run_path, "run_metadata.json")
  write_json_pretty(metadata, meta_path)
  invisible(meta_path)
}

#' Duplicate run configuration to a new run
#'
#' Copies config files (tracks.ini, tracks_config.json, run_config.yaml) to a
#' new run without copying outputs or logs.
#'
#' @param source_run_path path to the source run
#' @param project_config project config list
#' @param new_run_name name for the new run
#' @return new run metadata list
#' @export
duplicate_run_config <- function(source_run_path, project_config, new_run_name) {
  source_meta <- load_run_metadata(source_run_path)

  new_run <- create_run(
    project_config = project_config,
    run_name       = new_run_name,
    region         = source_meta$region %||% "",
    renderer       = source_meta$renderer %||% "pyGenomeTracks"
  )

  # Copy config files
  src_config <- file.path(source_run_path, "config")
  dst_config <- file.path(new_run$run_path, "config")
  for (f in c("tracks.ini", "tracks_config.json", "run_config.yaml")) {
    src_f <- file.path(src_config, f)
    if (file.exists(src_f)) file.copy(src_f, file.path(dst_config, f))
  }

  # Copy inputs_manifest if present
  src_manifest <- file.path(source_run_path, "inputs_manifest.tsv")
  if (file.exists(src_manifest))
    file.copy(src_manifest, file.path(new_run$run_path, "inputs_manifest.tsv"))

  new_run$status <- "duplicated"
  new_run$duplicated_from <- source_run_path
  save_run_metadata(new_run, new_run$run_path)
  invisible(new_run)
}

#' Delete a run directory
#'
#' @param run_path path to run
#' @param confirm must be TRUE to actually delete
#' @return invisible TRUE
#' @export
delete_run <- function(run_path, confirm = FALSE) {
  if (!confirm) stop("delete_run: confirm must be TRUE to delete a run")
  if (!dir.exists(run_path)) stop(sprintf("Run not found: %s", run_path))
  unlink(run_path, recursive = TRUE)
  message(sprintf("[run_manager] Deleted run: %s", run_path))
  invisible(TRUE)
}
