# =============================================================================
# config_generator.R — Build and write run configuration files
# =============================================================================

#' Build a full run configuration object
#'
#' @param project_config project config list
#' @param run_metadata run metadata list (from create_run)
#' @param tracks list of track objects
#' @param regions character vector of "chr:start-end" strings
#' @param figure_settings named list of figure settings
#' @return named list: the full run config
#' @export
build_run_config <- function(project_config, run_metadata, tracks, regions, figure_settings) {
  list(
    project_name   = project_config$project_name,
    project_slug   = project_config$project_slug,
    genome_label   = project_config$genome_label,
    run_id         = run_metadata$run_id,
    run_name       = run_metadata$run_name,
    renderer       = run_metadata$renderer,
    regions        = as.list(regions),
    figure_settings = figure_settings,
    tracks_count   = length(tracks),
    created_at     = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
    app_version    = APP_VERSION
  )
}

#' Write run_config.yaml to the run's config/ directory
#'
#' @param run_path path to run directory
#' @param run_config list from build_run_config
#' @return invisible path
#' @export
write_run_config <- function(run_path, run_config) {
  path <- file.path(run_path, "config", "run_config.yaml")
  write_yaml_safe(run_config, path)
  invisible(path)
}

#' Write tracks_config.json to the run's config/ directory
#'
#' @param run_path path to run directory
#' @param tracks list of track objects
#' @return invisible path
#' @export
write_tracks_config <- function(run_path, tracks) {
  path <- file.path(run_path, "config", "tracks_config.json")
  write_json_pretty(tracks, path)
  invisible(path)
}

#' Write inputs_manifest.tsv to the run directory
#'
#' Lists all files referenced in the tracks.
#'
#' @param run_path path to run directory
#' @param registry data.frame from load_file_registry
#' @param tracks list of track objects
#' @return invisible path
#' @export
write_inputs_manifest <- function(run_path, registry, tracks) {
  path <- file.path(run_path, "inputs_manifest.tsv")
  # Collect file entries for used tracks
  rows <- lapply(tracks, function(t) {
    if (is.null(t$file_id) || nchar(t$file_id %||% "") == 0) return(NULL)
    if (nrow(registry) == 0) {
      return(data.frame(
        track_name = t$track_name %||% "", file_id = t$file_id,
        file_path = t$file_path %||% "", track_type = t$track_type %||% "",
        stringsAsFactors = FALSE
      ))
    }
    entry <- get_file_by_id(registry, t$file_id)
    if (is.null(entry)) return(NULL)
    data.frame(
      track_name   = t$track_name %||% "",
      file_id      = entry$file_id,
      original_name = entry$original_name,
      stored_path  = entry$stored_path,
      track_type   = t$track_type %||% "",
      stringsAsFactors = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) > 0) {
    manifest <- do.call(rbind, rows)
  } else {
    manifest <- data.frame(track_name=character(), file_id=character(),
                           original_name=character(), stored_path=character(),
                           track_type=character(), stringsAsFactors=FALSE)
  }
  write.table(manifest, path, sep = "\t", row.names = FALSE, quote = FALSE)
  invisible(path)
}

#' Prepare all run files (tracks.ini, scripts, configs, manifest, metadata)
#'
#' This is the main entry point that orchestrates everything needed before a run.
#'
#' @param project_config project config list
#' @param run_path path to run directory (already created via create_run)
#' @param tracks list of track objects (enabled only)
#' @param schema schema list from load_track_schema
#' @param registry data.frame from load_file_registry
#' @param regions character vector of region strings
#' @param figure_settings named list of figure settings
#' @return invisible TRUE
#' @export
prepare_run_files <- function(project_config, run_path, tracks, schema, registry, regions, figure_settings) {
  run_metadata <- load_run_metadata(run_path)

  # Keep only enabled tracks
  enabled_tracks <- Filter(function(t) isTRUE(t$enabled), tracks)
  if (length(enabled_tracks) == 0) stop("No enabled tracks to generate files for")

  # Resolve file paths from registry
  enabled_tracks <- lapply(enabled_tracks, function(t) {
    if (!is.null(t$file_id) && nchar(t$file_id %||% "") > 0 && nrow(registry) > 0) {
      entry <- get_file_by_id(registry, t$file_id)
      if (!is.null(entry)) t$file_path <- entry$stored_path
    }
    t
  })

  # 1. tracks.ini
  write_tracks_ini(enabled_tracks, schema, file.path(run_path, "config", "tracks.ini"))

  # 2. tracks_config.json
  write_tracks_config(run_path, enabled_tracks)

  # 3. run_config.yaml
  run_cfg <- build_run_config(project_config, run_metadata, enabled_tracks, regions, figure_settings)
  write_run_config(run_path, run_cfg)

  # 4. inputs_manifest.tsv
  write_inputs_manifest(run_path, registry, enabled_tracks)

  # 5. R script
  generate_rgenometracks_script(run_path, enabled_tracks, schema, regions, figure_settings)

  # 6. Shell script
  generate_pygenometracks_shell_script(run_path, regions, figure_settings)

  # 7. Update metadata status
  run_metadata$status <- "prepared"
  run_metadata$prepared_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
  save_run_metadata(run_metadata, run_path)

  message(sprintf("[config_generator] Run prepared at: %s", run_path))
  invisible(TRUE)
}
