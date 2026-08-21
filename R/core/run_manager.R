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

#' Return the current UI region used as run source of truth
#'
#' rGenomeTrackUI stores the selected regions in app_state$regions. The first
#' entry is the active/current region used for run metadata and result display.
#'
#' @param input optional Shiny input object, kept for future UI-specific sources
#' @param app_state reactiveValues/list/environment containing regions
#' @return single region string or NULL
#' @export
get_current_region <- function(input = NULL, app_state) {
  regions <- app_state$regions %||% character(0)
  if (is.null(regions) || length(regions) == 0L) return(NULL)
  region <- trimws(as.character(regions[[1]]))
  if (!nzchar(region)) return(NULL)
  region
}

#' Signature for the selected region set
#'
#' @param regions character vector of regions
#' @return stable single string
#' @export
region_state_signature <- function(regions) {
  regions <- trimws(as.character(regions %||% character(0)))
  paste(regions[nzchar(regions)], collapse = "|")
}

#' Build expected pyGenomeTracks output path for a region
#'
#' @param run_path run directory
#' @param region region string
#' @param figure_settings named list of figure settings
#' @return absolute path to expected figure output
#' @export
expected_output_file_for_region <- function(run_path, region, figure_settings = NULL) {
  fs <- figure_settings %||% list()
  output_basename <- fs$output_basename %||% "figure"
  output_format <- fs$output_format %||% "png"
  safe_region <- if (exists("sanitize_region_for_filename", mode = "function")) {
    sanitize_region_for_filename(region)
  } else {
    gsub("[^A-Za-z0-9_]", "_", gsub("[:-]", "_", trimws(region)))
  }
  file.path(run_path, "outputs", "multi_region",
            sprintf("%s_%s.%s", output_basename, safe_region, output_format))
}

#' Build a human-readable pyGenomeTracks command preview for diagnostics
#'
#' @param run_path run directory
#' @param region region string
#' @param output_file expected output file
#' @param figure_settings named list of figure settings
#' @return command string
#' @export
build_pygenometracks_command_preview <- function(run_path, region, output_file, figure_settings = NULL) {
  fs <- figure_settings %||% list()
  width <- fs$width %||% 38
  dpi <- fs$dpi %||% 150
  sprintf(
    "pyGenomeTracks --tracks %s --region %s --outFileName %s --width %s --dpi %s",
    shQuote(file.path(run_path, "config", "tracks.ini")),
    shQuote(region),
    shQuote(output_file),
    width,
    dpi
  )
}

#' Invalidate prepared/run display state after a region change
#'
#' @param app_state reactiveValues/list/environment
#' @param region new region string, optional
#' @return invisible app_state
#' @export
invalidate_prepared_run_state <- function(app_state, region = NULL) {
  app_state$current_run <- NULL
  app_state$prepared_config <- NULL
  app_state$run_command <- NULL
  app_state$prepared_run_ready <- FALSE
  app_state$last_prepare_status <- "invalidated"
  app_state$last_output_file <- NULL
  app_state$last_run_region <- NULL
  app_state$preview_image <- NULL
  app_state$region_dirty <- TRUE
  app_state$config_dirty <- TRUE
  invisible(app_state)
}

#' Return TRUE when a prepared run can be launched
#'
#' This deliberately does not require signal scaling, deep validation, or output
#' image existence. Those are launch/render-time concerns after light prepare.
#'
#' @param app_state reactiveValues/list/environment
#' @return logical scalar
#' @export
can_launch_run <- function(app_state) {
  isTRUE(app_state$prepared_run_ready) &&
    !is.null(app_state$prepared_config) &&
    !is.null(app_state$run_command) &&
    !isTRUE(app_state$is_preparing) &&
    !isTRUE(app_state$is_running)
}

#' Select the figure that should be displayed for a run
#'
#' Prefers the explicitly recorded last output file, then metadata, then the
#' newest PNG in outputs/multi_region.
#'
#' @param run_path run directory
#' @param app_state optional reactiveValues/list/environment
#' @param metadata optional run metadata
#' @return figure path or NULL
#' @export
select_run_output_figure <- function(run_path, app_state = NULL, metadata = NULL) {
  candidates <- character(0)
  if (!is.null(app_state)) {
    candidates <- c(candidates, app_state$last_output_file %||% character(0))
  }
  if (!is.null(metadata)) {
    candidates <- c(candidates, metadata$last_output_file %||% character(0))
  }
  candidates <- candidates[nzchar(candidates)]
  candidates <- candidates[file.exists(candidates)]
  candidates <- candidates[startsWith(normalizePath(candidates, mustWork = FALSE),
                                      normalizePath(run_path, mustWork = FALSE))]
  if (length(candidates) > 0L) return(candidates[[1]])

  out_dir <- file.path(run_path, "outputs", "multi_region")
  if (!dir.exists(out_dir)) return(NULL)
  pngs <- list.files(out_dir, pattern = "\\.png$", full.names = TRUE)
  if (length(pngs) == 0L) return(NULL)
  info <- file.info(pngs)
  pngs[order(info$mtime, decreasing = TRUE)][[1]]
}

#' Build a unique output filename for quick re-renders
#'
#' @param region region string
#' @param render_index integer render sequence
#' @param settings optional settings list
#' @return PNG filename
#' @export
make_render_output_filename <- function(region, render_index, settings = NULL) {
  safe_region <- if (exists("sanitize_region_for_filename", mode = "function")) {
    sanitize_region_for_filename(region)
  } else {
    gsub("[^A-Za-z0-9_]", "_", gsub("[:-]", "_", trimws(region)))
  }
  paste0("figure_", safe_region, "_render_", sprintf("%03d", as.integer(render_index)), ".png")
}

#' Merge result-panel rendering settings into figure settings
#'
#' @param base_settings original figure settings
#' @param settings result-panel settings
#' @return figure_settings list compatible with generate_tracks_ini()
#' @export
merge_results_render_settings <- function(base_settings = list(), settings = list()) {
  fs <- base_settings %||% list()
  fs$width <- settings$width %||% fs$width %||% 12
  fs$dpi <- settings$dpi %||% fs$dpi %||% 300
  fs$apply_shared_scale_to_signal_tracks <- isTRUE(settings$apply_to_all_signal_tracks %||% TRUE)
  fs$signal_scale_mode <- settings$signal_scale_mode %||% fs$signal_scale_mode %||% "shared_global_max_padded"
  fs$shared_min_value <- settings$min_value %||% fs$shared_min_value %||% 0
  fs$manual_min_value <- settings$min_value %||% fs$manual_min_value %||% 0
  fs$manual_max_value <- settings$manual_max_value %||% fs$manual_max_value %||% 100
  fs$shared_quantile <- settings$quantile %||% fs$shared_quantile %||% 0.99
  fs$avoid_signal_clipping <- TRUE
  fs$signal_max_padding_factor <- settings$padding_factor %||% fs$signal_max_padding_factor %||% 1.15
  fs$signal_track_height <- settings$track_height %||% fs$signal_track_height %||% 1.1
  fs$annotation_track_height <- settings$annotation_height %||% fs$annotation_track_height %||% 0.25
  fs$annotation_labels <- isTRUE(settings$annotation_labels %||% FALSE)
  fs$insert_spacer_before_genes <- TRUE
  fs$spacer_before_genes_height <- settings$spacer_height %||% fs$spacer_before_genes_height %||% 0.05
  fs$gene_track_height <- settings$gene_track_height %||% fs$gene_track_height %||% 0.9
  fs$gene_label_fontsize <- settings$fontsize %||% fs$gene_label_fontsize %||% 6
  fs$gene_rows <- settings$gene_rows %||% fs$gene_rows %||% 0
  fs
}

#' Apply quick-render layout settings directly to track objects
#'
#' @param tracks list of track objects
#' @param settings result-panel settings
#' @return updated tracks
#' @export
apply_results_render_track_settings <- function(tracks, settings = list()) {
  signal_height <- suppressWarnings(as.numeric(settings$track_height %||% NA_real_))
  annotation_height <- suppressWarnings(as.numeric(settings$annotation_height %||% NA_real_))
  lapply(tracks %||% list(), function(track) {
    if (is.null(track$params)) track$params <- list()
    if (is_signal_track(track) && is.finite(signal_height)) {
      track$params$height <- signal_height
    }
    if (identical(track$track_type %||% "", "bed") && is.finite(annotation_height)) {
      track$params$height <- annotation_height
    }
    track
  })
}

#' Compute scale summary for result-panel diagnostics
#'
#' @param tracks list of track objects
#' @param region region string
#' @param figure_settings merged figure settings
#' @return scale summary list
#' @export
compute_render_signal_summary <- function(tracks, region, figure_settings = list()) {
  fs <- figure_settings %||% list()
  signal_tracks <- Filter(is_signal_track, tracks %||% list())
  compute_shared_signal_scale(
    signal_tracks = signal_tracks,
    region = region,
    mode = fs$signal_scale_mode %||% "auto_per_track",
    quantile = fs$shared_quantile %||% 0.99,
    shared_min_value = fs$shared_min_value %||% 0,
    manual_min_value = fs$manual_min_value %||% 0,
    manual_max_value = fs$manual_max_value %||% NA_real_,
    avoid_clipping = isTRUE(fs$avoid_signal_clipping %||% TRUE),
    padding_factor = fs$signal_max_padding_factor %||% 1.15
  )
}

#' Re-render the last pyGenomeTracks figure with display-only settings
#'
#' @param app_state application reactiveValues
#' @param settings result-panel settings
#' @param schema optional track schema
#' @param preview if TRUE, render lower DPI/width
#' @return list with render status and paths
#' @export
rerender_last_figure <- function(app_state, settings, schema = NULL, preview = FALSE) {
  last_render <- app_state$last_render %||% NULL
  if (is.null(last_render)) stop("Aucune figure précédente à ajuster.")
  run_path <- last_render$run_path %||% app_state$last_run_path %||% NULL
  if (is.null(run_path) || !dir.exists(run_path)) stop("Dossier du dernier run introuvable.")
  region <- settings$region %||% last_render$region %||% NULL
  if (is.null(region) || !nzchar(region)) stop("Région du dernier rendu introuvable.")

  tracks <- last_render$tracks %||% list()
  if (length(tracks) == 0L) {
    cfg <- file.path(run_path, "config", "tracks_config.json")
    if (file.exists(cfg)) tracks <- read_json_safe(cfg)
  }
  if (length(tracks) == 0L) stop("Aucune track disponible pour le re-rendu.")

  if (is.null(schema)) {
    schema <- load_track_schema(file.path(get_app_root(), "config", "track_schema.yaml"))
  }

  render_index <- as.integer(last_render$render_index %||% 0L) + 1L
  tracks <- apply_results_render_track_settings(tracks, settings)
  fs <- merge_results_render_settings(last_render$figure_settings %||% app_state$figure_settings %||% list(), settings)
  if (isTRUE(preview)) {
    fs$width <- min(as.numeric(fs$width %||% 20), 18)
    fs$dpi <- min(as.numeric(fs$dpi %||% 100), 90)
  }

  scale_summary <- compute_render_signal_summary(tracks, region, fs)
  ini_dir <- file.path(run_path, "config")
  output_dir <- last_render$output_dir %||% file.path(run_path, "outputs", "multi_region")
  ensure_dir(ini_dir)
  ensure_dir(output_dir)

  ini_file <- file.path(ini_dir, sprintf("rerender_%03d.ini", render_index))
  output_file <- file.path(output_dir, make_render_output_filename(region, render_index, settings))
  write_tracks_ini(tracks, schema, ini_file, regions = region, figure_settings = fs)

  pygt_bin <- Sys.which("pyGenomeTracks")
  if (!nzchar(pygt_bin)) pygt_bin <- "pyGenomeTracks"
  args <- c(
    "--tracks", ini_file,
    "--region", region,
    "--outFileName", output_file,
    "--width", as.character(fs$width %||% 38),
    "--dpi", as.character(fs$dpi %||% 150)
  )
  if (nzchar(fs$title %||% "")) args <- c(args, "--title", fs$title)
  command_preview <- paste(shQuote(pygt_bin), paste(shQuote(args), collapse = " "))

  log_info(run_path, "[RENDER] Re-render requested")
  log_info(run_path, sprintf("[RENDER] Region: %s", region))
  log_info(run_path, sprintf("[RENDER] Scaling mode: %s", fs$signal_scale_mode %||% "auto_per_track"))
  log_info(run_path, sprintf("[RENDER] Raw global max: %s", format_scale_value(scale_summary$raw_global_max %||% NA_real_)))
  log_info(run_path, sprintf("[RENDER] Final max_value: %s", format_scale_value(scale_summary$shared_max_value %||% NA_real_)))
  log_info(run_path, sprintf("[RENDER] New ini: %s", ini_file))
  log_info(run_path, sprintf("[RENDER] New image: %s", output_file))
  log_info(run_path, sprintf("[RENDER] pyGenomeTracks command: %s", command_preview))

  current_env <- as.list(Sys.getenv())
  current_env[["MPLBACKEND"]] <- "Agg"
  result <- processx::run(
    command = pygt_bin,
    args = args,
    stdout = "|",
    stderr = "|",
    error_on_status = FALSE,
    wd = run_path,
    env = unlist(current_env)
  )
  if (nzchar(result$stdout %||% "")) {
    append_log(file.path(run_path, "logs", "stdout.log"), result$stdout)
  }
  if (nzchar(result$stderr %||% "")) {
    append_log(file.path(run_path, "logs", "stderr.log"), result$stderr)
  }
  if (!identical(result$status, 0L) || !file.exists(output_file)) {
    log_error(run_path, sprintf("[RENDER] Re-render failed with exit code %s", result$status %||% "?"))
    stop("Le re-rendu pyGenomeTracks a échoué. Consultez les logs.")
  }

  app_state$last_render <- list(
    region = region,
    tracks = tracks,
    figure_settings = fs,
    config_template = last_render$config_template %||% file.path(run_path, "config", "tracks.ini"),
    output_dir = output_dir,
    run_path = run_path,
    last_ini = ini_file,
    last_image = output_file,
    render_index = render_index,
    signal_scaling_summary = scale_summary,
    command = command_preview
  )
  app_state$last_output_file <- output_file
  app_state$last_run_region <- region

  meta <- tryCatch(load_run_metadata(run_path), error = function(e) NULL)
  if (!is.null(meta)) {
    meta$last_output_file <- output_file
    meta$last_run_region <- region
    meta$last_render_ini <- ini_file
    meta$last_render_index <- render_index
    save_run_metadata(meta, run_path)
  }

  list(
    status = "completed",
    image = output_file,
    ini = ini_file,
    render_index = render_index,
    signal_scaling_summary = scale_summary
  )
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
