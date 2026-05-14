# =============================================================================
# templates.R — Track template management
# =============================================================================

#' List available templates
#'
#' @param template_dir directory to scan for .yaml templates
#' @return character vector of template file paths
#' @export
list_templates <- function(template_dir = "templates") {
  if (!dir.exists(template_dir)) {
    # Try relative to app root
    root_dir <- file.path(get_app_root(), template_dir)
    if (dir.exists(root_dir)) template_dir <- root_dir
    else return(character(0))
  }
  list.files(template_dir, pattern = "\\.yaml$", full.names = TRUE)
}

#' Load a template from a YAML file
#'
#' @param template_path path to the .yaml template
#' @return list with template definition
#' @export
load_template <- function(template_path) {
  if (!file.exists(template_path))
    stop(sprintf("Template not found: %s", template_path))
  tmpl <- read_yaml_safe(template_path)
  validate_template(tmpl)
  tmpl
}

#' Validate a template structure
#'
#' @param template list from load_template
#' @return invisible TRUE or stops
#' @export
validate_template <- function(template) {
  if (is.null(template$name)) stop("Template missing 'name'")
  if (is.null(template$tracks)) stop("Template missing 'tracks'")
  if (!is.list(template$tracks)) stop("Template 'tracks' must be a list")
  invisible(TRUE)
}

#' Apply a template to the current project, resolving files from registry
#'
#' Tries to match required files by track_type. Tracks without matching
#' files in the registry are created without a file (user must assign later).
#'
#' @param template list from load_template
#' @param project_config project config list (may be NULL)
#' @param registry data.frame from load_file_registry (may be NULL/empty)
#' @return list(tracks = list of track objects)
#' @export
apply_template <- function(template, project_config = NULL, registry = NULL) {
  tmpl_tracks <- template$tracks
  result_tracks <- vector("list", length(tmpl_tracks))

  for (i in seq_along(tmpl_tracks)) {
    tt       <- tmpl_tracks[[i]]
    ttype    <- tt$track_type %||% "spacer"
    tname    <- tt$track_name %||% tt$label %||% ttype
    tparams  <- tt$params     %||% list()

    # Try to find a matching file from registry
    file_id   <- NULL
    file_path <- ""
    if (!is.null(registry) && nrow(registry) > 0) {
      matches <- registry[registry$track_type_selected == ttype, ]
      if (nrow(matches) > 0) {
        file_id   <- matches$file_id[1]
        file_path <- matches$stored_path[1]
      }
    }

    result_tracks[[i]] <- list(
      track_id   = paste0("t_tmpl_", i, "_", sample.int(9999, 1)),
      track_name = tname,
      track_type = ttype,
      file_id    = file_id,
      file_path  = file_path,
      enabled    = TRUE,
      order      = i,
      params     = tparams,
      created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
      updated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
    )
  }

  list(tracks = result_tracks)
}

#' Save the current run configuration as a reusable template
#'
#' @param run_path path to run directory
#' @param template_name name for the template
#' @param template_dir destination directory
#' @return invisible path to saved template
#' @export
save_run_as_template <- function(run_path, template_name, template_dir = "templates") {
  sanitize_name(template_name)
  slug     <- slugify(template_name)
  cfg_path <- file.path(run_path, "config", "tracks_config.json")

  if (!file.exists(cfg_path))
    stop("No tracks_config.json found. Prepare a run first to save as template.")

  tracks_cfg <- read_json_safe(cfg_path)
  # Strip absolute paths and file IDs for portability
  tmpl_tracks <- lapply(seq_along(tracks_cfg), function(i) {
    t <- tracks_cfg[[i]]
    list(
      track_type = t$track_type %||% "spacer",
      track_name = t$track_name %||% paste0("track_", i),
      params     = t$params %||% list()
    )
  })

  template <- list(
    name        = template_name,
    description = sprintf("Saved from run: %s", basename(run_path)),
    created_at  = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
    tracks      = tmpl_tracks
  )

  ensure_dir(template_dir)
  dest <- file.path(template_dir, paste0(slug, ".yaml"))
  write_yaml_safe(template, dest)
  message(sprintf("[templates] Saved template: %s", dest))
  invisible(dest)
}

# =============================================================================
# Demo project
# =============================================================================

#' Create a demo project pre-loaded with example data
#'
#' @param projects_root root directory where projects live (mandatory)
#' @param project_name human-readable name for the demo project
#' @param overwrite if TRUE and the project already exists, re-loads it; if FALSE, stops
#' @return list(project_config, registry, demo_tracks, demo_regions, demo_figure)
#' @export
create_demo_project <- function(projects_root = NULL,
                                project_name  = "Demo rGenomeTrackUI",
                                overwrite     = TRUE) {
  if (is.null(projects_root) || !nzchar(trimws(as.character(projects_root))))
    stop("create_demo_project: projects_root cannot be NULL or empty")

  projects_root <- normalizePath(projects_root, mustWork = FALSE)
  if (!dir.exists(projects_root))
    dir.create(projects_root, recursive = TRUE, showWarnings = FALSE)

  demo_slug <- slugify(project_name)
  demo_path <- file.path(projects_root, demo_slug)

  # ---- Helper to build the standardised return value ----
  make_demo_result <- function(project_config, registry_df) {
    demo_regions <- list(list(region = "chr1:1000-10000", label = "Region 1"))
    demo_figure  <- list(
      output_format = "png",
      width         = 20,
      dpi           = 100,
      title         = "Demo — rGenomeTrackUI",
      fontsize      = 12,
      renderer      = "pyGenomeTracks"
    )
    demo_tracks <- list(
      list(track_id = "t1", track_type = "bedgraph",    track_name = "Signal",
           enabled = TRUE, file_id = "demo_mini_signal",
           params = list(color = "blue", height = 2)),
      list(track_id = "t2", track_type = "gtf",         track_name = "Gènes",
           enabled = TRUE, file_id = "demo_mini_genes",
           params = list(height = 4)),
      list(track_id = "t3", track_type = "narrowPeak",  track_name = "Peaks",
           enabled = TRUE, file_id = "demo_mini_peaks",
           params = list(color = "red", height = 2)),
      list(track_id = "t4", track_type = "x_axis",      track_name = "Coordonnées",
           enabled = TRUE, file_id = NULL, params = list())
    )
    list(
      project_config = project_config,
      registry       = registry_df,
      demo_tracks    = demo_tracks,
      demo_regions   = demo_regions,
      demo_figure    = demo_figure
    )
  }

  # ---- Already exists — reload ----
  if (dir.exists(demo_path)) {
    if (!overwrite)
      stop(sprintf("Demo project already exists at %s. Use overwrite = TRUE to reload.", demo_path))
    message("[demo] Demo project already exists, loading it.")
    project_config <- load_project(demo_path)
    registry_df    <- tryCatch(
      load_file_registry(project_config),
      error = function(e) {
        warning(sprintf("[demo] Could not load registry: %s", e$message))
        data.frame()
      }
    )
    return(invisible(make_demo_result(project_config, registry_df)))
  }

  # ---- Create project structure ----
  project_config <- create_project(
    project_name = project_name,
    genome_label = "demo",
    description  = "Demonstration project with pre-loaded example tracks.",
    root_dir     = projects_root
  )

  raw_dir <- file.path(demo_path, "inputs", "raw")
  ensure_dir(raw_dir)

  # ---- Copy example_data files ----
  example_dir <- file.path(get_app_root(), "example_data")
  example_files <- c(
    "mini_signal.bedgraph",
    "mini_genes.gtf",
    "mini_peaks.narrowPeak",
    "mini_links.bedpe",
    "mini_regions.bed"
  )

  registry_list <- list()
  for (fname in example_files) {
    src <- file.path(example_dir, fname)
    if (!file.exists(src)) {
      warning(sprintf("[demo] Example file not found, skipping: %s", src))
      next
    }
    dst <- file.path(raw_dir, fname)
    file.copy(src, dst, overwrite = TRUE)
    ftype <- detect_file_type(dst)
    registry_list <- c(registry_list, list(data.frame(
      file_id             = paste0("demo_", tools::file_path_sans_ext(fname)),
      original_name       = fname,
      stored_path         = dst,
      source_path         = src,
      linked_or_copied    = "copied",
      file_type_detected  = ftype,
      track_type_selected = ftype,
      size_bytes          = file.info(dst)$size,
      date_added          = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
      status              = "ok",
      notes               = "Example file",
      stringsAsFactors    = FALSE
    )))
  }

  # ---- Build registry data.frame and persist ----
  if (length(registry_list) > 0) {
    registry_df <- do.call(rbind, registry_list)
  } else {
    registry_df <- data.frame(
      file_id = character(), original_name = character(), stored_path = character(),
      source_path = character(), linked_or_copied = character(),
      file_type_detected = character(), track_type_selected = character(),
      size_bytes = numeric(), date_added = character(),
      status = character(), notes = character(),
      stringsAsFactors = FALSE
    )
  }
  save_file_registry(registry_df, project_config)

  message(sprintf("[demo] Demo project created at: %s", demo_path))
  invisible(make_demo_result(project_config, registry_df))
}
