# =============================================================================
# track_persistence.R — Project tracks and reusable track templates
# =============================================================================

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b
}

new_track_id <- function() {
  paste0("track_", format(Sys.time(), "%Y%m%d%H%M%S"), "_", sample(10000:99999, 1))
}

track_now <- function() {
  format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
}

project_tracks_path <- function(project_config) {
  if (is.null(project_config$project_path)) stop("project_config$project_path is required")
  file.path(project_config$project_path, "config", "tracks.json")
}

global_track_templates_path <- function() {
  file.path(path.expand("~"), ".rGenomeTrackUI", "track_templates.json")
}

ensure_app_data_dir <- function() {
  dir.create(file.path(path.expand("~"), ".rGenomeTrackUI"), showWarnings = FALSE, recursive = TRUE)
  for (path in c(global_track_templates_path(), global_track_set_templates_path())) {
    if (!file.exists(path)) writeLines("[]", path)
  }
  invisible(file.path(path.expand("~"), ".rGenomeTrackUI"))
}

global_track_set_templates_path <- function() {
  file.path(path.expand("~"), ".rGenomeTrackUI", "track_set_templates.json")
}

is_empty <- function(x) {
  is.null(x) || length(x) == 0L || all(is.na(x))
}

has_one_selection <- function(x) {
  !is_empty(x) && length(x) == 1L && !is.na(x[[1]])
}

has_multi_selection <- function(x) {
  !is_empty(x) && length(x) >= 2L
}

new_track_set_id <- function() {
  paste0("trackset_", format(Sys.time(), "%Y%m%d%H%M%S"), "_", sample(10000:99999, 1))
}

get_selected_track_ids <- function(input, tracks) {
  selected <- input$tracks_table_rows_selected %||% integer(0)
  if (is_empty(selected)) return(character(0))
  tracks <- standardize_tracks(tracks %||% list())
  if (length(tracks) == 0L) return(character(0))
  selected <- selected[!is.na(selected) & selected >= 1L & selected <= length(tracks)]
  if (length(selected) == 0L) return(character(0))
  ids <- vapply(tracks[selected], function(t) t$track_id %||% "", character(1))
  ids[nzchar(ids)]
}

#' Infer a stable biological colour family from a track
#'
#' Known assays receive deliberately distinct hues. Unknown assays are grouped
#' by the part of their label preceding a time point, so replicates/time courses
#' still share a coherent gradient.
track_colour_family <- function(track) {
  label <- toupper(paste(track$track_name %||% "", track$track_type %||% ""))
  if (grepl("ATAC", label)) return("ATAC")
  if (grepl("(^|[^A-Z])CPG([^A-Z]|$)", label)) return("CPG")
  if (grepl("(^|[^A-Z])CHG([^A-Z]|$)", label)) return("CHG")
  if (grepl("(^|[^A-Z])CHH([^A-Z]|$)", label)) return("CHH")
  if (grepl("RNA|TRANSCRIPT", label)) return("RNA")
  if (grepl("CHIP|H3K|H2A|CTCF", label)) return("CHIP")
  if ((track$track_type %||% "") %in% c("gtf", "genes")) return("GENES")
  if ((track$track_type %||% "") %in% c("bed", "narrowpeak", "broadpeak")) return("BED")
  stem <- toupper(trimws(track$track_name %||% "TRACK"))
  stem <- sub("(?:^|[ _-])(?:D|T|DAY|TIME|H)[ _-]*[0-9]+(?:\\.[0-9]+)?.*$", "", stem, perl = TRUE)
  stem <- gsub("[^A-Z0-9]+", "_", stem)
  paste0("OTHER_", if (nzchar(stem)) stem else "TRACK")
}

#' Extract a numeric time point from a track label
track_timepoint <- function(track) {
  label <- toupper(track$track_name %||% "")
  hit <- regexec("(?:^|[^A-Z0-9])(?:D|T|DAY|TIME|H)[ _-]*([0-9]+(?:\\.[0-9]+)?)", label, perl = TRUE)
  parts <- regmatches(label, hit)[[1]]
  if (length(parts) < 2L) return(NA_real_)
  suppressWarnings(as.numeric(parts[[2]]))
}

track_colour_anchors <- function(family) {
  known <- list(
    ATAC = c("#BAE6FD", "#0284C7", "#075985"),
    CPG = c("#FECACA", "#EF4444", "#991B1B"),
    CHG = c("#99F6E4", "#14B8A6", "#115E59"),
    CHH = c("#F5D0FE", "#D946EF", "#86198F"),
    RNA = c("#BBF7D0", "#22C55E", "#166534"),
    CHIP = c("#FDE68A", "#F59E0B", "#92400E"),
    BED = c("#D4D4D8", "#52525B", "#18181B"),
    GENES = c("#93C5FD", "#2563EB", "#1E3A8A")
  )
  if (!is.null(known[[family]])) return(known[[family]])
  fallback <- list(
    c("#FED7AA", "#F97316", "#9A3412"),
    c("#DDD6FE", "#7C3AED", "#4C1D95"),
    c("#A7F3D0", "#059669", "#064E3B"),
    c("#FBCFE8", "#DB2777", "#831843")
  )
  key <- sum(utf8ToInt(family %||% "OTHER"))
  fallback[[(key %% length(fallback)) + 1L]]
}

#' Assign modality-aware time gradients to tracks
assign_automatic_track_colours <- function(tracks) {
  tracks <- tracks %||% list()
  if (length(tracks) == 0L) return(tracks)
  families <- vapply(tracks, track_colour_family, character(1))
  times <- vapply(tracks, track_timepoint, numeric(1))
  types <- vapply(tracks, function(track) track$track_type %||% "", character(1))
  eligible <- !types %in% c("x_axis", "x-axis", "spacer", "scalebar")
  for (family in unique(families[eligible])) {
    idx <- which(families == family & eligible)
    anchors <- track_colour_anchors(family)
    observed <- sort(unique(times[idx][is.finite(times[idx])]))
    if (length(observed) == 0L) {
      shades <- grDevices::colorRampPalette(anchors)(max(3L, length(idx) + 2L))
      colours <- shades[seq.int(2L, length.out = length(idx))]
    } else if (length(observed) == 1L) {
      colours <- rep(anchors[[2]], length(idx))
    } else {
      scale <- grDevices::colorRampPalette(anchors)(length(observed))
      colours <- vapply(times[idx], function(value) {
        if (!is.finite(value)) anchors[[2]] else scale[[match(value, observed)]]
      }, character(1))
    }
    for (j in seq_along(idx)) {
      i <- idx[[j]]
      if (is.null(tracks[[i]]$params)) tracks[[i]]$params <- list()
      tracks[[i]]$params$color <- colours[[j]]
      tracks[[i]]$updated_at <- track_now()
    }
  }
  tracks
}

filter_tracks_for_display <- function(tracks, modalities, time_keys, keep_context = TRUE) {
  tracks <- tracks %||% list()
  context_types <- c("gtf", "genes", "bed", "narrowpeak", "broadpeak",
                     "x_axis", "x-axis", "spacer", "scalebar")
  lapply(tracks, function(track) {
    type <- track$track_type %||% ""
    family <- track_colour_family(track)
    time <- track_timepoint(track)
    time_key <- if (is.finite(time)) paste0("D", format(time, trim = TRUE)) else "Sans temps"
    track$enabled <- if (type %in% context_types) {
      isTRUE(keep_context)
    } else {
      family %in% (modalities %||% character(0)) && time_key %in% (time_keys %||% character(0))
    }
    track$updated_at <- track_now()
    track
  })
}

standardize_track <- function(track, project_config = NULL) {
  track <- track %||% list()
  params <- track$params %||% list()
  now <- track_now()
  file_path <- track$file_path %||% params$file_path %||% ""
  track_type <- track$track_type %||% params$track_type %||% ""
  track_name <- track$track_name %||% track$title %||% params$title %||% track_type %||% "Track"

  out <- list(
    track_id = track$track_id %||% new_track_id(),
    track_name = track_name,
    track_type = track_type,
    file_id = track$file_id %||% NULL,
    file_path = file_path,
    file_name = track$file_name %||% if (nzchar(file_path)) basename(file_path) else "",
    file_type = track$file_type %||% tools::file_ext(file_path) %||% "",
    enabled = isTRUE(track$enabled %||% TRUE),
    order = as.integer(track$order %||% 999L),
    color = params$color %||% track$color %||% NULL,
    height = suppressWarnings(as.numeric(params$height %||% track$height %||% NA_real_)),
    title = params$title %||% track$title %||% track_name,
    min_value = suppressWarnings(as.numeric(params$min_value %||% track$min_value %||% NA_real_)),
    max_value = suppressWarnings(as.numeric(params$max_value %||% track$max_value %||% NA_real_)),
    scale_mode = params$scale_mode %||% track$scale_mode %||% NULL,
    style = params$style %||% track$style %||% NULL,
    display = params$display %||% track$display %||% NULL,
    labels = params$labels %||% track$labels %||% NULL,
    fontsize = suppressWarnings(as.numeric(params$fontsize %||% track$fontsize %||% NA_real_)),
    gene_rows = suppressWarnings(as.numeric(params$gene_rows %||% track$gene_rows %||% NA_real_)),
    gene_style = params$gene_style %||% track$gene_style %||% params$style %||% NULL,
    gene_display = params$gene_display %||% track$gene_display %||% params$display %||% NULL,
    gene_label_field = params$gene_label_field %||% track$gene_label_field %||% params$prefered_name %||% NULL,
    spacer_before = suppressWarnings(as.numeric(params$spacer_before %||% track$spacer_before %||% NA_real_)),
    spacer_after = suppressWarnings(as.numeric(params$spacer_after %||% track$spacer_after %||% NA_real_)),
    created_at = track$created_at %||% now,
    updated_at = track$updated_at %||% now,
    source_project = track$source_project %||% project_config$project_slug %||% project_config$project_name %||% NULL,
    is_template = isTRUE(track$is_template %||% FALSE),
    template_name = track$template_name %||% NULL,
    template_description = track$template_description %||% NULL,
    params = params
  )
  for (nm in c("color", "height", "title", "min_value", "max_value", "scale_mode",
               "style", "display", "labels", "fontsize", "gene_rows")) {
    if (!is.null(out[[nm]]) && !(length(out[[nm]]) == 1L && is.na(out[[nm]]))) {
      out$params[[nm]] <- out[[nm]]
    }
  }
  out
}

standardize_tracks <- function(tracks, project_config = NULL) {
  tracks <- lapply(tracks %||% list(), standardize_track, project_config = project_config)
  if (length(tracks) == 0L) return(list())
  ids <- vapply(tracks, function(t) t$track_id %||% "", character(1))
  dup <- duplicated(ids) | !nzchar(ids)
  if (any(dup)) {
    for (i in which(dup)) tracks[[i]]$track_id <- new_track_id()
  }
  ord <- order(vapply(tracks, function(t) as.integer(t$order %||% 999L), integer(1)))
  tracks <- tracks[ord]
  for (i in seq_along(tracks)) tracks[[i]]$order <- i
  tracks
}

validate_track <- function(track) {
  required <- c("track_id", "track_name", "track_type", "enabled", "order", "params")
  missing <- required[!vapply(required, function(x) !is.null(track[[x]]), logical(1))]
  if (length(missing) > 0L) stop("Track missing required field(s): ", paste(missing, collapse = ", "))
  if (!nzchar(track$track_id %||% "")) stop("track_id cannot be empty")
  if (!nzchar(track$track_type %||% "")) stop("track_type cannot be empty")
  if (!is.logical(track$enabled) || length(track$enabled) != 1L) stop("enabled must be a logical scalar")
  if (!is.numeric(track$order) && !is.integer(track$order)) stop("order must be numeric")
  if (!is.null(track$file_path) && nzchar(track$file_path) && !file.exists(track$file_path)) {
    warning("Track source file does not exist: ", track$file_path)
  }
  invisible(TRUE)
}

validate_tracks_table <- function(tracks) {
  tracks <- standardize_tracks(tracks)
  ids <- vapply(tracks, function(t) t$track_id %||% "", character(1))
  if (anyDuplicated(ids)) stop("track_id values must be unique")
  invisible(lapply(tracks, validate_track))
}

save_project_tracks <- function(project_config, tracks) {
  path <- project_tracks_path(project_config)
  ensure_dir(dirname(path))
  tracks <- standardize_tracks(tracks, project_config)
  validate_tracks_table(tracks)
  jsonlite::write_json(tracks, path, pretty = TRUE, auto_unbox = TRUE, null = "null")
  message("[TRACK] Project tracks saved: ", path)
  invisible(path)
}

load_project_tracks <- function(project_config) {
  path <- project_tracks_path(project_config)
  if (!file.exists(path)) return(list())
  tracks <- jsonlite::read_json(path, simplifyVector = FALSE)
  standardize_tracks(tracks, project_config)
}

load_track_templates <- function(path = global_track_templates_path()) {
  if (identical(normalizePath(path, mustWork = FALSE), normalizePath(global_track_templates_path(), mustWork = FALSE))) {
    ensure_app_data_dir()
  }
  if (!file.exists(path)) return(list())
  templates <- jsonlite::read_json(path, simplifyVector = FALSE)
  templates <- lapply(templates %||% list(), function(t) {
    t$track <- standardize_track(t$track %||% t)
    t
  })
  message("[TEMPLATE] Loaded ", length(templates), " templates.")
  templates
}

write_track_templates <- function(templates, path = global_track_templates_path()) {
  if (identical(normalizePath(path, mustWork = FALSE), normalizePath(global_track_templates_path(), mustWork = FALSE))) {
    ensure_app_data_dir()
  }
  ensure_dir(dirname(path))
  jsonlite::write_json(templates %||% list(), path, pretty = TRUE, auto_unbox = TRUE, null = "null")
  invisible(path)
}

save_track_template <- function(track, template_name, template_description = NULL,
                                keep_file_path = FALSE, category = NULL,
                                path = global_track_templates_path()) {
  template_name <- trimws(template_name %||% "")
  if (!nzchar(template_name)) stop("template_name cannot be empty")
  track <- standardize_track(track)
  if (!isTRUE(keep_file_path)) {
    track$file_id <- NULL
    track$file_path <- ""
    track$file_name <- ""
    track$file_type <- ""
  }
  track$is_template <- TRUE
  track$template_name <- template_name
  track$template_description <- template_description %||% ""
  template <- list(
    template_id = paste0("template_", format(Sys.time(), "%Y%m%d%H%M%S"), "_", sample(10000:99999, 1)),
    template_name = template_name,
    template_description = template_description %||% "",
    category = category %||% track$track_type %||% "custom",
    requires_file = !track$track_type %in% c("x_axis", "spacer"),
    created_at = track_now(),
    updated_at = track_now(),
    track = track
  )
  templates <- load_track_templates(path)
  templates <- c(templates, list(template))
  write_track_templates(templates, path)
  message("[TEMPLATE] Saved template: ", template_name)
  invisible(template)
}

apply_track_template <- function(template, file_path = NULL, track_name = NULL) {
  track <- standardize_track(template$track %||% template)
  track$track_id <- new_track_id()
  track$track_name <- track_name %||% track$track_name %||% template$template_name %||% "Template track"
  if (!is.null(file_path)) {
    track$file_path <- file_path
    track$file_name <- if (nzchar(file_path)) basename(file_path) else ""
    track$file_type <- tools::file_ext(file_path)
  }
  track$is_template <- FALSE
  track$created_at <- track_now()
  track$updated_at <- track_now()
  track
}

delete_track_template <- function(template_id, path = global_track_templates_path()) {
  templates <- load_track_templates(path)
  templates <- Filter(function(t) !identical(t$template_id %||% "", template_id), templates)
  write_track_templates(templates, path)
  invisible(TRUE)
}

validate_track_set_template <- function(track_set) {
  required <- c("track_set_id", "name", "tracks")
  missing <- required[!vapply(required, function(x) !is.null(track_set[[x]]), logical(1))]
  if (length(missing) > 0L) stop("Track set missing required field(s): ", paste(missing, collapse = ", "))
  if (!is.list(track_set$tracks) || length(track_set$tracks) == 0L) stop("Track set must contain at least one track")
  invisible(TRUE)
}

load_track_set_templates <- function(path = global_track_set_templates_path()) {
  if (identical(normalizePath(path, mustWork = FALSE), normalizePath(global_track_set_templates_path(), mustWork = FALSE))) {
    ensure_app_data_dir()
  }
  if (!file.exists(path)) return(list())
  sets <- jsonlite::read_json(path, simplifyVector = FALSE)
  sets <- lapply(sets %||% list(), function(s) {
    s$tracks <- standardize_tracks(s$tracks %||% list())
    s
  })
  message("[TRACK_SET] Loaded ", length(sets), " track sets.")
  sets
}

write_track_set_templates <- function(track_sets, path = global_track_set_templates_path()) {
  if (identical(normalizePath(path, mustWork = FALSE), normalizePath(global_track_set_templates_path(), mustWork = FALSE))) {
    ensure_app_data_dir()
  }
  ensure_dir(dirname(path))
  jsonlite::write_json(track_sets %||% list(), path, pretty = TRUE, auto_unbox = TRUE, null = "null")
  invisible(path)
}

save_track_set_template <- function(tracks, selected_track_ids, name, description = NULL,
                                    keep_file_paths = TRUE, category = NULL,
                                    path = global_track_set_templates_path()) {
  name <- trimws(name %||% "")
  if (!nzchar(name)) stop("Track set name cannot be empty")
  tracks <- standardize_tracks(tracks %||% list())
  selected_track_ids <- as.character(selected_track_ids %||% character(0))
  selected <- Filter(function(t) t$track_id %in% selected_track_ids, tracks)
  selected <- standardize_tracks(selected)
  if (length(selected) == 0L) stop("No selected tracks to save")
  if (!isTRUE(keep_file_paths)) {
    selected <- lapply(selected, function(t) {
      t$file_id <- NULL
      t$file_path <- ""
      t$file_name <- ""
      t$file_type <- ""
      t
    })
  }
  for (i in seq_along(selected)) selected[[i]]$order <- i
  track_set <- list(
    track_set_id = new_track_set_id(),
    name = name,
    description = description %||% "",
    category = category %||% "custom",
    created_at = track_now(),
    updated_at = track_now(),
    n_tracks = length(selected),
    tracks = selected
  )
  validate_track_set_template(track_set)
  sets <- load_track_set_templates(path)
  sets <- c(sets, list(track_set))
  write_track_set_templates(sets, path)
  message("[TRACK_SET] Saved track set: ", name)
  invisible(track_set)
}

apply_track_set_template <- function(track_set_template, current_tracks, keep_template_file_paths = TRUE) {
  validate_track_set_template(track_set_template)
  current_tracks <- standardize_tracks(current_tracks %||% list())
  base_order <- if (length(current_tracks) == 0L) 0L else max(vapply(current_tracks, function(t) as.integer(t$order %||% 0L), integer(1)))
  new_tracks <- lapply(seq_along(track_set_template$tracks), function(i) {
    t <- standardize_track(track_set_template$tracks[[i]])
    t$track_id <- new_track_id()
    if (!isTRUE(keep_template_file_paths)) {
      t$file_id <- NULL
      t$file_path <- ""
      t$file_name <- ""
      t$file_type <- ""
    }
    t$order <- base_order + i
    t$created_at <- track_now()
    t$updated_at <- track_now()
    t$is_template <- FALSE
    t
  })
  standardize_tracks(c(current_tracks, new_tracks))
}

delete_track_set_template <- function(track_set_id, path = global_track_set_templates_path()) {
  sets <- load_track_set_templates(path)
  sets <- Filter(function(s) !identical(s$track_set_id %||% "", track_set_id), sets)
  write_track_set_templates(sets, path)
  invisible(TRUE)
}

export_track_templates_zip <- function(output_file) {
  path <- global_track_templates_path()
  if (!file.exists(path)) stop("No track template library found.")
  utils::zip(output_file, files = path)
  invisible(output_file)
}

import_track_templates_zip <- function(zip_file) {
  dest_dir <- dirname(global_track_templates_path())
  ensure_dir(dest_dir)
  utils::unzip(zip_file, exdir = dest_dir)
  invisible(load_track_templates())
}

update_track_by_id <- function(tracks, track_id, updated_track) {
  tracks <- standardize_tracks(tracks)
  idx <- which(vapply(tracks, function(t) identical(t$track_id, track_id), logical(1)))
  if (length(idx) != 1L) stop("Track introuvable: ", track_id)
  updated_track$track_id <- track_id
  updated_track$order <- tracks[[idx]]$order
  updated_track$created_at <- tracks[[idx]]$created_at
  updated_track$updated_at <- track_now()
  tracks[[idx]] <- standardize_track(updated_track)
  standardize_tracks(tracks)
}

get_track_by_id <- function(tracks, track_id) {
  idx <- which(vapply(tracks %||% list(), function(t) identical(t$track_id %||% "", track_id), logical(1)))
  if (length(idx) != 1L) return(NULL)
  tracks[[idx]]
}

project_cache_dir <- function(project_config) {
  path <- file.path(project_config$project_path, "cache")
  ensure_dir(path)
  path
}

get_or_build_cache <- function(key, cache_file, build_fun, force = FALSE) {
  cache <- if (!force && file.exists(cache_file)) {
    tryCatch(readRDS(cache_file), error = function(e) list())
  } else {
    list()
  }
  if (!force && !is.null(cache[[key]])) {
    attr(cache[[key]], "cache_hit") <- TRUE
    return(cache[[key]])
  }
  value <- build_fun()
  cache[[key]] <- value
  ensure_dir(dirname(cache_file))
  saveRDS(cache, cache_file)
  attr(value, "cache_hit") <- FALSE
  value
}

validate_files_light <- function(tracks) {
  problems <- character(0)
  for (track in tracks %||% list()) {
    path <- track$file_path %||% ""
    requires_file <- nzchar(path)
    if (!requires_file) next
    if (!file.exists(path)) {
      problems <- c(problems, sprintf("%s: file not found: %s", track$track_name %||% track$track_id, path))
      next
    }
    info <- file.info(path)
    if (is.na(info$size) || info$size <= 0) {
      problems <- c(problems, sprintf("%s: empty file: %s", track$track_name %||% track$track_id, path))
    }
    if (file.access(path, 4) != 0) {
      problems <- c(problems, sprintf("%s: unreadable file: %s", track$track_name %||% track$track_id, path))
    }
  }
  if (length(problems) > 0L) stop(paste(problems, collapse = "\n"))
  invisible(TRUE)
}

hash_run_state <- function(region, regions, tracks, figure_settings, renderer) {
  digest::digest(list(
    region = region,
    regions = regions,
    tracks = lapply(tracks %||% list(), function(t) {
      list(
        track_id = t$track_id,
        track_name = t$track_name,
        track_type = t$track_type,
        file_path = t$file_path,
        file_mtime = if (nzchar(t$file_path %||% "") && file.exists(t$file_path)) file.info(t$file_path)$mtime else NA,
        enabled = t$enabled,
        order = t$order,
        params = t$params
      )
    }),
    figure_settings = figure_settings,
    renderer = renderer
  ))
}
