# =============================================================================
# file_registry.R — Input file tracking and registry
# =============================================================================

# File type detection mapping: extension -> track_type suggestion
.EXT_TO_TYPE <- list(
  ".bw"          = "bigwig",
  ".bigwig"      = "bigwig",
  ".bigWig"      = "bigwig",
  ".bed"         = "bed",
  ".bed.gz"      = "bed",
  ".bedgraph"    = "bedgraph",
  ".bedGraph"    = "bedgraph",
  ".bg"          = "bedgraph",
  ".gtf"         = "gtf",
  ".gtf.gz"      = "gtf",
  ".gff"         = "gtf",
  ".gff3"        = "gtf",
  ".gff.gz"      = "gtf",
  ".narrowPeak"  = "narrowPeak",
  ".narrowpeak"  = "narrowPeak",
  ".bedpe"       = "links",
  ".links"       = "links",
  ".links.gz"    = "links",
  ".cool"        = "hic_matrix",
  ".mcool"       = "hic_matrix",
  ".hic"         = "hic_matrix"
)

#' Detect the likely track type from a file extension
#'
#' @param path file path
#' @return character track type key or "unknown"
#' @export
detect_file_type <- function(path) {
  filename <- basename(path)
  # Check two-part extensions first (.bed.gz, .gtf.gz, etc.)
  for (ext in names(.EXT_TO_TYPE)) {
    if (endsWith(tolower(filename), tolower(ext))) return(.EXT_TO_TYPE[[ext]])
  }
  "unknown"
}

#' Add a file to the project's input registry
#'
#' @param project_config project config list
#' @param source_path original path of the file
#' @param mode "copy" to copy into inputs/raw, "link" to symlink into inputs/linked
#' @param track_type track type override (if NULL, auto-detected)
#' @param notes optional notes string
#' @return updated registry as data.frame
#' @export
add_file_to_registry <- function(project_config, source_path, mode = "copy", track_type = NULL, notes = "", original_name = NULL) {
  validate_file_exists(source_path)
  validate_file_non_empty(source_path)

  registry      <- load_file_registry(project_config)
  file_id       <- paste0("file_", format(Sys.time(), "%Y%m%d%H%M%S"), "_", sample.int(9999, 1))
  # Utilise le nom original fourni (upload) ou le basename du chemin (local)
  original_name <- if (!is.null(original_name) && nchar(trimws(original_name)) > 0) {
    trimws(original_name)
  } else {
    basename(source_path)
  }
  detected_type <- detect_file_type(original_name)  # utilise le vrai nom, pas le chemin temp
  track_type_final <- if (!is.null(track_type) && nchar(trimws(track_type)) > 0) track_type else detected_type

  message(sprintf("[file_registry] file_id=%s  original_name=%s  detected_type=%s  track_type=%s  mode=%s",
                  file_id, original_name, detected_type, track_type_final, mode))

  project_path <- project_config$project_path
  if (mode == "copy") {
    dest_dir <- file.path(project_path, "inputs", "raw")
    ensure_dir(dest_dir)
    dest_path <- file.path(dest_dir, original_name)
    # Avoid overwriting by appending file_id if needed
    if (file.exists(dest_path)) {
      name_noext <- tools::file_path_sans_ext(original_name)
      ext        <- tools::file_ext(original_name)
      dest_path  <- file.path(dest_dir, paste0(name_noext, "_", substr(file_id, 6, 20), ".", ext))
    }
    file.copy(source_path, dest_path)
    message(sprintf("[file_registry] copied to %s", dest_path))
    stored_path       <- dest_path
    linked_or_copied  <- "copied"
  } else {
    dest_dir <- file.path(project_path, "inputs", "linked")
    ensure_dir(dest_dir)
    dest_path <- file.path(dest_dir, original_name)
    if (!file.exists(dest_path)) {
      ok <- file.symlink(normalizePath(source_path, mustWork = FALSE), dest_path)
      if (!ok) stop(sprintf("Impossible de cr\u00e9er le lien symbolique vers : %s", source_path))
      message(sprintf("[file_registry] symlinked to %s", dest_path))
    }
    stored_path      <- dest_path
    linked_or_copied <- "linked"
  }

  new_entry <- data.frame(
    file_id             = file_id,
    original_name       = original_name,
    stored_path         = stored_path,
    source_path         = normalizePath(source_path, mustWork = FALSE),
    linked_or_copied    = linked_or_copied,
    file_type_detected  = detected_type,
    track_type_selected = track_type_final,
    size_bytes          = file.info(source_path)$size,
    date_added          = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
    status              = "ok",
    notes               = notes,
    stringsAsFactors    = FALSE
  )

  if (nrow(registry) == 0) {
    updated <- new_entry
  } else {
    updated <- rbind(registry, new_entry)
  }

  save_file_registry(updated, project_config)
  invisible(updated)
}

#' Load file registry for a project
#'
#' @param project_config project config list
#' @return data.frame (empty if no registry exists yet)
#' @export
load_file_registry <- function(project_config) {
  reg_path <- file.path(project_config$project_path, "inputs", "file_registry.tsv")
  if (!file.exists(reg_path)) {
    return(data.frame(
      file_id = character(), original_name = character(), stored_path = character(),
      source_path = character(), linked_or_copied = character(), file_type_detected = character(),
      track_type_selected = character(), size_bytes = numeric(), date_added = character(),
      status = character(), notes = character(), stringsAsFactors = FALSE
    ))
  }
  tryCatch(
    read.table(reg_path, header = TRUE, sep = "\t", stringsAsFactors = FALSE, quote = ""),
    error = function(e) {
      warning(sprintf("Could not read file registry: %s", e$message))
      data.frame()
    }
  )
}

#' Save file registry to disk
#'
#' @param registry data.frame
#' @param project_config project config list
#' @return invisible
#' @export
save_file_registry <- function(registry, project_config) {
  inputs_dir <- file.path(project_config$project_path, "inputs")
  ensure_dir(inputs_dir)
  tsv_path  <- file.path(inputs_dir, "file_registry.tsv")
  json_path <- file.path(inputs_dir, "file_registry.json")
  write.table(registry, tsv_path, sep = "\t", row.names = FALSE, quote = FALSE)
  write_json_pretty(registry, json_path)
  invisible(NULL)
}

#' Validate a registry entry
#'
#' @param entry single-row data.frame or named list
#' @return invisible TRUE or stops
#' @export
validate_registry_entry <- function(entry) {
  required_fields <- c("file_id", "original_name", "stored_path", "file_type_detected")
  for (f in required_fields) {
    if (is.null(entry[[f]]) || nchar(trimws(as.character(entry[[f]]))) == 0)
      stop(sprintf("Registry entry missing required field: %s", f))
  }
  if (!file.exists(entry$stored_path))
    stop(sprintf("File not found at stored_path: %s", entry$stored_path))
  invisible(TRUE)
}

#' Get a registry entry by file_id
#'
#' @param registry data.frame from load_file_registry
#' @param file_id character file ID
#' @return single-row data.frame or NULL
#' @export
get_file_by_id <- function(registry, file_id) {
  if (nrow(registry) == 0) return(NULL)
  row <- registry[registry$file_id == file_id, ]
  if (nrow(row) == 0) return(NULL)
  row[1, ]
}
