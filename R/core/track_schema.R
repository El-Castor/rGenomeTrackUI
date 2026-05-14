# =============================================================================
# track_schema.R — Load and query the central track schema
# =============================================================================

#' Load the track schema from YAML
#'
#' @param path path to track_schema.yaml
#' @return list with $tracks and $figure_defaults
#' @export
load_track_schema <- function(path = "config/track_schema.yaml") {
  if (!file.exists(path)) {
    # Try relative to app root
    root_path <- file.path(get_app_root(), path)
    if (file.exists(root_path)) path <- root_path
    else stop(sprintf("Track schema not found at: %s", path))
  }
  schema <- read_yaml_safe(path)
  if (is.null(schema$tracks)) stop("Invalid track schema: missing 'tracks' key")
  schema
}

#' Get a list of supported track type keys
#'
#' @param schema schema list from load_track_schema
#' @return character vector of type keys
#' @export
get_supported_track_types <- function(schema) {
  names(schema$tracks)
}

#' Get a named vector of track types -> labels suitable for selectInput
#'
#' @param schema schema list
#' @return named character vector: names are labels, values are keys
#' @export
get_track_type_choices <- function(schema) {
  types <- schema$tracks
  labels <- vapply(types, function(t) t$label %||% "?", character(1))
  # Return named vector: name = label, value = key
  setNames(names(types), labels)
}

#' Get parameter definitions for a track type
#'
#' @param schema schema list
#' @param track_type track type key (e.g. "bigwig")
#' @return list of parameter definitions
#' @export
get_track_params <- function(schema, track_type) {
  entry <- schema$tracks[[track_type]]
  if (is.null(entry)) stop(sprintf("Unknown track type: %s", track_type))
  entry$params %||% list()
}

#' Get default parameter values for a track type
#'
#' @param schema schema list
#' @param track_type track type key
#' @return named list of defaults
#' @export
get_track_default_params <- function(schema, track_type) {
  params <- get_track_params(schema, track_type)
  defaults <- lapply(params, function(p) p$default)
  setNames(defaults, names(params))
}

#' Check whether a track type requires a file
#'
#' @param schema schema list
#' @param track_type track type key
#' @return logical
#' @export
track_requires_file <- function(schema, track_type) {
  entry <- schema$tracks[[track_type]]
  if (is.null(entry)) stop(sprintf("Unknown track type: %s", track_type))
  isTRUE(entry$file_required)
}

#' Get the pyGenomeTracks file_type value for a track type
#'
#' @param schema schema list
#' @param track_type track type key
#' @return character string or NULL
#' @export
get_pygenometracks_file_type <- function(schema, track_type) {
  entry <- schema$tracks[[track_type]]
  if (is.null(entry)) stop(sprintf("Unknown track type: %s", track_type))
  entry$pygenometracks_file_type
}

#' Get the rGenomeTracks R function name for a track type
#'
#' @param schema schema list
#' @param track_type track type key
#' @return character string or NULL
#' @export
get_rgenometracks_function <- function(schema, track_type) {
  entry <- schema$tracks[[track_type]]
  if (is.null(entry)) stop(sprintf("Unknown track type: %s", track_type))
  entry$rgenometracks_function
}

#' Validate a track against the schema
#'
#' Checks that required fields are present and that the track type is known.
#'
#' @param track list representing a single track
#' @param schema schema list
#' @return invisible TRUE or stops with error
#' @export
validate_track_against_schema <- function(track, schema) {
  if (is.null(track$track_type)) stop("Track missing 'track_type'")
  if (!(track$track_type %in% get_supported_track_types(schema)))
    stop(sprintf("Unknown track type: '%s'", track$track_type))
  if (track_requires_file(schema, track$track_type)) {
    if (is.null(track$file_path) || nchar(trimws(track$file_path %||% "")) == 0)
      stop(sprintf("Track type '%s' requires a file_path", track$track_type))
  }
  invisible(TRUE)
}

#' Get accepted file extensions for a track type
#'
#' @param schema schema list
#' @param track_type track type key
#' @return character vector of extensions
#' @export
get_accepted_extensions <- function(schema, track_type) {
  entry <- schema$tracks[[track_type]]
  if (is.null(entry)) return(character(0))
  unlist(entry$accepted_extensions %||% list())
}
