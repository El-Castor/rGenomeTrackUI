# =============================================================================
# validators.R — Input validation utilities
# =============================================================================

#' Check that a file path exists
#'
#' @param path file path
#' @return invisible TRUE or stops
#' @export
validate_file_exists <- function(path) {
  if (!file.exists(path))
    stop(sprintf("File not found: %s", path))
  invisible(TRUE)
}

#' Check that a file is non-empty
#'
#' @param path file path
#' @return invisible TRUE or stops
#' @export
validate_file_non_empty <- function(path) {
  validate_file_exists(path)
  if (file.info(path)$size == 0)
    stop(sprintf("File is empty: %s", path))
  invisible(TRUE)
}

#' Light validation of a BED file (first few lines)
#'
#' @param path BED file path
#' @param n_lines number of lines to check
#' @return invisible TRUE or stops
#' @export
validate_bed_light <- function(path, n_lines = 10) {
  validate_file_non_empty(path)
  con <- file(path, "r")
  on.exit(close(con))
  lines_checked <- 0
  while (TRUE) {
    line <- readLines(con, n = 1, warn = FALSE)
    if (length(line) == 0) break
    if (grepl("^#|^track|^browser", line)) next
    parts <- strsplit(line, "\t")[[1]]
    if (length(parts) < 3) stop(sprintf("BED file: line has fewer than 3 columns: %s", line))
    if (is.na(suppressWarnings(as.integer(parts[2]))) ||
        is.na(suppressWarnings(as.integer(parts[3]))))
      stop(sprintf("BED file: start/end not integers in line: %s", line))
    lines_checked <- lines_checked + 1
    if (lines_checked >= n_lines) break
  }
  invisible(TRUE)
}

#' Light validation of a BedGraph file
#'
#' @param path BedGraph file path
#' @param n_lines number of lines to check
#' @return invisible TRUE or stops
#' @export
validate_bedgraph_light <- function(path, n_lines = 10) {
  validate_file_non_empty(path)
  con <- file(path, "r")
  on.exit(close(con))
  lines_checked <- 0
  while (TRUE) {
    line <- readLines(con, n = 1, warn = FALSE)
    if (length(line) == 0) break
    if (grepl("^#|^track|^browser", line)) next
    parts <- strsplit(line, "\t| ")[[1]]
    parts <- parts[nchar(parts) > 0]
    if (length(parts) < 4) stop(sprintf("BedGraph: expected 4 columns, got %d", length(parts)))
    if (is.na(suppressWarnings(as.numeric(parts[4]))))
      stop(sprintf("BedGraph: value column not numeric: %s", parts[4]))
    lines_checked <- lines_checked + 1
    if (lines_checked >= n_lines) break
  }
  invisible(TRUE)
}

#' Light validation of a GTF file
#'
#' @param path GTF file path
#' @param n_lines number of lines to check
#' @return invisible TRUE or stops
#' @export
validate_gtf_light <- function(path, n_lines = 10) {
  validate_file_non_empty(path)
  con <- file(path, "r")
  on.exit(close(con))
  lines_checked <- 0
  while (TRUE) {
    line <- readLines(con, n = 1, warn = FALSE)
    if (length(line) == 0) break
    if (grepl("^#", line)) next
    parts <- strsplit(line, "\t")[[1]]
    if (length(parts) < 9) stop(sprintf("GTF: expected 9 tab-separated columns, got %d", length(parts)))
    lines_checked <- lines_checked + 1
    if (lines_checked >= n_lines) break
  }
  invisible(TRUE)
}

#' Light validation of a narrowPeak file
#'
#' @param path narrowPeak file path
#' @param n_lines number of lines to check
#' @return invisible TRUE or stops
#' @export
validate_narrowpeak_light <- function(path, n_lines = 10) {
  validate_file_non_empty(path)
  con <- file(path, "r")
  on.exit(close(con))
  lines_checked <- 0
  while (TRUE) {
    line <- readLines(con, n = 1, warn = FALSE)
    if (length(line) == 0) break
    if (grepl("^#|^track|^browser", line)) next
    parts <- strsplit(line, "\t")[[1]]
    if (length(parts) < 6) stop(sprintf("narrowPeak: expected at least 6 columns, got %d", length(parts)))
    lines_checked <- lines_checked + 1
    if (lines_checked >= n_lines) break
  }
  invisible(TRUE)
}

#' Validate a genomic region string
#'
#' Accepted format: chr:start-end (e.g. chr1:100000-250000)
#'
#' @param region character string
#' @return TRUE if valid, FALSE otherwise (never throws)
#' @export
validate_region <- function(region) {
  if (!is.character(region) || length(region) != 1 || nchar(trimws(region)) == 0)
    return(FALSE)
  parsed <- parse_region(region)
  if (is.null(parsed)) return(FALSE)
  if (parsed$start >= parsed$end) return(FALSE)
  TRUE
}

#' Parse a genomic region string into components
#'
#' @param region character string "chr:start-end"
#' @return list(chrom, start, end, raw) or NULL if unparseable
#' @export
parse_region <- function(region) {
  if (!is.character(region) || length(region) != 1) return(NULL)
  region <- trimws(region)
  # Accept formats: chr1:100000-250000 or chr1:100,000-250,000
  region_clean <- gsub(",", "", region)
  m <- regmatches(region_clean, regexpr("^([^:]+):([0-9]+)-([0-9]+)$", region_clean))
  if (length(m) == 0 || nchar(m) == 0) return(NULL)
  parts <- strsplit(m, "[:|-]")[[1]]
  if (length(parts) != 3) return(NULL)
  list(
    chrom = parts[1],
    chr   = parts[1],   # backward-compat alias
    start = as.integer(parts[2]),
    end   = as.integer(parts[3]),
    raw   = region
  )
}

#' Sanitize a region string for use in a filename
#'
#' @param region character string "chr:start-end"
#' @return safe filename string
#' @export
sanitize_region_for_filename <- function(region) {
  r <- trimws(region)
  r <- gsub(":", "_", r)
  r <- gsub("-", "_", r)
  r <- gsub(",", "", r)
  r <- gsub("[^a-zA-Z0-9_]", "", r)
  r
}

#' Read regions from a BED file
#'
#' @param path path to BED file
#' @return character vector of "chr:start-end" strings
#' @export
read_regions_bed <- function(path) {
  validate_file_non_empty(path)
  lines <- readLines(path, warn = FALSE)
  regions <- character(0)
  for (line in lines) {
    line <- trimws(line)
    if (nchar(line) == 0 || grepl("^#|^track|^browser", line)) next
    parts <- strsplit(line, "\t")[[1]]
    if (length(parts) < 3) next
    regions <- c(regions, sprintf("%s:%s-%s", parts[1], parts[2], parts[3]))
  }
  regions
}

#' Validate a list of tracks against the schema and registry
#'
#' @param tracks list of track objects
#' @param schema schema list
#' @param registry data.frame from load_file_registry
#' @return character vector of warning messages (empty if all OK)
#' @export
validate_track_list <- function(tracks, schema, registry = NULL) {
  warnings <- character(0)
  if (length(tracks) == 0) {
    warnings <- c(warnings, "No tracks defined")
    return(warnings)
  }
  for (i in seq_along(tracks)) {
    t <- tracks[[i]]
    tryCatch(
      validate_track_against_schema(t, schema),
      error = function(e) {
        warnings <<- c(warnings, sprintf("Track %d (%s): %s", i, t$track_name %||% "?", e$message))
      }
    )
    if (track_requires_file(schema, t$track_type) && !is.null(registry)) {
      if (!is.null(t$file_id) && nrow(registry) > 0) {
        found <- registry[registry$file_id == t$file_id, ]
        if (nrow(found) == 0)
          warnings <- c(warnings, sprintf("Track %d (%s): file_id '%s' not found in registry", i, t$track_name %||% "?", t$file_id))
      }
    }
  }
  warnings
}

#' Validate figure settings
#'
#' @param settings named list of figure settings
#' @return character vector of warning strings
#' @export
validate_figure_settings <- function(settings) {
  warnings <- character(0)
  valid_formats <- c("png", "pdf", "svg")
  if (!is.null(settings$output_format)) {
    if (!(settings$output_format %in% valid_formats))
      warnings <- c(warnings, sprintf("output_format '%s' not in %s", settings$output_format, paste(valid_formats, collapse=", ")))
  }
  if (!is.null(settings$width)) {
    w <- suppressWarnings(as.numeric(settings$width))
    if (is.na(w) || w <= 0) warnings <- c(warnings, "width must be a positive number")
  }
  if (!is.null(settings$dpi)) {
    d <- suppressWarnings(as.numeric(settings$dpi))
    if (is.na(d) || d < 72) warnings <- c(warnings, "dpi must be >= 72")
  }
  warnings
}

#' Validate renderer choice
#'
#' @param renderer character: "rGenomeTracks", "pyGenomeTracks", or "both"
#' @return invisible TRUE or stops
#' @export
validate_renderer <- function(renderer) {
  valid <- c("rGenomeTracks", "pyGenomeTracks", "both")
  if (!renderer %in% valid)
    stop(sprintf("Invalid renderer '%s'. Must be one of: %s", renderer, paste(valid, collapse=", ")))
  invisible(TRUE)
}
