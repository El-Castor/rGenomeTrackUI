# =============================================================================
# ini_generator.R — Generate pyGenomeTracks tracks.ini files
# =============================================================================

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b
}

#' Format a single value for INI output
#'
#' @param x value to format
#' @return character string
#' @export
format_ini_value <- function(x) {
  if (is.null(x) || (length(x) == 1 && is.na(x))) return(NULL)
  if (is.logical(x)) return(if (x) "true" else "false")
  if (is.numeric(x)) return(as.character(x))
  as.character(x)
}

DEFAULT_SIGNAL_TRACK_COLORS <- c(
  "#38bdf8", "#8b5cf6", "#22c55e", "#f59e0b",
  "#ef4444", "#14b8a6", "#e879f9"
)

#' Return TRUE for track types rendered as quantitative signals
is_signal_track_type <- function(track_type) {
  track_type %in% c("bigwig", "bedgraph")
}

#' Assign distinct default colors to signal tracks without an explicit color
#'
#' Tracks that still carry the schema default color are treated as auto-colored.
#' Non-default user colors are preserved.
#'
#' @param tracks list of track objects
#' @param schema schema list from load_track_schema
#' @param palette character vector of colors
#' @return updated list of tracks
#' @export
assign_default_track_colors <- function(tracks, schema,
                                        palette = DEFAULT_SIGNAL_TRACK_COLORS) {
  if (length(tracks) == 0) return(tracks)

  signal_i <- 0L
  lapply(tracks, function(track) {
    track_type <- track$track_type %||% ""
    if (!is_signal_track_type(track_type)) return(track)

    signal_i <<- signal_i + 1L
    schema_entry <- schema$tracks[[track_type]]
    default_color <- schema_entry$params$color$default %||% ""
    current_color <- track$params$color %||% ""
    current_color <- trimws(as.character(current_color))

    has_explicit_color <- nchar(current_color) > 0 &&
      !identical(tolower(current_color), tolower(default_color))

    if (!has_explicit_color) {
      assigned <- palette[((signal_i - 1L) %% length(palette)) + 1L]
      if (is.null(track$params)) track$params <- list()
      track$params$color <- assigned
      message("[TrackColor] Track ", track$track_name %||% track$track_id %||% track_type,
              " -> color ", assigned)
    } else {
      message("[TrackColor] Track ", track$track_name %||% track$track_id %||% track_type,
              " -> color ", current_color)
    }

    track
  })
}

annotation_format_from_path <- function(path) {
  lower <- tolower(path %||% "")
  if (grepl("\\.gff3(\\.gz)?$", lower)) return("GFF3")
  if (grepl("\\.gff(\\.gz)?$", lower)) return("GFF")
  if (grepl("\\.gtf(\\.gz)?$", lower)) return("GTF")
  "UNKNOWN"
}

open_text_connection <- function(path) {
  if (grepl("\\.gz$", tolower(path))) gzfile(path, open = "rt") else file(path, open = "rt")
}

detect_annotation_seqnames <- function(path, max_lines = 2000L) {
  if (!file.exists(path)) return(character(0))
  con <- open_text_connection(path)
  on.exit(close(con), add = TRUE)
  seqnames <- character(0)
  repeat {
    lines <- readLines(con, n = max_lines, warn = FALSE)
    if (length(lines) == 0) break
    lines <- lines[!grepl("^\\s*#", lines) & nzchar(lines)]
    if (length(lines) > 0) {
      fields <- strsplit(lines, "\t", fixed = TRUE)
      seqnames <- unique(c(seqnames, vapply(fields, function(x) x[[1]] %||% "", character(1))))
      seqnames <- seqnames[nzchar(seqnames)]
      if (length(seqnames) >= 10L) break
    }
  }
  head(seqnames, 10L)
}

parse_gff_attributes_for_gtf <- function(attr) {
  attr <- attr %||% ""
  parts <- strsplit(attr, ";", fixed = TRUE)[[1]]
  parts <- trimws(parts)
  parts <- parts[nzchar(parts)]
  out <- list()
  for (part in parts) {
    kv <- strsplit(part, "=", fixed = TRUE)[[1]]
    if (length(kv) < 2L) next
    key <- trimws(kv[[1]])
    value <- paste(kv[-1], collapse = "=")
    value <- trimws(value)
    value <- tryCatch(utils::URLdecode(value), error = function(e) value)
    if (nzchar(key)) out[[key]] <- value
  }
  out
}

gtf_attribute_string <- function(gene_id, transcript_id, gene_name = NULL) {
  gene_id <- trimws(as.character(gene_id %||% ""))
  transcript_id <- trimws(as.character(transcript_id %||% gene_id))
  gene_name <- trimws(as.character(gene_name %||% gene_id))
  if (!nzchar(gene_id)) gene_id <- transcript_id
  if (!nzchar(transcript_id)) transcript_id <- gene_id
  if (!nzchar(gene_name)) gene_name <- gene_id

  sprintf(
    'gene_id "%s"; transcript_id "%s"; gene_name "%s";',
    gsub('"', "'", gene_id, fixed = TRUE),
    gsub('"', "'", transcript_id, fixed = TRUE),
    gsub('"', "'", gene_name, fixed = TRUE)
  )
}

normalize_gff_feature_for_gtf <- function(feature_type) {
  ft <- as.character(feature_type %||% "")
  if (ft %in% c("mRNA", "transcript", "lnc_RNA", "ncRNA", "rRNA", "tRNA")) return("transcript")
  if (ft %in% c("five_prime_UTR", "three_prime_UTR", "UTR")) return("UTR")
  ft
}

build_converted_gtf_path <- function(annotation_path, work_dir) {
  base <- basename(annotation_path)
  base <- sub("\\.gz$", "", base, ignore.case = TRUE)
  base <- sub("\\.(gff3|gff)$", "", base, ignore.case = TRUE)
  file.path(work_dir, paste0(base, ".converted.gtf"))
}

#' Convert a GFF/GFF3 annotation to a small GTF suitable for pyGenomeTracks
#'
#' @param annotation_path source GFF/GFF3 path
#' @param out_path destination GTF path
#' @return out_path invisibly
#' @export
convert_gff_to_gtf_for_pygenometracks <- function(annotation_path, out_path) {
  if (!file.exists(annotation_path)) {
    stop(sprintf("Annotation file not found: %s", annotation_path))
  }
  ensure_dir(dirname(out_path))

  con <- open_text_connection(annotation_path)
  on.exit(close(con), add = TRUE)
  lines <- readLines(con, warn = FALSE)
  lines <- lines[!grepl("^\\s*#", lines) & nzchar(lines)]
  if (length(lines) == 0) {
    writeLines(character(0), out_path)
    return(invisible(out_path))
  }

  primary <- grep("\tgene\t", lines, fixed = TRUE, value = TRUE)
  fallback <- FALSE
  if (length(primary) == 0L) {
    primary <- grep("\t(mRNA|transcript)\t", lines, value = TRUE)
    fallback <- TRUE
  }
  if (length(primary) == 0L) {
    writeLines(character(0), out_path)
    message("[TrackGene] converted GFF/GFF3 to GTF: ", out_path, " (0 records)")
    return(invisible(out_path))
  }

  out <- vector("list", length(primary))
  out_i <- 0L

  for (line in primary) {
    x <- strsplit(line, "\t", fixed = TRUE)[[1]]
    if (length(x) < 9L) next
    attr <- parse_gff_attributes_for_gtf(x[[9]])
    id <- attr$ID %||% attr$Name %||% paste0(x[[1]], ":", x[[4]], "-", x[[5]])
    name <- attr$Name %||% attr$gene_name %||% id
    gene_id <- attr$ID %||% attr$Name %||% attr$gene_id %||% attr$locus_tag %||% id
    if (fallback) {
      parents <- strsplit(attr$Parent %||% "", ",", fixed = TRUE)[[1]]
      parents <- trimws(parents[nzchar(parents)])
      gene_id <- parents[[1]] %||% gene_id
    }
    tx_id <- attr$ID %||% gene_id
    gene_name <- name %||% gene_id

    x[[3]] <- "exon"
    x[[9]] <- gtf_attribute_string(gene_id, tx_id, gene_name)
    out_i <- out_i + 1L
    if (out_i > length(out)) length(out) <- length(out) * 2L
    out[[out_i]] <- paste(x[1:9], collapse = "\t")
  }

  out <- unlist(out[seq_len(out_i)], use.names = FALSE)
  writeLines(out, out_path)
  message("[TrackGene] converted GFF/GFF3 to GTF: ", out_path, " (", length(out), " records)")
  invisible(out_path)
}

prepare_annotation_file_for_track <- function(file_path, pgt_type, work_dir = NULL) {
  if (!identical(pgt_type, "gtf")) return(file_path)

  fmt <- annotation_format_from_path(file_path)
  seqnames <- detect_annotation_seqnames(file_path)
  message("[TrackGene] annotation format: ", fmt)
  message("[TrackGene] annotation file: ", file_path)
  message("[TrackGene] first seqnames: ", paste(seqnames, collapse = ", "))

  if (fmt %in% c("GFF3", "GFF")) {
    if (is.null(work_dir) || !nzchar(work_dir)) {
      message("[TrackGene] no work_dir provided; keeping annotation path unchanged")
      return(file_path)
    }
    out_path <- build_converted_gtf_path(file_path, work_dir)
    convert_gff_to_gtf_for_pygenometracks(file_path, out_path)
    message("[TrackGene] converted annotation file: ", out_path)
    return(out_path)
  }

  file_path
}

should_skip_ini_param <- function(param_name, ini_val) {
  if (param_name == "file_type") return(TRUE)
  if (param_name == "data_range_style") return(TRUE)
  if (param_name == "orientation" && identical(tolower(ini_val), "normal")) return(TRUE)
  if (param_name %in% c("min_value", "max_value") && identical(ini_val, "auto")) return(TRUE)
  FALSE
}

#' Convert a track object to an INI block string
#'
#' @param track list representing a single track
#' @param schema schema list from load_track_schema
#' @param work_dir optional directory for generated intermediate files
#' @return character string for one [section] block
#' @export
track_to_ini_block <- function(track, schema, work_dir = NULL) {
  track_type <- track$track_type
  schema_entry <- schema$tracks[[track_type]]
  if (is.null(schema_entry)) stop(sprintf("Unknown track type: %s", track_type))

  # Section header: use track_name if set, else a safe default
  raw_name  <- track$track_name %||% schema_entry$label %||% track_type
  # Sanitize: no brackets allowed in INI section name
  safe_name <- gsub("[\\[\\]]", "", raw_name)
  if (nchar(trimws(safe_name)) == 0) safe_name <- track_type

  lines <- c(sprintf("[%s]", safe_name))

  # file_type
  pgt_type <- schema_entry$pygenometracks_file_type

  # file line first (required for most types)
  file_path <- track$file_path %||% ""
  if (isTRUE(schema_entry$file_required) && nchar(file_path) > 0) {
    file_path <- prepare_annotation_file_for_track(file_path, pgt_type, work_dir)
    lines <- c(lines, sprintf("file = %s", file_path))
  }

  if (!is.null(pgt_type) && nchar(pgt_type) > 0) {
    lines <- c(lines, sprintf("file_type = %s", pgt_type))
  }

  # Parameters: merge schema defaults with track-specific params
  param_defs  <- schema_entry$params %||% list()
  track_params <- track$params %||% list()
  if (is.null(track_params$title) || !nzchar(trimws(as.character(track_params$title %||% "")))) {
    track_params$title <- safe_name
  }

  for (param_name in names(param_defs)) {
    # Use track-specific value if set, else schema default
    value <- track_params[[param_name]]
    if (is.null(value)) value <- param_defs[[param_name]]$default
    if (is.null(value)) next

    ini_val <- format_ini_value(value)
    if (is.null(ini_val) || nchar(trimws(ini_val)) == 0) next

    if (should_skip_ini_param(param_name, ini_val)) next

    lines <- c(lines, sprintf("%s = %s", param_name, ini_val))
  }

  if (identical(pgt_type, "gtf")) {
    message("[TrackGene] Generated gene track config:")
    message(paste(lines, collapse = "\n"))
  }

  # Append a blank line after each block
  lines <- c(lines, "")
  paste(lines, collapse = "\n")
}

#' Generate a full tracks.ini content string from a list of tracks
#'
#' @param tracks list of track objects (enabled only, ordered)
#' @param schema schema list from load_track_schema
#' @param work_dir optional directory for generated intermediate files
#' @return character string with full INI content
#' @export
generate_tracks_ini <- function(tracks, schema, work_dir = NULL) {
  if (length(tracks) == 0) stop("No tracks provided for INI generation")

  # Sort by order field if present
  orders <- vapply(tracks, function(t) as.integer(t$order %||% 999L), integer(1))
  tracks <- tracks[order(orders)]

  # Filter to enabled tracks only
  tracks <- Filter(function(t) isTRUE(t$enabled), tracks)
  if (length(tracks) == 0) stop("No enabled tracks available for INI generation")
  tracks <- assign_default_track_colors(tracks, schema)

  blocks <- vapply(tracks, function(t) {
    tryCatch(track_to_ini_block(t, schema, work_dir = work_dir), error = function(e) {
      warning(sprintf("Skipping track '%s': %s", t$track_name %||% "?", e$message))
      ""
    })
  }, character(1))

  header <- paste0(
    "# tracks.ini — generated by rGenomeTrackUI\n",
    "# Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n",
    "# DO NOT EDIT while a run is in progress\n\n"
  )

  paste0(header, paste(blocks, collapse = "\n"))
}

#' Write a tracks.ini file to disk
#'
#' @param tracks list of track objects
#' @param schema schema list
#' @param path destination file path
#' @return invisible path
#' @export
write_tracks_ini <- function(tracks, schema, path) {
  ensure_dir(dirname(path))
  ini_text <- generate_tracks_ini(tracks, schema, work_dir = dirname(path))
  writeLines(ini_text, path)
  invisible(path)
}
