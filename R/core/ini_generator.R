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

INSERT_SPACER_BEFORE_GENES <- TRUE
SPACER_BEFORE_GENES_HEIGHT <- 0.4
AVOID_SIGNAL_CLIPPING <- TRUE
SIGNAL_MAX_PADDING_FACTOR <- 1.15
COMPUTE_SIGNAL_SCALE_DURING_PREPARE <- FALSE

SIGNAL_SCALE_MODE <- c(
  "auto_per_track",
  "shared_by_modality",
  "shared_global_max",
  "shared_global_max_padded",
  "shared_global_quantile",
  "manual"
)

infer_signal_modality <- function(track) {
  text <- tolower(paste(
    track$track_name %||% "", track$file_name %||% "", track$file_path %||% "",
    (track$params %||% list())$track_group %||% ""
  ))
  if (grepl("cpg", text)) return("WGBS-CpG")
  if (grepl("(^|[^a-z])chg([^a-z]|$)", text)) return("WGBS-CHG")
  if (grepl("chh", text)) return("WGBS-CHH")
  if (grepl("wgbs|methyl|bisulf", text)) return("WGBS")
  if (grepl("atac|accessib", text)) return("ATAC-seq")
  group <- trimws(as.character((track$params %||% list())$track_group %||% ""))
  if (nzchar(group)) group else "Autre signal"
}

apply_signal_scale_by_modality <- function(tracks, regions, quantile = 0.99,
                                           shared_min_value = 0,
                                           avoid_clipping = TRUE,
                                           padding_factor = 1.15) {
  signal_idx <- which(vapply(tracks, is_signal_track, logical(1)))
  if (length(signal_idx) == 0L) return(list(tracks = tracks, scale_info = NULL))
  groups <- vapply(tracks[signal_idx], infer_signal_modality, character(1))
  summaries <- list()
  all_warnings <- character(0)
  all_used <- character(0)
  all_excluded <- character(0)
  total_values <- 0L

  for (group in unique(groups)) {
    idx <- signal_idx[groups == group]
    info <- compute_shared_signal_scale(
      tracks[idx], regions, "shared_global_max_padded",
      quantile = quantile,
      shared_min_value = shared_min_value,
      avoid_clipping = avoid_clipping,
      padding_factor = padding_factor
    )
    summaries[[group]] <- info
    for (i in idx) {
      if (is.null(tracks[[i]]$params)) tracks[[i]]$params <- list()
      tracks[[i]]$params$track_group <- group
      tracks[[i]] <- write_signal_track_with_scale(tracks[[i]], info)
    }
    all_used <- c(all_used, info$tracks_used %||% character(0))
    all_excluded <- c(all_excluded, info$tracks_excluded %||% character(0))
    total_values <- total_values + as.integer(info$number_of_values_used %||% 0L)
    all_warnings <- c(all_warnings, paste0(group, " : ", info$warning %||% character(0)))
    message("[INFO] Modality scale ", group, ": max_value=",
            format_scale_value(info$shared_max_value), " (", length(idx), " track(s))")
  }

  list(
    tracks = tracks,
    scale_info = list(
      scaling_mode = "shared_by_modality",
      group_summaries = summaries,
      raw_global_max = NULL,
      shared_min_value = NULL,
      shared_max_value = NULL,
      final_max_value_used = NULL,
      padding_factor = padding_factor,
      quantile = quantile,
      potential_clipping = "no",
      number_of_values_used = total_values,
      tracks_used = all_used,
      tracks_excluded = all_excluded,
      warning = all_warnings
    )
  )
}

make_signal_scale_cache_key <- function(files, region, mode, quantile, padding,
                                        shared_min_value = 0,
                                        manual_min_value = NA_real_,
                                        manual_max_value = NA_real_) {
  files <- as.character(files %||% character(0))
  file_info <- if (length(files) > 0L) {
    info <- file.info(files)
    data.frame(
      file = normalizePath(files, mustWork = FALSE),
      size = info$size,
      mtime = as.character(info$mtime),
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(file = character(0), size = numeric(0), mtime = character(0))
  }
  digest::digest(list(
    files = file_info,
    region = region,
    mode = mode,
    quantile = quantile,
    padding = padding,
    shared_min_value = shared_min_value,
    manual_min_value = manual_min_value,
    manual_max_value = manual_max_value
  ))
}

#' Return TRUE for track types rendered as quantitative signals
is_signal_track_type <- function(track_type) {
  tolower(track_type %||% "") %in% c("bigwig", "bw", "bedgraph", "wig")
}

#' Return TRUE when a track object is a quantitative signal track
#'
#' @param track track list or track type string
#' @return logical scalar
#' @export
is_signal_track <- function(track) {
  if (is.character(track)) return(is_signal_track_type(track[[1]] %||% ""))
  track_type <- tolower(track$track_type %||% "")
  is_signal_track_type(track_type)
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
  if (grepl("\\.bed12(\\.gz)?$", lower)) return("BED12")
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
    if (grepl("=", part, fixed = TRUE)) {
      kv <- strsplit(part, "=", fixed = TRUE)[[1]]
      if (length(kv) < 2L) next
      key <- trimws(kv[[1]])
      value <- paste(kv[-1], collapse = "=")
    } else {
      m <- regexec("^([^[:space:]]+)[[:space:]]+\"?([^\"]+)\"?$", part)
      hit <- regmatches(part, m)[[1]]
      if (length(hit) < 3L) next
      key <- trimws(hit[[2]])
      value <- hit[[3]]
    }
    value <- trimws(value)
    value <- sub("^\"", "", value)
    value <- sub("\"$", "", value)
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

build_converted_bed12_path <- function(annotation_path, work_dir) {
  base <- basename(annotation_path)
  base <- sub("\\.gz$", "", base, ignore.case = TRUE)
  base <- sub("\\.(gff3|gff|gtf)$", "", base, ignore.case = TRUE)
  file.path(work_dir, paste0(base, ".converted.bed12"))
}

annotation_qc_path <- function(work_dir) {
  file.path(work_dir, "annotation_track_QC.tsv")
}

extract_parent_ids <- function(x) {
  parents <- strsplit(x %||% "", ",", fixed = TRUE)[[1]]
  trimws(parents[nzchar(parents)])
}

read_gff_records <- function(annotation_path) {
  if (!file.exists(annotation_path)) return(list())
  con <- open_text_connection(annotation_path)
  on.exit(close(con), add = TRUE)
  lines <- readLines(con, warn = FALSE)
  lines <- lines[!grepl("^\\s*#", lines) & nzchar(lines)]
  out <- vector("list", length(lines))
  out_i <- 0L
  for (line in lines) {
    x <- strsplit(line, "\t", fixed = TRUE)[[1]]
    if (length(x) < 9L) next
    attr <- parse_gff_attributes_for_gtf(x[[9]])
    out_i <- out_i + 1L
    out[[out_i]] <- list(
      fields = x,
      attrs = attr,
      feature_raw = x[[3]],
      feature = normalize_gff_feature_for_gtf(x[[3]])
    )
  }
  out[seq_len(out_i)]
}

annotation_track_qc <- function(annotation_path) {
  records <- read_gff_records(annotation_path)
  raw_features <- vapply(records, function(r) r$feature_raw, character(1))
  normalized_features <- vapply(records, function(r) r$feature, character(1))
  attrs <- lapply(records, `[[`, "attrs")
  strands <- vapply(records, function(r) r$fields[[7]] %||% ".", character(1))
  count_raw <- function(x) sum(raw_features == x, na.rm = TRUE)
  count_norm <- function(x) sum(normalized_features == x, na.rm = TRUE)
  has_id <- vapply(attrs, function(a) !is.null(a$ID) && nzchar(a$ID), logical(1))
  has_parent <- vapply(attrs, function(a) !is.null(a$Parent) && nzchar(a$Parent), logical(1))
  has_strand <- strands %in% c("+", "-")
  block_source <- if (count_norm("exon") > 0L) "exon" else if (count_norm("CDS") > 0L) "CDS" else "gene-only"
  list(
    records = records,
    metrics = list(
      format = annotation_format_from_path(annotation_path),
      total_annotated_lines = length(records),
      gene = count_norm("gene"),
      mRNA = count_raw("mRNA"),
      transcript = count_raw("transcript") + sum(raw_features != "mRNA" & normalized_features == "transcript"),
      exon = count_norm("exon"),
      CDS = count_norm("CDS"),
      UTR = count_norm("UTR"),
      records_with_ID = sum(has_id),
      records_with_Parent = sum(has_parent),
      records_with_strand = sum(has_strand),
      exon_intron_display_possible = if (block_source %in% c("exon", "CDS")) "yes" else "no",
      display_blocks_used = block_source
    )
  )
}

write_annotation_track_qc <- function(annotation_path, work_dir, qc = NULL) {
  if (is.null(work_dir) || !nzchar(work_dir)) return(invisible(NULL))
  ensure_dir(work_dir)
  qc <- qc %||% annotation_track_qc(annotation_path)
  metrics <- qc$metrics
  rows <- data.frame(
    metric = names(metrics),
    value = unlist(metrics, use.names = FALSE),
    stringsAsFactors = FALSE
  )
  out_path <- annotation_qc_path(work_dir)
  utils::write.table(rows, out_path, sep = "\t", quote = FALSE, row.names = FALSE)
  message("[TrackGene] annotation QC written: ", out_path)
  invisible(out_path)
}

#' Convert GFF/GFF3 gene features to simple gene-body GTF blocks
#'
#' @param annotation_path source GFF/GFF3 path
#' @param out_path destination GTF path
#' @return out_path invisibly
#' @export
convert_gff_gene_blocks_to_gtf <- function(annotation_path, out_path) {
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
      gene_id <- if (length(parents) > 0L) parents[[1]] else gene_id
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

convert_gff_to_gtf_for_pygenometracks <- convert_gff_gene_blocks_to_gtf

convert_gff_with_gffread <- function(annotation_path, out_path) {
  gffread <- Sys.which("gffread")
  if (!nzchar(gffread)) return(FALSE)
  res <- tryCatch(
    system2(gffread, c(annotation_path, "-T", "-o", out_path), stdout = TRUE, stderr = TRUE),
    error = function(e) character(0)
  )
  ok <- file.exists(out_path) && file.info(out_path)$size > 0
  if (isTRUE(ok)) {
    message("[TrackGene] converted GFF/GFF3 to GTF with gffread: ", out_path)
  } else {
    message("[TrackGene] gffread conversion unavailable/failed; using internal parser.")
    if (length(res) > 0L) message(paste(res, collapse = "\n"))
  }
  ok
}

convert_gff_models_to_gtf <- function(annotation_path, out_path, use_cds_as_exon_blocks = TRUE, qc = NULL) {
  if (!file.exists(annotation_path)) {
    stop(sprintf("Annotation file not found: %s", annotation_path))
  }
  ensure_dir(dirname(out_path))

  qc <- qc %||% annotation_track_qc(annotation_path)
  parsed <- qc$records
  if (length(parsed) == 0L) {
    writeLines(character(0), out_path)
    return(invisible(out_path))
  }

  gene_names <- new.env(parent = emptyenv(), hash = TRUE)
  tx_to_gene <- new.env(parent = emptyenv(), hash = TRUE)

  env_get <- function(env, key, default = NULL) {
    key <- as.character(key %||% "")
    if (nzchar(key) && exists(key, envir = env, inherits = FALSE)) get(key, envir = env) else default
  }

  for (row in parsed) {
    x <- row$fields
    attr <- row$attrs
    feature <- row$feature
    id <- attr$ID %||% attr$Name %||% paste0(x[[1]], ":", x[[4]], "-", x[[5]])
    name <- attr$Name %||% attr$gene_name %||% id
    parents <- extract_parent_ids(attr$Parent)

    if (identical(feature, "gene")) {
      gene_id <- attr$ID %||% attr$Name %||% attr$gene_id %||% attr$locus_tag %||% id
      assign(gene_id, name, envir = gene_names)
    } else if (identical(feature, "transcript")) {
      tx_id <- attr$ID %||% id
      gene_id <- if (length(parents) > 0L) parents[[1]] else tx_id
      assign(tx_id, gene_id, envir = tx_to_gene)
      if (!exists(gene_id, envir = gene_names, inherits = FALSE)) {
        assign(gene_id, name, envir = gene_names)
      }
    }
  }

  out <- vector("list", length(parsed) * 3L)
  out_i <- 0L
  feature_counts <- c(transcript = 0L, exon = 0L, CDS = 0L, UTR = 0L)
  tx_ranges <- new.env(parent = emptyenv(), hash = TRUE)
  has_source_exons <- any(vapply(parsed, function(row) identical(row$feature, "exon"), logical(1)))
  if (!has_source_exons && isTRUE(use_cds_as_exon_blocks)) {
    message("[TrackGene] Warning: No exon features detected. CDS features will be used as display blocks. UTRs will not be shown.")
  }

  update_tx_range <- function(tx_id, gene_id, gene_name, x) {
    start <- suppressWarnings(as.integer(x[[4]]))
    end <- suppressWarnings(as.integer(x[[5]]))
    if (!is.finite(start) || !is.finite(end)) return()
    current <- env_get(tx_ranges, tx_id)
    if (is.null(current)) {
      current <- list(
        chrom = x[[1]], source = x[[2]], start = start, end = end,
        score = ".", strand = x[[7]], frame = ".",
        gene_id = gene_id, tx_id = tx_id, gene_name = gene_name
      )
    } else {
      current$start <- min(current$start, start)
      current$end <- max(current$end, end)
    }
    assign(tx_id, current, envir = tx_ranges)
  }

  append_gtf_row <- function(x, feature, gene_id, tx_id, gene_name) {
    y <- x
    y[[3]] <- feature
    y[[9]] <- gtf_attribute_string(gene_id, tx_id, gene_name)
    out_i <<- out_i + 1L
    if (out_i > length(out)) length(out) <<- length(out) * 2L
    out[[out_i]] <<- paste(y[1:9], collapse = "\t")
    if (feature %in% names(feature_counts)) {
      feature_counts[[feature]] <<- feature_counts[[feature]] + 1L
    }
  }

  for (row in parsed) {
    x <- row$fields
    attr <- row$attrs
    feature <- row$feature
    if (has_source_exons) {
      if (!(feature %in% c("exon", "CDS", "UTR"))) next
    } else {
      if (!isTRUE(use_cds_as_exon_blocks) || !identical(feature, "CDS")) next
    }

    parents <- extract_parent_ids(attr$Parent)
    if (length(parents) == 0L) {
      parents <- attr$transcript_id %||% attr$ID %||% paste0(x[[1]], ":", x[[4]], "-", x[[5]])
    }

    for (tx_id in parents) {
      gene_id <- env_get(tx_to_gene, tx_id, tx_id)
      gene_name <- attr$gene_name %||% attr$Name %||% env_get(gene_names, gene_id, gene_id)
      update_tx_range(tx_id, gene_id, gene_name, x)
      emit_features <- feature
      if (!has_source_exons && isTRUE(use_cds_as_exon_blocks) && identical(feature, "CDS")) {
        emit_features <- c("exon", feature)
      }
      for (emit_feature in emit_features) {
        append_gtf_row(x, emit_feature, gene_id, tx_id, gene_name)
      }
    }
  }

  if (out_i == 0L) {
    message("[TrackGene] no transcript/exon features found; falling back to gene blocks")
    return(convert_gff_gene_blocks_to_gtf(annotation_path, out_path))
  }

  tx_keys <- ls(tx_ranges)
  tx_lines <- vapply(tx_keys, function(tx_id) {
    tx <- get(tx_id, envir = tx_ranges, inherits = FALSE)
    fields <- c(
      tx$chrom, tx$source, "transcript", as.character(tx$start), as.character(tx$end),
      tx$score, tx$strand, tx$frame,
      gtf_attribute_string(tx$gene_id, tx$tx_id, tx$gene_name)
    )
    paste(fields, collapse = "\t")
  }, character(1))
  feature_counts[["transcript"]] <- length(tx_lines)
  out <- unlist(out[seq_len(out_i)], use.names = FALSE)
  out <- c(tx_lines, out)
  writeLines(out, out_path)
  message("[TrackGene] converted GFF/GFF3 transcript/exon model to GTF: ", out_path,
          " (", length(out), " records; transcript=", feature_counts[["transcript"]],
          ", exon=", feature_counts[["exon"]],
          ", CDS=", feature_counts[["CDS"]], ", UTR=", feature_counts[["UTR"]], ")")
  invisible(out_path)
}

convert_gff_models_to_bed12 <- function(annotation_path, out_path, use_cds_as_exon_blocks = TRUE,
                                        preferred_label = "Name", qc = NULL) {
  if (!file.exists(annotation_path)) {
    stop(sprintf("Annotation file not found: %s", annotation_path))
  }
  ensure_dir(dirname(out_path))
  qc <- qc %||% annotation_track_qc(annotation_path)
  records <- qc$records
  if (length(records) == 0L) {
    writeLines(character(0), out_path)
    return(invisible(out_path))
  }

  gene_names <- new.env(parent = emptyenv(), hash = TRUE)
  gene_ids <- new.env(parent = emptyenv(), hash = TRUE)
  tx_to_gene <- new.env(parent = emptyenv(), hash = TRUE)
  tx_names <- new.env(parent = emptyenv(), hash = TRUE)
  tx_blocks <- new.env(parent = emptyenv(), hash = TRUE)
  tx_cds <- new.env(parent = emptyenv(), hash = TRUE)
  has_source_exons <- any(vapply(records, function(row) identical(row$feature, "exon"), logical(1)))
  block_feature <- if (has_source_exons) "exon" else if (isTRUE(use_cds_as_exon_blocks)) "CDS" else "none"

  if (identical(block_feature, "CDS")) {
    message("[TrackGene] Warning: No exon features detected. CDS features will be used as display blocks. UTRs will not be shown.")
  }

  env_get <- function(env, key, default = NULL) {
    key <- as.character(key %||% "")
    if (nzchar(key) && exists(key, envir = env, inherits = FALSE)) get(key, envir = env) else default
  }

  label_from_attrs <- function(attrs, fallback) {
    key <- preferred_label %||% "Name"
    val <- attrs[[key]] %||% attrs$Name %||% attrs$ID %||% attrs$gene_id %||% attrs$transcript_id %||% fallback
    trimws(as.character(val %||% fallback))
  }

  add_block <- function(tx_id, gene_id, chrom, start, end, strand, name) {
    start <- suppressWarnings(as.integer(start))
    end <- suppressWarnings(as.integer(end))
    if (!is.finite(start) || !is.finite(end)) return()
    current <- env_get(tx_blocks, tx_id, list())
    current[[length(current) + 1L]] <- list(start = start, end = end)
    assign(tx_id, current, envir = tx_blocks)
    if (is.null(env_get(tx_cds, tx_id))) {
      assign(tx_id, list(chrom = chrom, strand = strand, gene_id = gene_id, name = name,
                         thick_start = start, thick_end = end), envir = tx_cds)
    } else {
      meta <- env_get(tx_cds, tx_id)
      meta$thick_start <- min(meta$thick_start, start)
      meta$thick_end <- max(meta$thick_end, end)
      assign(tx_id, meta, envir = tx_cds)
    }
  }

  for (row in records) {
    x <- row$fields
    attr <- row$attrs
    feature <- row$feature
    id <- attr$ID %||% paste0(x[[1]], ":", x[[4]], "-", x[[5]])
    if (identical(feature, "gene")) {
      gene_id <- attr$ID %||% attr$gene_id %||% attr$Name %||% id
      assign(gene_id, label_from_attrs(attr, gene_id), envir = gene_names)
      assign(gene_id, gene_id, envir = gene_ids)
    } else if (identical(feature, "transcript")) {
      tx_id <- attr$ID %||% attr$transcript_id %||% id
      parents <- extract_parent_ids(attr$Parent)
      gene_id <- if (length(parents) > 0L) parents[[1]] else (attr$gene_id %||% tx_id)
      assign(tx_id, gene_id, envir = tx_to_gene)
      assign(tx_id, label_from_attrs(attr, tx_id), envir = tx_names)
    }
  }

  if (identical(block_feature, "none")) {
    message("[TrackGene] Only gene/transcript intervals are being plotted. Exon-intron structure will not be visible.")
    return(convert_gff_gene_blocks_to_gtf(annotation_path, sub("\\.bed12$", ".gtf", out_path)))
  }

  for (row in records) {
    if (!identical(row$feature, block_feature)) next
    x <- row$fields
    attr <- row$attrs
    parents <- extract_parent_ids(attr$Parent)
    if (length(parents) == 0L) parents <- attr$transcript_id %||% attr$ID %||% paste0(x[[1]], ":", x[[4]], "-", x[[5]])
    for (tx_id in parents) {
      gene_id <- env_get(tx_to_gene, tx_id, tx_id)
      gene_name <- env_get(gene_names, gene_id, gene_id)
      tx_name <- env_get(tx_names, tx_id, gene_name)
      name <- if ((preferred_label %||% "Name") %in% c("transcript_id", "ID")) tx_id else tx_name
      add_block(tx_id, gene_id, x[[1]], x[[4]], x[[5]], x[[7]], name)
    }
  }

  tx_ids <- ls(tx_blocks)
  out <- character(0)
  for (tx_id in tx_ids) {
    blocks <- env_get(tx_blocks, tx_id, list())
    if (length(blocks) == 0L) next
    starts <- vapply(blocks, `[[`, integer(1), "start")
    ends <- vapply(blocks, `[[`, integer(1), "end")
    ord <- order(starts, ends)
    starts <- starts[ord]
    ends <- ends[ord]
    meta <- env_get(tx_cds, tx_id)
    chrom_start <- min(starts) - 1L
    chrom_end <- max(ends)
    block_sizes <- ends - starts + 1L
    block_starts <- starts - 1L - chrom_start
    out <- c(out, paste(c(
      meta$chrom,
      chrom_start,
      chrom_end,
      meta$name %||% tx_id,
      0,
      if (meta$strand %in% c("+", "-")) meta$strand else ".",
      max(0L, meta$thick_start - 1L),
      meta$thick_end,
      "0,0,0",
      length(blocks),
      paste0(block_sizes, collapse = ","),
      paste0(block_starts, collapse = ",")
    ), collapse = "\t"))
  }
  writeLines(out, out_path)
  message("[TrackGene] converted GFF/GFF3 transcript model to BED12: ", out_path,
          " (", length(out), " transcripts; display blocks=", block_feature, ")")
  invisible(out_path)
}

annotation_model_feature_counts <- function(annotation_path, max_lines = Inf) {
  if (!file.exists(annotation_path)) return(integer())
  con <- open_text_connection(annotation_path)
  on.exit(close(con), add = TRUE)
  counts <- integer()
  read_n <- 10000L
  seen <- 0L
  repeat {
    lines <- readLines(con, n = read_n, warn = FALSE)
    if (length(lines) == 0L) break
    seen <- seen + length(lines)
    lines <- lines[!grepl("^\\s*#", lines) & nzchar(lines)]
    fields <- strsplit(lines, "\t", fixed = TRUE)
    feats <- vapply(fields, function(x) if (length(x) >= 3L) normalize_gff_feature_for_gtf(x[[3]]) else "", character(1))
    feats <- feats[nzchar(feats)]
    tab <- table(feats)
    for (nm in names(tab)) {
      old <- if (nm %in% names(counts)) counts[[nm]] else 0L
      counts[[nm]] <- old + as.integer(tab[[nm]])
    }
    if (seen >= max_lines) break
  }
  counts
}

annotation_overlaps_regions <- function(annotation_path, regions = NULL) {
  parsed_regions <- lapply(regions %||% character(0), parse_ini_region)
  parsed_regions <- Filter(Negate(is.null), parsed_regions)
  if (!file.exists(annotation_path) || length(parsed_regions) == 0L) return(NA)

  con <- open_text_connection(annotation_path)
  on.exit(close(con), add = TRUE)
  wanted <- c("gene", "transcript", "exon", "CDS", "UTR")
  repeat {
    lines <- readLines(con, n = 10000L, warn = FALSE)
    if (length(lines) == 0L) break
    lines <- lines[!grepl("^\\s*#", lines) & nzchar(lines)]
    fields <- strsplit(lines, "\t", fixed = TRUE)
    for (x in fields) {
      if (length(x) < 5L) next
      feature <- if (length(x) >= 3L) normalize_gff_feature_for_gtf(x[[3]]) else ""
      if (!feature %in% wanted) next
      chrom <- x[[1]]
      start <- suppressWarnings(as.integer(x[[4]]))
      end <- suppressWarnings(as.integer(x[[5]]))
      if (!is.finite(start) || !is.finite(end)) next
      for (region in parsed_regions) {
        if (identical(chrom, region$chrom) && start <= region$end && end >= region$start) {
          return(TRUE)
        }
      }
    }
  }
  FALSE
}

prepare_annotation_file_for_track <- function(file_path, pgt_type, work_dir = NULL,
                                              display_mode = "Gene blocks", regions = NULL,
                                              render_format = "GTF",
                                              use_cds_as_exon_blocks = TRUE,
                                              preferred_label = "Name",
                                              light_prepare = FALSE) {
  if (!identical(pgt_type, "gtf")) {
    return(list(file_path = file_path, file_type = pgt_type, qc = NULL))
  }

  fmt <- annotation_format_from_path(file_path)
  if (identical(fmt, "BED12")) {
    return(list(file_path = file_path, file_type = "bed", qc = NULL))
  }
  cached_path <- NULL
  cached_type <- pgt_type
  if (!is.null(work_dir) && nzchar(work_dir) && fmt %in% c("GFF3", "GFF", "GTF")) {
    if (identical(display_mode, "Transcript/exon model") && identical(render_format, "BED12")) {
      cached_path <- build_converted_bed12_path(file_path, work_dir)
      cached_type <- "bed"
    } else {
      cached_path <- build_converted_gtf_path(file_path, work_dir)
      cached_type <- "gtf"
    }
  }
  cache_is_current <- !is.null(cached_path) && file.exists(cached_path) &&
    file.info(cached_path)$size > 0 &&
    file.info(cached_path)$mtime >= file.info(file_path)$mtime
  if (isTRUE(cache_is_current)) {
    message("[CACHE] Annotation render file cache hit: ", cached_path)
    return(list(file_path = cached_path, file_type = cached_type, qc = NULL))
  }
  # Migrate a compatible annotation generated by an older run into the new
  # project-level cache. This avoids one last multi-minute conversion after an
  # upgrade for projects that already rendered the same annotation.
  if (!is.null(cached_path) && grepl("[/\\]cache[/\\]annotations$", work_dir)) {
    project_dir <- dirname(dirname(work_dir))
    legacy_name <- basename(cached_path)
    legacy_candidates <- list.files(
      file.path(project_dir, "runs"), recursive = TRUE, full.names = TRUE
    )
    legacy_candidates <- legacy_candidates[basename(legacy_candidates) == legacy_name]
    legacy_candidates <- legacy_candidates[
      file.exists(legacy_candidates) &
      file.info(legacy_candidates)$size > 0 &
      file.info(legacy_candidates)$mtime >= file.info(file_path)$mtime
    ]
    if (length(legacy_candidates) > 0L) {
      source_cache <- legacy_candidates[[which.max(file.info(legacy_candidates)$mtime)]]
      ensure_dir(dirname(cached_path))
      if (isTRUE(file.copy(source_cache, cached_path, overwrite = TRUE))) {
        message("[CACHE] Migrated annotation render file: ", source_cache, " -> ", cached_path)
        return(list(file_path = cached_path, file_type = cached_type, qc = NULL))
      }
    }
  }
  if (isTRUE(light_prepare)) {
    message("[PREPARE] Light annotation prepare: skipping annotation QC/conversion for ", file_path)
    return(list(file_path = file_path, file_type = pgt_type, qc = NULL))
  }
  seqnames <- detect_annotation_seqnames(file_path)
  qc <- annotation_track_qc(file_path)
  if (!is.null(work_dir) && nzchar(work_dir)) write_annotation_track_qc(file_path, work_dir, qc)
  message("[TrackGene] annotation format: ", fmt)
  message("[TrackGene] annotation file: ", file_path)
  message("[TrackGene] display mode: ", display_mode)
  message("[TrackGene] first seqnames: ", paste(seqnames, collapse = ", "))
  message("[TrackGene] feature counts: gene=", qc$metrics$gene,
          ", mRNA=", qc$metrics$mRNA,
          ", transcript=", qc$metrics$transcript,
          ", exon=", qc$metrics$exon,
          ", CDS=", qc$metrics$CDS,
          ", UTR=", qc$metrics$UTR)
  overlaps <- annotation_overlaps_regions(file_path, regions)
  if (identical(overlaps, FALSE)) {
    message("[TrackGene] Warning: no gene/transcript/exon feature overlaps the selected region(s). Signal tracks can still be rendered.")
  }

  if (fmt %in% c("GFF3", "GFF", "GTF")) {
    if (is.null(work_dir) || !nzchar(work_dir)) {
      message("[TrackGene] no work_dir provided; keeping annotation path unchanged")
      return(list(file_path = file_path, file_type = pgt_type, qc = qc))
    }
    counts <- qc$metrics
    has_exon <- as.integer(counts$exon %||% 0L) > 0L
    has_cds <- as.integer(counts$CDS %||% 0L) > 0L
    has_model <- has_exon || (has_cds && isTRUE(use_cds_as_exon_blocks))
    if (identical(display_mode, "Transcript/exon model") && isTRUE(has_model)) {
      if (identical(render_format, "BED12")) {
        out_path <- build_converted_bed12_path(file_path, work_dir)
        convert_gff_models_to_bed12(
          file_path, out_path,
          use_cds_as_exon_blocks = use_cds_as_exon_blocks,
          preferred_label = preferred_label,
          qc = qc
        )
        message("[TrackGene] converted annotation file: ", out_path)
        return(list(file_path = out_path, file_type = "bed", qc = qc))
      }
      out_path <- build_converted_gtf_path(file_path, work_dir)
      if (!convert_gff_with_gffread(file_path, out_path)) {
        convert_gff_models_to_gtf(
          file_path, out_path,
          use_cds_as_exon_blocks = use_cds_as_exon_blocks,
          qc = qc
        )
      }
    } else {
      if (identical(display_mode, "Transcript/exon model") && !isTRUE(has_model)) {
        message("[TrackGene] Le fichier ne contient pas de modèles exon/transcrit exploitables. Affichage gene blocks utilisé.")
      }
      message("[TrackGene] Only gene/transcript intervals are being plotted. Exon-intron structure will not be visible.")
      out_path <- build_converted_gtf_path(file_path, work_dir)
      convert_gff_gene_blocks_to_gtf(file_path, out_path)
    }
    message("[TrackGene] converted annotation file: ", out_path)
    return(list(file_path = out_path, file_type = "gtf", qc = qc))
  }

  list(file_path = file_path, file_type = pgt_type, qc = qc)
}

should_skip_ini_param <- function(param_name, ini_val) {
  if (param_name %in% c("file_type", "annotation_display_mode",
                        "annotation_source_format", "annotation_render_format",
                        "display_structure_from", "use_CDS_as_exon_blocks",
                        "track_group", "scale_mode", "robust_percentile")) return(TRUE)
  if (param_name == "data_range_style") return(TRUE)
  if (param_name == "orientation" && identical(tolower(ini_val), "normal")) return(TRUE)
  if (param_name %in% c("min_value", "max_value") && identical(ini_val, "auto")) return(TRUE)
  FALSE
}

parse_ini_region <- function(region) {
  m <- regexec("^([^:]+):(\\d+)-(\\d+)$", as.character(region %||% ""))
  hit <- regmatches(region, m)[[1]]
  if (length(hit) != 4L) return(NULL)
  list(chrom = hit[[2]], start = as.integer(hit[[3]]), end = as.integer(hit[[4]]))
}

read_bedgraph_values_for_regions <- function(path, regions = NULL) {
  if (!file.exists(path)) return(numeric(0))
  dat <- tryCatch(utils::read.table(path, sep = "\t", comment.char = "#",
                                    quote = "", stringsAsFactors = FALSE),
                  error = function(e) NULL)
  if (is.null(dat) || ncol(dat) < 4L || nrow(dat) == 0L) return(numeric(0))
  vals <- suppressWarnings(as.numeric(dat[[4]]))
  if (is.null(regions) || length(regions) == 0L) return(vals[is.finite(vals)])
  keep <- rep(FALSE, nrow(dat))
  for (region in regions) {
    r <- parse_ini_region(region)
    if (is.null(r)) next
    keep <- keep | (as.character(dat[[1]]) == r$chrom &
                    as.integer(dat[[2]]) < r$end &
                    as.integer(dat[[3]]) > r$start)
  }
  vals <- vals[keep]
  vals[is.finite(vals)]
}

read_bigwig_values_for_regions <- function(path, regions = NULL, bins_per_region = 10000L) {
  conda_python <- file.path(Sys.getenv("CONDA_PREFIX", unset = ""), "bin", "python")
  r_home_python <- file.path(dirname(dirname(dirname(R.home("bin")))), "bin", "python")
  rscript_python <- file.path(dirname(Sys.which("Rscript")), "python")
  python <- if (file.exists(conda_python)) conda_python
            else if (file.exists(r_home_python)) r_home_python
            else if (file.exists(rscript_python)) rscript_python
            else Sys.which("python")
  if (!nzchar(python)) return(numeric(0))
  script <- tempfile(fileext = ".py")
  regions_json <- tempfile(fileext = ".json")
  on.exit(unlink(script), add = TRUE)
  on.exit(unlink(regions_json), add = TRUE)
  writeLines(c(
    "import json, math, sys",
    "try:",
    "    import pyBigWig",
    "except Exception:",
    "    print(json.dumps([])); sys.exit(0)",
    "path = sys.argv[1]",
    "with open(sys.argv[2]) as fh:",
    "    regions = json.load(fh)",
    "bins = int(sys.argv[3])",
    "vals = []",
    "try:",
    "    bw = pyBigWig.open(path)",
    "    chroms = bw.chroms()",
    "    if not regions:",
    "        regions = [[c, 0, l] for c, l in chroms.items()]",
    "    for chrom, start, end in regions:",
    "        if chrom not in chroms:",
    "            continue",
    "        start = max(0, int(start)); end = max(start + 1, int(end))",
    "        n = max(1, min(bins, end - start))",
    "        chunk = bw.stats(chrom, start, end, nBins=n, type='mean')",
    "        vals.extend([v for v in chunk if v is not None and math.isfinite(v)])",
    "    bw.close()",
    "except Exception:",
    "    vals = []",
    "print(json.dumps(vals))"
  ), script)
  parsed_regions <- lapply(regions %||% character(0), parse_ini_region)
  parsed_regions <- Filter(Negate(is.null), parsed_regions)
  region_payload <- lapply(parsed_regions, function(r) list(r$chrom, max(0L, r$start - 1L), r$end))
  writeLines(jsonlite::toJSON(region_payload, auto_unbox = TRUE), regions_json)
  out <- tryCatch(system2(python, c(script, path, regions_json, as.character(bins_per_region)),
                         stdout = TRUE, stderr = FALSE),
                  error = function(e) character(0))
  vals <- tryCatch(jsonlite::fromJSON(paste(out, collapse = "\n")), error = function(e) numeric(0))
  vals <- as.numeric(vals)
  vals[is.finite(vals)]
}

read_bigwig_max_for_regions <- function(path, regions = NULL) {
  conda_python <- file.path(Sys.getenv("CONDA_PREFIX", unset = ""), "bin", "python")
  r_home_python <- file.path(dirname(dirname(dirname(R.home("bin")))), "bin", "python")
  rscript_python <- file.path(dirname(Sys.which("Rscript")), "python")
  python <- if (file.exists(conda_python)) conda_python
            else if (file.exists(r_home_python)) r_home_python
            else if (file.exists(rscript_python)) rscript_python
            else Sys.which("python")
  if (!nzchar(python)) return(NA_real_)
  script <- tempfile(fileext = ".py")
  regions_json <- tempfile(fileext = ".json")
  on.exit(unlink(script), add = TRUE)
  on.exit(unlink(regions_json), add = TRUE)
  writeLines(c(
    "import json, math, sys",
    "try:",
    "    import pyBigWig",
    "except Exception:",
    "    print('nan'); sys.exit(0)",
    "path = sys.argv[1]",
    "with open(sys.argv[2]) as fh:",
    "    regions = json.load(fh)",
    "vals = []",
    "try:",
    "    bw = pyBigWig.open(path)",
    "    chroms = bw.chroms()",
    "    if not regions:",
    "        regions = [[c, 0, l] for c, l in chroms.items()]",
    "    for chrom, start, end in regions:",
    "        if chrom not in chroms:",
    "            continue",
    "        start = max(0, int(start)); end = max(start + 1, int(end))",
    "        v = bw.stats(chrom, start, end, type='max', exact=True)[0]",
    "        if v is not None and math.isfinite(v):",
    "            vals.append(v)",
    "    bw.close()",
    "except Exception:",
    "    vals = []",
    "print(max(vals) if vals else 'nan')"
  ), script)
  parsed_regions <- lapply(regions %||% character(0), parse_ini_region)
  parsed_regions <- Filter(Negate(is.null), parsed_regions)
  region_payload <- lapply(parsed_regions, function(r) list(r$chrom, max(0L, r$start - 1L), r$end))
  writeLines(jsonlite::toJSON(region_payload, auto_unbox = TRUE), regions_json)
  out <- tryCatch(system2(python, c(script, path, regions_json), stdout = TRUE, stderr = FALSE),
                  error = function(e) character(0))
  val <- suppressWarnings(as.numeric(tail(out, 1)))
  if (length(val) == 0L || !is.finite(val)) NA_real_ else val
}

signal_track_values <- function(track, regions = NULL) {
  path <- track$file_path %||% ""
  type <- tolower(track$track_type %||% "")
  if (!nzchar(path)) return(numeric(0))
  if (type %in% c("bigwig", "bw")) return(read_bigwig_values_for_regions(path, regions))
  if (identical(type, "bedgraph")) return(read_bedgraph_values_for_regions(path, regions))
  numeric(0)
}

signal_track_max <- function(track, regions = NULL) {
  path <- track$file_path %||% ""
  type <- tolower(track$track_type %||% "")
  if (!nzchar(path) || !file.exists(path)) return(NA_real_)
  if (type %in% c("bigwig", "bw")) return(read_bigwig_max_for_regions(path, regions))
  vals <- signal_track_values(track, regions)
  vals <- vals[is.finite(vals)]
  if (length(vals) == 0L) NA_real_ else max(vals, na.rm = TRUE)
}

signal_stats <- function(values, percentile = 99) {
  values <- as.numeric(values)
  values <- values[is.finite(values)]
  if (length(values) == 0L) return(NULL)
  list(
    min = min(values),
    max = max(values),
    p95 = as.numeric(stats::quantile(values, 0.95, names = FALSE, na.rm = TRUE)),
    p99 = as.numeric(stats::quantile(values, 0.99, names = FALSE, na.rm = TRUE)),
    p995 = as.numeric(stats::quantile(values, 0.995, names = FALSE, na.rm = TRUE)),
    robust = as.numeric(stats::quantile(values, percentile / 100, names = FALSE, na.rm = TRUE)),
    mean = mean(values)
  )
}

format_scale_value <- function(x) {
  if (is.null(x) || length(x) == 0L || is.na(x) || !is.finite(x)) return(NULL)
  format(signif(as.numeric(x), 6), scientific = FALSE, trim = TRUE)
}

track_scale_group <- function(track) {
  params <- track$params %||% list()
  params$track_group %||% "Custom"
}

track_scale_mode <- function(track) {
  params <- track$params %||% list()
  params$scale_mode %||% "independent"
}

merge_track_schema_defaults <- function(track, schema) {
  entry <- schema$tracks[[track$track_type %||% ""]]
  if (is.null(entry)) return(track)
  defaults <- lapply(entry$params %||% list(), function(p) p$default)
  defaults <- defaults[!vapply(defaults, is.null, logical(1))]
  track$params <- utils::modifyList(defaults, track$params %||% list())
  track
}

signal_track_key <- function(track, i = NULL) {
  track$track_id %||% track$track_name %||% as.character(i %||% "")
}

signal_track_label <- function(track, i = NULL) {
  track$track_name %||% track$track_id %||% paste0("track_", i %||% "")
}

#' Compute a shared visual y-axis scale for signal tracks
#'
#' @param signal_tracks list of signal track objects
#' @param region character vector of regions, e.g. chr:start-end
#' @param mode one of auto_per_track/shared_global_max/shared_global_quantile/manual
#' @param quantile quantile for shared_global_quantile
#' @param shared_min_value common minimum for shared modes
#' @param manual_min_value manual minimum
#' @param manual_max_value manual maximum
#' @return list with scale metadata and warnings
#' @export
compute_shared_signal_scale <- function(signal_tracks, region, mode,
                                        quantile = 0.99,
                                        shared_min_value = 0,
                                        manual_min_value = 0,
                                        manual_max_value = NA_real_,
                                        avoid_clipping = AVOID_SIGNAL_CLIPPING,
                                        padding_factor = SIGNAL_MAX_PADDING_FACTOR) {
  mode <- mode %||% "auto_per_track"
  if (!mode %in% SIGNAL_SCALE_MODE) mode <- "auto_per_track"
  signal_tracks <- Filter(is_signal_track, signal_tracks %||% list())
  labels <- vapply(seq_along(signal_tracks), function(i) signal_track_label(signal_tracks[[i]], i), character(1))
  warnings <- character(0)
  padding_factor <- suppressWarnings(as.numeric(padding_factor %||% SIGNAL_MAX_PADDING_FACTOR))
  if (!is.finite(padding_factor) || padding_factor < 1) padding_factor <- 1

  if (identical(mode, "auto_per_track")) {
    if (length(signal_tracks) > 1L) {
      warnings <- c(warnings, "Attention : l'autoscaling par track peut rendre les comparaisons entre conditions trompeuses.")
    }
    return(list(
      shared_min_value = NULL,
      shared_max_value = NULL,
      scaling_mode = mode,
      quantile = quantile,
      raw_global_max = NULL,
      padding_factor = padding_factor,
      final_max_value_used = NULL,
      potential_clipping = if (length(signal_tracks) > 1L) "yes" else "unknown",
      number_of_values_used = 0L,
      tracks_used = labels,
      tracks_excluded = character(0),
      warning = warnings
    ))
  }

  if (identical(mode, "manual")) {
    min_val <- suppressWarnings(as.numeric(manual_min_value %||% 0))
    max_val <- suppressWarnings(as.numeric(manual_max_value %||% NA_real_))
    if (!is.finite(max_val)) {
      warnings <- c(warnings, "Valeur max manuelle invalide : retour à l'autoscaling par track.")
      max_val <- NULL
    }
    return(list(
      shared_min_value = if (is.finite(min_val)) min_val else 0,
      shared_max_value = max_val,
      scaling_mode = mode,
      quantile = quantile,
      raw_global_max = NULL,
      padding_factor = 1,
      final_max_value_used = max_val,
      potential_clipping = "manual",
      number_of_values_used = 0L,
      tracks_used = labels,
      tracks_excluded = character(0),
      warning = warnings
    ))
  }

  values <- numeric(0)
  raw_max_values <- numeric(0)
  used <- character(0)
  excluded <- character(0)
  for (i in seq_along(signal_tracks)) {
    tr <- signal_tracks[[i]]
    label <- signal_track_label(tr, i)
    path <- tr$file_path %||% ""
    if (!nzchar(path) || !file.exists(path)) {
      excluded <- c(excluded, label)
      warnings <- c(warnings, paste0("Signal track missing or unreadable: ", label))
      next
    }
    raw_max <- signal_track_max(tr, region)
    vals <- signal_track_values(tr, region)
    vals <- vals[is.finite(vals)]
    if (length(vals) == 0L && !is.finite(raw_max)) {
      excluded <- c(excluded, label)
      warnings <- c(warnings, paste0("No signal values found in region for track: ", label))
      next
    }
    if (length(vals) > 0L) values <- c(values, vals)
    if (is.finite(raw_max)) raw_max_values <- c(raw_max_values, raw_max)
    used <- c(used, label)
  }

  values <- values[is.finite(values)]
  raw_global_max <- if (length(raw_max_values) > 0L) max(raw_max_values, na.rm = TRUE) else if (length(values) > 0L) max(values, na.rm = TRUE) else NA_real_
  if (!is.finite(raw_global_max) || raw_global_max <= 0) {
    warnings <- c(warnings, "Aucune valeur de signal trouvée dans la région : retour à l'autoscaling.")
    return(list(
      shared_min_value = NULL,
      shared_max_value = NULL,
      scaling_mode = mode,
      quantile = quantile,
      raw_global_max = raw_global_max,
      padding_factor = padding_factor,
      final_max_value_used = NULL,
      potential_clipping = "unknown",
      number_of_values_used = 0L,
      tracks_used = used,
      tracks_excluded = excluded,
      warning = warnings
    ))
  }

  min_val <- suppressWarnings(as.numeric(shared_min_value %||% 0))
  if (!is.finite(min_val)) min_val <- 0
  if (mode %in% c("shared_global_max", "shared_global_max_padded")) {
    use_padding <- identical(mode, "shared_global_max_padded") || isTRUE(avoid_clipping)
    max_val <- raw_global_max * if (use_padding) padding_factor else 1
    if (!use_padding) {
      warnings <- c(warnings, "Le max_value est égal au max observé. Les pics peuvent toucher le plafond visuel. Ajouter un padding est recommandé.")
    }
  } else {
    q <- suppressWarnings(as.numeric(quantile %||% 0.99))
    if (!is.finite(q) || q <= 0 || q > 1) q <- 0.99
    quantile <- q
    if (length(values) == 0L) {
      warnings <- c(warnings, "Impossible de calculer le quantile : retour au maximum global avec marge.")
      max_val <- raw_global_max * padding_factor
    } else {
    max_val <- as.numeric(stats::quantile(values, probs = q, names = FALSE, na.rm = TRUE))
      if (isTRUE(avoid_clipping)) max_val <- max_val * padding_factor
      warnings <- c(warnings,
        sprintf("Les valeurs extrêmes au-dessus du quantile %.3g peuvent être visuellement tronquées.", q),
        "Le mode quantile peut tronquer visuellement les pics extrêmes. Utiliser shared_global_max_padded pour afficher tous les pics."
      )
    }
  }

  list(
    shared_min_value = min_val,
    shared_max_value = max_val,
    scaling_mode = mode,
    quantile = quantile,
    raw_global_max = raw_global_max,
    padding_factor = if (max_val > raw_global_max && mode %in% c("shared_global_max", "shared_global_max_padded", "shared_global_quantile")) padding_factor else 1,
    final_max_value_used = max_val,
    potential_clipping = if (max_val >= raw_global_max && !identical(mode, "shared_global_quantile")) "no" else "yes",
    number_of_values_used = length(values),
    tracks_used = used,
    tracks_excluded = excluded,
    warning = warnings
  )
}

#' Apply min_value/max_value generated from shared signal scaling
#'
#' @param track track list
#' @param scale_info output of compute_shared_signal_scale
#' @return updated track
#' @export
write_signal_track_with_scale <- function(track, scale_info) {
  if (!is_signal_track(track)) return(track)
  if (is.null(scale_info$shared_max_value) || !is.finite(scale_info$shared_max_value)) return(track)
  if (is.null(track$params)) track$params <- list()
  track$params$min_value <- format_scale_value(scale_info$shared_min_value %||% 0)
  track$params$max_value <- format_scale_value(scale_info$shared_max_value)
  track
}

apply_global_signal_scale <- function(tracks, regions = NULL, figure_settings = NULL) {
  fs <- figure_settings %||% list()
  apply_scale <- !is.null(figure_settings) && isTRUE(fs$apply_shared_scale_to_signal_tracks %||% TRUE)
  if (!apply_scale) {
    return(list(tracks = tracks, scale_info = NULL))
  }

  mode <- fs$signal_scale_mode %||% "shared_global_quantile"
  q <- suppressWarnings(as.numeric(fs$shared_quantile %||% 0.99))
  shared_min <- suppressWarnings(as.numeric(fs$shared_min_value %||% 0))
  manual_min <- suppressWarnings(as.numeric(fs$manual_min_value %||% 0))
  manual_max <- suppressWarnings(as.numeric(fs$manual_max_value %||% NA_real_))
  avoid_clipping <- isTRUE(fs$avoid_signal_clipping %||% AVOID_SIGNAL_CLIPPING)
  padding_factor <- suppressWarnings(as.numeric(fs$signal_max_padding_factor %||% SIGNAL_MAX_PADDING_FACTOR))
  if (identical(mode, "shared_by_modality")) {
    return(apply_signal_scale_by_modality(
      tracks, regions,
      quantile = q,
      shared_min_value = shared_min,
      avoid_clipping = avoid_clipping,
      padding_factor = padding_factor
    ))
  }
  signal_tracks <- Filter(is_signal_track, tracks)
  signal_files <- vapply(signal_tracks, function(t) t$file_path %||% "", character(1))
  signal_files <- signal_files[nzchar(signal_files)]
  cache_file <- fs$signal_scaling_cache_file %||% ""
  scale_info <- NULL
  cache_hit <- FALSE
  if (nzchar(cache_file)) {
    cache_key <- make_signal_scale_cache_key(
      signal_files, regions, mode, q, padding_factor,
      shared_min_value = shared_min,
      manual_min_value = manual_min,
      manual_max_value = manual_max
    )
    cache <- if (file.exists(cache_file)) {
      tryCatch(readRDS(cache_file), error = function(e) list())
    } else {
      list()
    }
    if (!is.null(cache[[cache_key]])) {
      scale_info <- cache[[cache_key]]
      cache_hit <- TRUE
      message("[CACHE] Signal scaling cache hit")
    } else {
      message("[CACHE] Signal scaling cache miss")
      scale_info <- compute_shared_signal_scale(
        signal_tracks, regions, mode,
        quantile = q,
        shared_min_value = shared_min,
        manual_min_value = manual_min,
        manual_max_value = manual_max,
        avoid_clipping = avoid_clipping,
        padding_factor = padding_factor
      )
      ensure_dir(dirname(cache_file))
      cache[[cache_key]] <- scale_info
      saveRDS(cache, cache_file)
    }
    scale_info$cache_hit <- cache_hit
    scale_info$cache_key <- cache_key
  } else {
    scale_info <- compute_shared_signal_scale(
      signal_tracks, regions, mode,
      quantile = q,
      shared_min_value = shared_min,
      manual_min_value = manual_min,
      manual_max_value = manual_max,
      avoid_clipping = avoid_clipping,
      padding_factor = padding_factor
    )
  }

  message("[INFO] Signal scaling mode: ", scale_info$scaling_mode)
  if (identical(scale_info$scaling_mode, "shared_global_quantile")) {
    message("[INFO] Shared quantile: ", scale_info$quantile)
  }
  if (!is.null(scale_info$raw_global_max) && is.finite(scale_info$raw_global_max)) {
    message("[INFO] Raw global max detected: ", format_scale_value(scale_info$raw_global_max))
    message("[INFO] Signal max padding factor: ", scale_info$padding_factor)
  }
  if (!is.null(scale_info$shared_max_value)) {
    message("[INFO] Shared max_value used: ", format_scale_value(scale_info$shared_max_value))
  }
  if (length(scale_info$tracks_used) > 0L) {
    message("[INFO] Tracks used for scaling: ", paste(scale_info$tracks_used, collapse = ", "))
  }
  if (length(scale_info$tracks_excluded) > 0L) {
    message("[WARN] Tracks ignored for scaling: ", paste(scale_info$tracks_excluded, collapse = ", "))
  }
  for (w in scale_info$warning %||% character(0)) message("[WARN] ", w)

  if (!is.null(scale_info$shared_max_value) && is.finite(scale_info$shared_max_value)) {
    tracks <- lapply(tracks, write_signal_track_with_scale, scale_info = scale_info)
    message("[INFO] Applied min_value=", format_scale_value(scale_info$shared_min_value %||% 0),
            " and max_value=", format_scale_value(scale_info$shared_max_value),
            " to ", length(scale_info$tracks_used), " signal tracks")
  }

  list(tracks = tracks, scale_info = scale_info)
}

apply_gene_layout_defaults <- function(tracks, figure_settings = NULL) {
  fs <- figure_settings %||% list()
  lapply(tracks, function(track) {
    if (is.null(track$params)) track$params <- list()
    if (is_signal_track(track) && !is.null(fs$signal_track_height)) {
      track$params$height <- fs$signal_track_height
    }
    if (identical(track$track_type %||% "", "bed") &&
        !is.null(fs$annotation_track_height)) {
      track$params$height <- fs$annotation_track_height
      track$params$labels <- isTRUE(fs$annotation_labels %||% FALSE)
    }
    if (!identical(track$track_type %||% "", "gtf")) return(track)
    if (!is.null(figure_settings)) {
      track$params$height <- fs$gene_track_height %||% 0.9
      track$params$fontsize <- fs$gene_label_fontsize %||% 6
      track$params$gene_rows <- fs$gene_rows %||% 0
      track$params$display <- fs$gene_display %||% "stacked"
      track$params$style <- fs$gene_style %||% "UCSC"
    }
    track$params$arrowhead_included <- track$params$arrowhead_included %||% TRUE
    track$params$arrow_interval <- track$params$arrow_interval %||% 2
    track$params$color_backbone <- track$params$color_backbone %||% "black"
    track
  })
}

apply_compact_track_heights <- function(tracks, figure_settings = NULL) {
  fs <- figure_settings %||% list()
  if (!isTRUE(fs$compact_track_layout)) return(tracks)
  n_signal <- sum(vapply(tracks, is_signal_track, logical(1)))
  signal_cap <- if (n_signal >= 12L) 0.42 else if (n_signal >= 8L) 0.52 else if (n_signal >= 5L) 0.65 else 0.85
  lapply(tracks, function(track) {
    if (is.null(track$params)) track$params <- list()
    current <- suppressWarnings(as.numeric(track$params$height %||% Inf))
    if (!is.finite(current) || current <= 0) current <- Inf
    if (is_signal_track(track)) track$params$height <- min(current, signal_cap)
    if (identical(track$track_type %||% "", "bed")) {
      track$params$height <- min(current, if (n_signal >= 8L) 0.10 else 0.15)
    }
    if (identical(track$track_type %||% "", "gtf")) {
      track$params$height <- min(current, if (n_signal >= 8L) 0.50 else 0.65)
      track$params$fontsize <- min(suppressWarnings(as.numeric(track$params$fontsize %||% 6)), 6)
    }
    track
  })
}

#' Insert a spacer track after the last signal track and before gene tracks
#'
#' @param tracks ordered list of track objects
#' @param enabled logical
#' @param height numeric spacer height
#' @return updated track list
#' @export
insert_spacer_before_genes <- function(tracks, enabled = TRUE, height = SPACER_BEFORE_GENES_HEIGHT) {
  if (!isTRUE(enabled) || length(tracks) == 0L) return(tracks)
  if (any(vapply(tracks, function(t) identical(t$track_name %||% "", "spacer_before_genes") ||
                 identical(t$track_id %||% "", "spacer_before_genes"), logical(1)))) {
    return(tracks)
  }
  signal_idx <- which(vapply(tracks, is_signal_track, logical(1)))
  gene_idx <- which(vapply(tracks, function(t) (t$track_type %||% "") %in% c("gtf", "gff", "gff3", "genes"), logical(1)))
  if (length(signal_idx) == 0L || length(gene_idx) == 0L) return(tracks)
  insert_before <- gene_idx[gene_idx > max(signal_idx)][1]
  if (is.na(insert_before)) insert_before <- gene_idx[1]
  spacer <- list(
    track_id = "spacer_before_genes",
    track_type = "spacer",
    track_name = "spacer_before_genes",
    enabled = TRUE,
    file_path = NULL,
    params = list(height = height)
  )
  message("[INFO] Inserted spacer_before_genes height=", height)
  append(tracks, list(spacer), after = insert_before - 1L)
}

insert_spacer_before_annotations <- function(tracks, height = 0.05) {
  if (length(tracks) == 0L || !is.finite(height) || height <= 0) return(tracks)
  if (any(vapply(tracks, function(t) identical(t$track_id %||% "", "spacer_before_annotations"), logical(1)))) {
    return(tracks)
  }
  signal_idx <- which(vapply(tracks, is_signal_track, logical(1)))
  annotation_idx <- which(vapply(tracks, function(t) identical(t$track_type %||% "", "bed"), logical(1)))
  if (length(signal_idx) == 0L || length(annotation_idx) == 0L) return(tracks)
  # Une annotation peut se trouver entre deux groupes de signaux (ATAC, puis
  # dACRs, puis WGBS). Insérer avant le premier BED directement précédé par
  # un signal, pas seulement après le dernier signal de toute la figure.
  insert_before <- annotation_idx[vapply(annotation_idx, function(i) {
    i > 1L && is_signal_track(tracks[[i - 1L]])
  }, logical(1))][1L]
  if (length(insert_before) == 0L || is.na(insert_before)) return(tracks)
  spacer <- list(
    track_id = "spacer_before_annotations",
    track_type = "spacer",
    track_name = "spacer_before_annotations",
    enabled = TRUE,
    file_path = NULL,
    params = list(height = height)
  )
  append(tracks, list(spacer), after = insert_before - 1L)
}

ensure_x_axis_track <- function(tracks) {
  has_axis <- any(vapply(tracks, function(track) {
    (track$track_type %||% "") %in% c("x_axis", "x-axis")
  }, logical(1)))
  if (has_axis) return(tracks)
  c(tracks, list(list(
    track_id = "automatic_x_axis",
    track_type = "x_axis",
    track_name = "Coordonnées génomiques",
    enabled = TRUE,
    file_path = NULL,
    order = length(tracks) + 1L,
    params = list(where = "bottom", fontsize = 6, title = "")
  )))
}

signal_scaling_summary_lines <- function(scale_info) {
  if (is.null(scale_info)) return("# Signal scaling summary: legacy per-track settings")
  group_lines <- if (length(scale_info$group_summaries %||% list()) > 0L) {
    vapply(names(scale_info$group_summaries), function(group) {
      info <- scale_info$group_summaries[[group]]
      sprintf("# Echelle %s: min=%s max=%s (%d tracks)", group,
              format_scale_value(info$shared_min_value),
              format_scale_value(info$shared_max_value),
              length(info$tracks_used %||% character(0)))
    }, character(1))
  } else character(0)
  c(
    "# Signal scaling summary",
    sprintf("# Mode d'echelle: %s", scale_info$scaling_mode %||% "unknown"),
    sprintf("# Raw global max detected: %s", format_scale_value(scale_info$raw_global_max) %||% "NA"),
    sprintf("# Padding factor: %s", scale_info$padding_factor %||% "NA"),
    sprintf("# Final max_value used: %s", format_scale_value(scale_info$final_max_value_used) %||% "auto"),
    sprintf("# Min value applique: %s", format_scale_value(scale_info$shared_min_value) %||% "auto"),
    sprintf("# Max value applique: %s", format_scale_value(scale_info$shared_max_value) %||% "auto"),
    sprintf("# Quantile utilise: %s", scale_info$quantile %||% "NA"),
    sprintf("# Potential clipping: %s", scale_info$potential_clipping %||% "unknown"),
    sprintf("# Nombre de tracks signal inclus: %d", length(scale_info$tracks_used %||% character(0))),
    sprintf("# Nombre de valeurs utilisees: %d", as.integer(scale_info$number_of_values_used %||% 0L)),
    sprintf("# Tracks exclus du scaling: %s", paste(scale_info$tracks_excluded %||% character(0), collapse = ", ")),
    group_lines,
    if (length(scale_info$warning %||% character(0)) > 0L) sprintf("# Warning: %s", paste(scale_info$warning, collapse = " | ")) else "# Warning: none",
    "# Note: normalisation d'affichage uniquement; les fichiers bigWig/bedGraph ne sont pas modifies."
  )
}

gene_layout_summary_lines <- function(figure_settings = NULL) {
  fs <- figure_settings %||% list()
  enabled <- isTRUE(fs$insert_spacer_before_genes %||% INSERT_SPACER_BEFORE_GENES)
  height <- fs$spacer_before_genes_height %||% SPACER_BEFORE_GENES_HEIGHT
  c(
    "# Gene track layout summary",
    sprintf("# Spacer before genes: %s", if (enabled) "yes" else "no"),
    sprintf("# Spacer height: %s", height),
    sprintf("# Gene track height: %s", fs$gene_track_height %||% "track/default"),
    sprintf("# Gene label fontsize: %s", fs$gene_label_fontsize %||% "track/default"),
    sprintf("# Gene rows: %s", fs$gene_rows %||% "track/default"),
    sprintf("# Gene style: %s", fs$gene_style %||% "track/default"),
    sprintf("# Gene display: %s", fs$gene_display %||% "track/default")
  )
}

apply_signal_scale_modes <- function(tracks, regions = NULL, stats_by_track = NULL) {
  if (length(tracks) == 0L) return(tracks)
  signal_idx <- which(vapply(tracks, function(t) is_signal_track_type(t$track_type %||% ""), logical(1)))
  if (length(signal_idx) == 0L) return(tracks)

  stats_by_track <- stats_by_track %||% list()
  for (i in signal_idx) {
    key <- tracks[[i]]$track_id %||% tracks[[i]]$track_name %||% as.character(i)
    if (is.null(stats_by_track[[key]])) {
      vals <- signal_track_values(tracks[[i]], regions)
      pct <- as.numeric((tracks[[i]]$params %||% list())$robust_percentile %||% 99)
      stats_by_track[[key]] <- signal_stats(vals, percentile = pct)
    }
  }

  group_keys <- vapply(signal_idx, function(i) {
    paste(track_scale_group(tracks[[i]]), track_scale_mode(tracks[[i]]), sep = "\r")
  }, character(1))

  for (gkey in unique(group_keys)) {
    idx <- signal_idx[group_keys == gkey]
    mode <- track_scale_mode(tracks[[idx[[1]]]])
    group <- track_scale_group(tracks[[idx[[1]]]])
    if (identical(mode, "independent")) next

    if (identical(mode, "manual")) {
      for (i in idx) {
        message("[TrackScale] Track ", tracks[[i]]$track_name %||% i, " manual min=",
                (tracks[[i]]$params %||% list())$min_value %||% "auto", " max=",
                (tracks[[i]]$params %||% list())$max_value %||% "auto")
      }
      next
    }

    keys <- vapply(idx, function(i) tracks[[i]]$track_id %||% tracks[[i]]$track_name %||% as.character(i), character(1))
    stats <- Filter(Negate(is.null), stats_by_track[keys])
    if (length(stats) == 0L) {
      message("[TrackScale] No signal statistics available for group ", group, "; keeping independent values.")
      next
    }

    if (identical(mode, "shared_by_group")) {
      group_min <- min(vapply(stats, `[[`, numeric(1), "min"), na.rm = TRUE)
      group_max <- max(vapply(stats, `[[`, numeric(1), "max"), na.rm = TRUE)
    } else if (identical(mode, "robust_shared_by_group")) {
      group_min <- 0
      group_max <- max(vapply(stats, `[[`, numeric(1), "robust"), na.rm = TRUE)
    } else {
      next
    }

    min_val <- format_scale_value(group_min)
    max_val <- format_scale_value(group_max)
    if (is.null(min_val) || is.null(max_val)) next
    for (i in idx) {
      if (is.null(tracks[[i]]$params)) tracks[[i]]$params <- list()
      tracks[[i]]$params$min_value <- min_val
      tracks[[i]]$params$max_value <- max_val
      message("[TrackScale] Track ", tracks[[i]]$track_name %||% i,
              " group=", group, " mode=", mode, " min=", min_val, " max=", max_val)
    }
  }
  tracks
}

#' Convert a track object to an INI block string
#'
#' @param track list representing a single track
#' @param schema schema list from load_track_schema
#' @param work_dir optional directory for generated intermediate files
#' @return character string for one [section] block
#' @export
track_to_ini_block <- function(track, schema, work_dir = NULL, regions = NULL,
                               figure_settings = NULL) {
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

  # Parameters: merge schema defaults with track-specific params
  param_defs  <- schema_entry$params %||% list()
  track_params <- track$params %||% list()
  if (is.null(track_params$title) || !nzchar(trimws(as.character(track_params$title %||% "")))) {
    track_params$title <- safe_name
  }

  # file line first (required for most types)
  file_path <- track$file_path %||% ""
  annotation_qc <- NULL
  if (isTRUE(schema_entry$file_required) && nchar(file_path) > 0) {
    render_format <- track_params$annotation_render_format %||% "GTF"
    display_structure <- track_params$display_structure_from %||% "exon"
    use_cds_blocks <- isTRUE(track_params$use_CDS_as_exon_blocks) || identical(display_structure, "CDS")
    annotation_work_dir <- figure_settings$annotation_cache_dir %||% work_dir
    prepared <- prepare_annotation_file_for_track(
      file_path, pgt_type, annotation_work_dir,
      display_mode = track_params$annotation_display_mode %||% "Gene blocks",
      regions = regions,
      render_format = render_format,
      use_cds_as_exon_blocks = use_cds_blocks,
      preferred_label = track_params$prefered_name %||% "Name",
      light_prepare = isTRUE((figure_settings %||% list())$light_prepare)
    )
    file_path <- prepared$file_path %||% file_path
    pgt_type <- prepared$file_type %||% pgt_type
    annotation_qc <- prepared$qc
    lines <- c(lines, sprintf("file = %s", file_path))
  }

  if (!is.null(pgt_type) && nchar(pgt_type) > 0) {
    lines <- c(lines, sprintf("file_type = %s", pgt_type))
  }

  if (!is.null(annotation_qc)) {
    metrics <- annotation_qc$metrics
    lines <- c(lines,
      sprintf("# annotation_format = %s", metrics$format %||% "UNKNOWN"),
      sprintf("# annotation_genes = %s", metrics$gene %||% 0L),
      sprintf("# annotation_mRNA = %s", metrics$mRNA %||% 0L),
      sprintf("# annotation_transcripts = %s", metrics$transcript %||% 0L),
      sprintf("# annotation_exons = %s", metrics$exon %||% 0L),
      sprintf("# annotation_CDS = %s", metrics$CDS %||% 0L),
      sprintf("# exon_intron_display_possible = %s", metrics$exon_intron_display_possible %||% "no"),
      sprintf("# display_blocks_used = %s", metrics$display_blocks_used %||% "gene-only")
    )
  }

  if (is_signal_track_type(track_type)) {
    lines <- c(lines,
      sprintf("# track_group = %s", track_params$track_group %||% "Custom"),
      sprintf("# scale_mode = %s", track_params$scale_mode %||% "independent")
    )
  }

  for (param_name in names(param_defs)) {
    # Use track-specific value if set, else schema default
    value <- track_params[[param_name]]
    if (is.null(value)) value <- param_defs[[param_name]]$default
    if (is.null(value)) next

    ini_val <- format_ini_value(value)
    if (is.null(ini_val) || nchar(trimws(ini_val)) == 0) next

    if (should_skip_ini_param(param_name, ini_val)) next

    # In the UI, gene_rows = 0 means "automatic". pyGenomeTracks does not
    # assign that meaning to zero: it treats it as a hard maximum of zero rows
    # and consequently skips every BED feature. Omitting the option restores
    # pyGenomeTracks' automatic row allocation (gene_rows = None).
    if (identical(pgt_type, "bed") && identical(param_name, "gene_rows") &&
        isTRUE(suppressWarnings(as.numeric(ini_val)) == 0)) next

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
#' @param regions optional regions used for per-region signal scale statistics
#' @param stats_by_track optional precomputed signal stats for tests/workflows
#' @return character string with full INI content
#' @export
generate_tracks_ini <- function(tracks, schema, work_dir = NULL, regions = NULL,
                                stats_by_track = NULL, figure_settings = NULL) {
  if (length(tracks) == 0) stop("No tracks provided for INI generation")

  # Sort by order field if present
  orders <- vapply(tracks, function(t) as.integer(t$order %||% 999L), integer(1))
  tracks <- tracks[order(orders)]

  # Filter to enabled tracks only
  tracks <- Filter(function(t) isTRUE(t$enabled), tracks)
  if (length(tracks) == 0) stop("No enabled tracks available for INI generation")
  tracks <- lapply(tracks, merge_track_schema_defaults, schema = schema)
  tracks <- assign_default_track_colors(tracks, schema)
  tracks <- apply_gene_layout_defaults(tracks, figure_settings)
  tracks <- apply_compact_track_heights(tracks, figure_settings)
  fs <- figure_settings %||% list()
  # Le mode léger sert aux aperçus et à la préparation. Il doit être
  # testé avant apply_global_signal_scale(), car cette fonction ouvre chaque
  # BigWig et constituait précisément le blocage de plusieurs dizaines de
  # secondes que le mode léger devait éviter.
  global_scale <- if (isTRUE(fs$light_prepare)) {
    list(tracks = tracks, scale_info = NULL)
  } else {
    apply_global_signal_scale(tracks, regions = regions, figure_settings = figure_settings)
  }
  tracks <- global_scale$tracks
  # Preparing a run only needs a syntactically valid template. Reading every
  # BigWig here blocks Shiny for several seconds and is redundant because the
  # final configuration computes (and caches) the scale at launch time.
  if (is.null(global_scale$scale_info) && !isTRUE(fs$light_prepare)) {
    tracks <- apply_signal_scale_modes(tracks, regions = regions, stats_by_track = stats_by_track)
  } else if (isTRUE(fs$light_prepare)) {
    message("[PREPARE] Light mode: signal statistics deferred until launch")
  }
  tracks <- insert_spacer_before_annotations(tracks, height = 0.05)
  spacer_enabled <- isTRUE(fs$insert_spacer_before_genes %||% INSERT_SPACER_BEFORE_GENES)
  spacer_height <- suppressWarnings(as.numeric(fs$spacer_before_genes_height %||% SPACER_BEFORE_GENES_HEIGHT))
  if (!is.finite(spacer_height)) spacer_height <- SPACER_BEFORE_GENES_HEIGHT
  tracks <- insert_spacer_before_genes(tracks, enabled = spacer_enabled, height = spacer_height)
  tracks <- ensure_x_axis_track(tracks)

  blocks <- vapply(tracks, function(t) {
    tryCatch(track_to_ini_block(t, schema, work_dir = work_dir, regions = regions,
                                figure_settings = figure_settings), error = function(e) {
      warning(sprintf("Skipping track '%s': %s", t$track_name %||% "?", e$message))
      ""
    })
  }, character(1))

  header <- paste0(
    "# tracks.ini — generated by rGenomeTrackUI\n",
    "# Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n",
    "# DO NOT EDIT while a run is in progress\n",
    paste(signal_scaling_summary_lines(global_scale$scale_info), collapse = "\n"), "\n",
    paste(gene_layout_summary_lines(figure_settings), collapse = "\n"), "\n",
    "# Pour comparer plusieurs conditions, une echelle Y commune est recommandee.\n",
    "# L'autoscaling independant peut masquer une perte d'accessibilite ou une fermeture chromatinienne.\n\n"
  )

  paste0(header, paste(blocks, collapse = "\n"))
}

#' Write a tracks.ini file to disk
#'
#' @param tracks list of track objects
#' @param schema schema list
#' @param path destination file path
#' @param regions optional regions used for per-region signal scale statistics
#' @return invisible path
#' @export
write_tracks_ini <- function(tracks, schema, path, regions = NULL, figure_settings = NULL) {
  ensure_dir(dirname(path))
  ini_text <- generate_tracks_ini(tracks, schema, work_dir = dirname(path), regions = regions,
                                  figure_settings = figure_settings)
  writeLines(ini_text, path)
  invisible(path)
}
