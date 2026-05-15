# =============================================================================
# genome_index.R — Chromosome/contig index for guided region selection
# =============================================================================

# Null-coalescing operator (standalone for isolated sourcing)
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b
}

# -----------------------------------------------------------------------------
# Internal helpers: BigWig binary header reader
# -----------------------------------------------------------------------------

.bw_read_u16 <- function(con, endian) {
  readBin(con, "integer", 1L, size = 2L, signed = FALSE, endian = endian)
}
.bw_read_u32 <- function(con, endian) {
  # signed=TRUE: R only supports signed=FALSE for sizes 1-2; chromosome sizes
  # are always < 2^31 (2.1 Gbp) so the signed interpretation is identical.
  readBin(con, "integer", 1L, size = 4L, signed = TRUE, endian = endian)
}
.bw_read_offset <- function(con, endian) {
  # uint64 as two int32 (safe: file offsets < 2^31 for any chromosome tree)
  lo <- readBin(con, "integer", 1L, size = 4L, signed = TRUE, endian = endian)
  hi <- readBin(con, "integer", 1L, size = 4L, signed = TRUE, endian = endian)
  if (endian == "little") as.numeric(lo) + as.numeric(hi) * 4294967296
  else                    as.numeric(hi) + as.numeric(lo) * 4294967296
}

# Read the B+ tree of chromosomes starting at `node_offset`.
# Returns a list of lists(chrom, length).
.bw_read_chrom_tree_node <- function(con, node_offset, key_size, endian) {
  seek(con, node_offset, origin = "start")
  is_leaf <- readBin(con, "integer", 1L, size = 1L, signed = FALSE)
  readBin(con, "raw", 1L)  # reserved
  count   <- .bw_read_u16(con, endian)
  results <- list()

  if (is_leaf == 1L) {
    for (i in seq_len(count)) {
      key_raw  <- readBin(con, "raw", key_size)
      null_pos <- which(key_raw == as.raw(0x00))
      key_bytes <- if (length(null_pos) > 0) key_raw[seq_len(null_pos[1] - 1L)] else key_raw
      chrom_name <- rawToChar(key_bytes)
      chrom_id   <- .bw_read_u32(con, endian)   # noqa: stored but unused
      chrom_size <- .bw_read_u32(con, endian)
      results[[length(results) + 1L]] <- list(chrom = chrom_name, length = chrom_size)
    }
  } else {
    # Internal node: collect child offsets, then recurse
    child_offsets <- numeric(count)
    for (i in seq_len(count)) {
      readBin(con, "raw", key_size)  # skip key
      child_offsets[i] <- .bw_read_offset(con, endian)
    }
    for (co in child_offsets) {
      results <- c(results, .bw_read_chrom_tree_node(con, co, key_size, endian))
    }
  }
  results
}

# -----------------------------------------------------------------------------
# 1. inspect_bigwig_chroms
# -----------------------------------------------------------------------------

#' Read chromosome names and sizes from a BigWig binary header
#'
#' Parses the B+ tree in the BigWig header without loading any signal data.
#' Works for any valid LE or BE BigWig file < 4 GB.
#'
#' @param file_path path to a BigWig (.bw / .bigwig) file
#' @return data.frame(chrom, length, source_file, source_type, length_source)
#'   or NULL on error
#' @export
inspect_bigwig_chroms <- function(file_path) {
  tryCatch({
    con <- file(file_path, "rb")
    on.exit(close(con), add = TRUE)

    magic_raw <- readBin(con, "raw", 4L)
    hex <- paste(as.character(magic_raw), collapse = "")
    endian <- if (hex == "26fc8f88") "little" else if (hex == "888ffc26") "big" else
      stop("Not a BigWig file")

    readBin(con, "raw", 4L)   # version (2) + zoomLevels (2)
    chrom_tree_offset <- .bw_read_offset(con, endian)

    # Navigate to bptFile header (32 bytes)
    seek(con, chrom_tree_offset, origin = "start")
    readBin(con, "raw", 4L)   # bpt magic
    readBin(con, "raw", 4L)   # blockSize
    key_size <- .bw_read_u32(con, endian)
    readBin(con, "raw", 4L)   # valSize
    readBin(con, "raw", 16L)  # itemCount (8) + reserved (8)

    root_offset <- chrom_tree_offset + 32L  # bptFile header = 32 bytes
    chroms <- .bw_read_chrom_tree_node(con, root_offset, key_size, endian)

    if (length(chroms) == 0L) return(NULL)
    data.frame(
      chrom         = vapply(chroms, `[[`, character(1L), "chrom"),
      min_start     = NA_integer_,
      length        = vapply(chroms, `[[`, numeric(1L),   "length"),
      length_source = "bigwig_header",
      n_features    = NA_integer_,
      source_file   = file_path,
      source_type   = "bigwig",
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    warning(sprintf("[genome_index] inspect_bigwig_chroms(%s): %s",
                    basename(file_path), conditionMessage(e)))
    NULL
  })
}

# -----------------------------------------------------------------------------
# 2. inspect_gff_chroms
# -----------------------------------------------------------------------------

#' Extract chromosome extents from a GFF/GFF3/GTF file
#'
#' Tries to use ##sequence-region directives first; falls back to max(end)
#' over all feature lines. Reads up to max_lines data lines.
#'
#' @param file_path path to a GFF/GFF3/GTF file
#' @param max_lines maximum feature lines to scan (default 500000)
#' @return data.frame(chrom, min_start, length, length_source, n_features,
#'   source_file, source_type)  or NULL on error
#' @export
inspect_gff_chroms <- function(file_path, max_lines = 500000L) {
  tryCatch({
    con <- file(file_path, "r")
    on.exit(close(con), add = TRUE)

    seq_region <- list()   # from ##sequence-region directives
    feat_info  <- list()   # chrom -> c(min_start, max_end, n)
    n_feat     <- 0L

    repeat {
      line <- readLines(con, n = 1L, warn = FALSE)
      if (length(line) == 0L) break
      line <- trimws(line)
      if (nchar(line) == 0L) next

      # ##sequence-region  chrom  start  end
      if (startsWith(line, "##sequence-region")) {
        parts <- strsplit(trimws(sub("^##sequence-region\\s*", "", line)), "\\s+")[[1]]
        if (length(parts) >= 3L) {
          s <- suppressWarnings(as.integer(parts[2]))
          e <- suppressWarnings(as.integer(parts[3]))
          if (!is.na(s) && !is.na(e))
            seq_region[[parts[1]]] <- list(start = s, end = e)
        }
        next
      }

      if (startsWith(line, "#")) next    # other comment lines
      if (n_feat >= max_lines) next
      n_feat <- n_feat + 1L

      cols <- strsplit(line, "\t")[[1]]
      if (length(cols) < 5L) next

      chrom <- cols[1]
      s <- suppressWarnings(as.integer(cols[4]))
      e <- suppressWarnings(as.integer(cols[5]))
      if (is.na(s) || is.na(e)) next

      fi <- feat_info[[chrom]]
      if (is.null(fi)) {
        feat_info[[chrom]] <- c(s, e, 1L)
      } else {
        feat_info[[chrom]] <- c(min(fi[1], s), max(fi[2], e), fi[3] + 1L)
      }
    }

    all_chroms <- union(names(seq_region), names(feat_info))
    if (length(all_chroms) == 0L) return(NULL)

    rows <- lapply(all_chroms, function(ch) {
      sr  <- seq_region[[ch]]
      fi  <- feat_info[[ch]]
      if (!is.null(sr)) {
        list(chrom = ch, min_start = sr$start, length = sr$end,
             length_source = "gff3_sequence_region",
             n_features = if (!is.null(fi)) fi[3] else 0L)
      } else {
        list(chrom = ch, min_start = fi[1], length = fi[2],
             length_source = "inferred_from_features",
             n_features = fi[3])
      }
    })

    data.frame(
      chrom         = vapply(rows, `[[`, character(1L), "chrom"),
      min_start     = vapply(rows, `[[`, integer(1L),   "min_start"),
      length        = vapply(rows, `[[`, integer(1L),   "length"),
      length_source = vapply(rows, `[[`, character(1L), "length_source"),
      n_features    = vapply(rows, `[[`, integer(1L),   "n_features"),
      source_file   = file_path,
      source_type   = "gff",
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    warning(sprintf("[genome_index] inspect_gff_chroms(%s): %s",
                    basename(file_path), conditionMessage(e)))
    NULL
  })
}

# -----------------------------------------------------------------------------
# 3. inspect_text_track_chroms
# -----------------------------------------------------------------------------

#' Extract chromosome names from text-based track files (BED, BedGraph, etc.)
#'
#' Reads up to max_lines data lines to get distinct chromosomes + extent.
#'
#' @param file_path file path
#' @param file_type optional file type hint ("bed", "bedgraph", "narrowPeak", etc.)
#' @param max_lines maximum data lines to scan
#' @return data.frame(chrom, min_start, length, n_features, source_file, source_type)
#'   or NULL on error
#' @export
inspect_text_track_chroms <- function(file_path, file_type = NULL, max_lines = 100000L) {
  tryCatch({
    con <- file(file_path, "r")
    on.exit(close(con), add = TRUE)

    feat_info <- list()
    n_feat    <- 0L

    repeat {
      line <- readLines(con, n = 1L, warn = FALSE)
      if (length(line) == 0L) break
      line <- trimws(line)
      if (nchar(line) == 0L || grepl("^#|^track|^browser", line)) next
      if (n_feat >= max_lines) next
      n_feat <- n_feat + 1L

      cols <- strsplit(line, "\t")[[1]]
      if (length(cols) < 3L) next

      chrom <- cols[1]
      s <- suppressWarnings(as.integer(cols[2]))
      e <- suppressWarnings(as.integer(cols[3]))
      if (is.na(s) || is.na(e)) next

      fi <- feat_info[[chrom]]
      if (is.null(fi)) {
        feat_info[[chrom]] <- c(s, e, 1L)
      } else {
        feat_info[[chrom]] <- c(min(fi[1], s), max(fi[2], e), fi[3] + 1L)
      }
    }

    if (length(feat_info) == 0L) return(NULL)
    src_type <- if (!is.null(file_type)) file_type else "text_track"

    data.frame(
      chrom         = names(feat_info),
      min_start     = vapply(feat_info, `[`, integer(1L), 1L),
      length        = vapply(feat_info, `[`, integer(1L), 2L),
      length_source = "inferred_from_features",
      n_features    = vapply(feat_info, `[`, integer(1L), 3L),
      source_file   = file_path,
      source_type   = src_type,
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    warning(sprintf("[genome_index] inspect_text_track_chroms(%s): %s",
                    basename(file_path), conditionMessage(e)))
    NULL
  })
}

# -----------------------------------------------------------------------------
# 4. inspect_file_chroms (dispatcher)
# -----------------------------------------------------------------------------

#' Dispatch chromosome inspection based on file type
#'
#' @param file_path path to the file
#' @param file_type detected track type (e.g. "bigwig", "gff", "bed", etc.)
#' @return data.frame or NULL
#' @export
inspect_file_chroms <- function(file_path, file_type = NULL) {
  if (is.null(file_path) || !file.exists(file_path)) return(NULL)
  ft <- tolower(file_type %||% "")

  if (ft %in% c("bigwig"))                         return(inspect_bigwig_chroms(file_path))
  if (ft %in% c("gtf", "gff", "gff3"))             return(inspect_gff_chroms(file_path))
  if (ft %in% c("bed", "bedgraph", "narrowpeak",
                "links", "domains"))               return(inspect_text_track_chroms(file_path, ft))

  # Guess from extension
  ext <- tolower(tools::file_ext(file_path))
  if (ext %in% c("bw", "bigwig"))                  return(inspect_bigwig_chroms(file_path))
  if (ext %in% c("gff", "gff3", "gtf"))            return(inspect_gff_chroms(file_path))
  if (ext %in% c("bed", "bedgraph", "bg",
                 "narrowpeak", "bedpe", "links"))  return(inspect_text_track_chroms(file_path, ext))
  NULL
}

# -----------------------------------------------------------------------------
# 5. build_project_chrom_index
# -----------------------------------------------------------------------------

#' Build a unified chromosome index for a project
#'
#' @param project_config project config list (must have $project_path)
#' @param file_registry data.frame from load_file_registry()
#' @param active_file_ids character vector of file_ids to restrict to (NULL = all)
#' @return list with:
#'   $index  — data.frame(chrom, length, length_source, n_files, source_types,
#'               in_bigwig, in_gff, in_text, files)
#'   $compat — "ok" | "warning" | "error": cross-file compatibility
#'   $compat_msg — character explanation
#'   $per_file — list of per-file data.frames
#' @export
build_project_chrom_index <- function(project_config, file_registry = NULL,
                                      active_file_ids = NULL) {
  if (is.null(file_registry) || nrow(file_registry) == 0L) {
    return(list(index = NULL, compat = "error",
                compat_msg = "Aucun fichier dans le registre.", per_file = list()))
  }

  reg <- file_registry
  if (!is.null(active_file_ids)) {
    reg <- reg[reg$file_id %in% active_file_ids, , drop = FALSE]
    if (nrow(reg) == 0L) {
      return(list(index = NULL, compat = "error",
                  compat_msg = "Aucun fichier actif dans le registre.", per_file = list()))
    }
  }

  # Filter to files that have a tracks type and exist on disk
  indexable_types <- c("bigwig", "gtf", "gff", "gff3", "bed", "bedgraph",
                       "narrowPeak", "links", "domains")
  reg <- reg[tolower(reg$file_type_detected %||% "") %in% tolower(indexable_types), , drop = FALSE]
  if (nrow(reg) == 0L) {
    return(list(index = NULL, compat = "warning",
                compat_msg = "Aucun fichier indexable (BigWig, GFF, BED…) trouvé.", per_file = list()))
  }

  # Inspect each file
  per_file <- list()
  message("[genome_index] Analysing ", nrow(reg), " file(s)...")

  for (i in seq_len(nrow(reg))) {
    row   <- reg[i, ]
    fpath <- row$stored_path %||% ""
    ftype <- row$file_type_detected %||% ""
    fid   <- row$file_id %||% paste0("f", i)
    if (!file.exists(fpath)) next

    message(sprintf("[genome_index]   %s  %s", fid, basename(fpath)))
    df <- inspect_file_chroms(fpath, ftype)
    if (!is.null(df) && nrow(df) > 0L) per_file[[fid]] <- df
  }

  if (length(per_file) == 0L) {
    return(list(index = NULL, compat = "error",
                compat_msg = "Impossible d'extraire les chromosomes des fichiers.", per_file = list()))
  }

  # Merge all per-file info into a unified index
  all_dfs <- do.call(rbind, per_file)
  chroms  <- unique(all_dfs$chrom)

  rows <- lapply(chroms, function(ch) {
    rows_ch <- all_dfs[all_dfs$chrom == ch, , drop = FALSE]
    # Best length: prefer bigwig_header > gff3_sequence_region > inferred
    priority <- c("bigwig_header" = 1, "gff3_sequence_region" = 2,
                  "inferred_from_features" = 3)
    rows_ch$prio <- priority[rows_ch$length_source]
    rows_ch$prio[is.na(rows_ch$prio)] <- 4L
    best <- rows_ch[order(rows_ch$prio), ][1L, ]

    list(
      chrom        = ch,
      length       = best$length,
      length_source = best$length_source,
      n_files      = nrow(rows_ch),
      source_types = paste(sort(unique(rows_ch$source_type)), collapse = "+"),
      files        = paste(basename(rows_ch$source_file), collapse = ", "),
      in_bigwig    = any(rows_ch$source_type == "bigwig"),
      in_gff       = any(rows_ch$source_type %in% c("gff", "gtf")),
      in_text      = any(rows_ch$source_type %in%
                           c("bed", "bedgraph", "narrowpeak", "narrowPeak", "links"))
    )
  })

  idx <- data.frame(
    chrom        = vapply(rows, `[[`, character(1L), "chrom"),
    length       = vapply(rows, `[[`, numeric(1L),   "length"),
    length_source = vapply(rows, `[[`, character(1L), "length_source"),
    n_files      = vapply(rows, `[[`, integer(1L),   "n_files"),
    source_types = vapply(rows, `[[`, character(1L), "source_types"),
    files        = vapply(rows, `[[`, character(1L), "files"),
    in_bigwig    = vapply(rows, `[[`, logical(1L),   "in_bigwig"),
    in_gff       = vapply(rows, `[[`, logical(1L),   "in_gff"),
    in_text      = vapply(rows, `[[`, logical(1L),   "in_text"),
    stringsAsFactors = FALSE
  )

  # Compatibility check: are all files using the same chromosome names?
  file_chroms <- lapply(per_file, function(d) d$chrom)
  if (length(file_chroms) >= 2L) {
    common <- Reduce(intersect, file_chroms)
    if (length(common) == 0L) {
      compat <- "error"
      compat_msg <- paste0(
        "Aucun chromosome commun entre les fichiers. ",
        "Les figures seront vides. Vérifiez les noms de chromosomes : ",
        paste(vapply(file_chroms, paste, character(1L), collapse = ", "), collapse = " | ")
      )
    } else if (length(common) < max(vapply(file_chroms, length, integer(1L)))) {
      compat <- "warning"
      compat_msg <- sprintf(
        "%d chromosome(s) commun(s) sur %d. Certains fichiers utilisent des noms différents.",
        length(common), nrow(idx)
      )
    } else {
      compat <- "ok"
      compat_msg <- sprintf("%d chromosome(s) cohérents entre tous les fichiers.", nrow(idx))
    }
  } else {
    compat <- "ok"
    compat_msg <- sprintf("%d chromosome(s) détecté(s).", nrow(idx))
  }

  list(index = idx, compat = compat, compat_msg = compat_msg, per_file = per_file)
}

# -----------------------------------------------------------------------------
# 6. Cache management
# -----------------------------------------------------------------------------

#' Save chromosome index cache to project metadata
#'
#' @param index_result output of build_project_chrom_index()
#' @param file_registry data.frame of registry entries that were indexed
#' @param project_config project config list
#' @export
save_chrom_index_cache <- function(index_result, file_registry, project_config) {
  tryCatch({
    cache_dir  <- file.path(project_config$project_path, "metadata")
    cache_path <- file.path(cache_dir, "chrom_index.json")
    if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

    # Record mtimes of indexed files for cache invalidation
    mtimes <- list()
    if (!is.null(file_registry) && nrow(file_registry) > 0L) {
      for (i in seq_len(nrow(file_registry))) {
        fpath <- file_registry$stored_path[i]
        if (file.exists(fpath)) {
          mtimes[[file_registry$file_id[i]]] <- as.numeric(file.info(fpath)$mtime)
        }
      }
    }

    cache_obj <- list(
      generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      file_mtimes  = mtimes,
      index        = index_result$index,
      compat       = index_result$compat,
      compat_msg   = index_result$compat_msg
    )
    jsonlite::write_json(cache_obj, cache_path, auto_unbox = TRUE, pretty = TRUE)
    message(sprintf("[genome_index] Cache saved to %s", cache_path))
    invisible(cache_path)
  }, error = function(e) {
    warning(sprintf("[genome_index] Could not save cache: %s", conditionMessage(e)))
    invisible(NULL)
  })
}

#' Load chromosome index from cache
#'
#' @param project_config project config list
#' @return cache list or NULL
#' @export
load_chrom_index_cache <- function(project_config) {
  cache_path <- file.path(project_config$project_path, "metadata", "chrom_index.json")
  if (!file.exists(cache_path)) return(NULL)
  tryCatch(
    jsonlite::read_json(cache_path, simplifyVector = TRUE),
    error = function(e) NULL
  )
}

#' Check whether the cached chromosome index is still valid
#'
#' @param cache result of load_chrom_index_cache()
#' @param file_registry current file_registry data.frame
#' @return TRUE if cache is valid (no file has changed), FALSE otherwise
#' @export
is_chrom_index_cache_valid <- function(cache, file_registry) {
  if (is.null(cache) || is.null(cache$file_mtimes)) return(FALSE)
  if (is.null(file_registry) || nrow(file_registry) == 0L) return(FALSE)
  for (i in seq_len(nrow(file_registry))) {
    fid   <- file_registry$file_id[i]
    fpath <- file_registry$stored_path[i]
    if (!file.exists(fpath)) return(FALSE)
    cached_mtime <- cache$file_mtimes[[fid]]
    if (is.null(cached_mtime)) return(FALSE)
    if (abs(as.numeric(file.info(fpath)$mtime) - as.numeric(cached_mtime)) > 1) return(FALSE)
  }
  TRUE
}

# -----------------------------------------------------------------------------
# 7. validate_region_against_index
# -----------------------------------------------------------------------------

#' Validate a genomic region string against the chromosome index
#'
#' @param region_str character string "chrom:start-end"
#' @param chrom_index data.frame from build_project_chrom_index()$index
#' @param active_only if TRUE, only warn about chroms present in index but
#'   not in all active files (uses in_bigwig logic)
#' @return list(status="ok"|"warning"|"error", messages=character vector)
#' @export
validate_region_against_index <- function(region_str, chrom_index, active_only = FALSE) {
  parsed <- parse_region(region_str)
  if (is.null(parsed)) {
    return(list(status = "error",
                messages = sprintf("Format invalide : '%s'. Attendu : chrom:début-fin", region_str)))
  }
  if (parsed$start < 1L) {
    return(list(status = "error", messages = "Le début doit être ≥ 1."))
  }
  if (parsed$start >= parsed$end) {
    return(list(status = "error", messages = "Le début doit être < la fin."))
  }

  msgs <- character(0)

  if (!is.null(chrom_index) && nrow(chrom_index) > 0L) {
    row <- chrom_index[chrom_index$chrom == parsed$chrom, , drop = FALSE]
    if (nrow(row) == 0L) {
      avail <- paste(head(chrom_index$chrom, 5L), collapse = ", ")
      return(list(
        status   = "error",
        messages = sprintf(
          "Le chromosome '%s' n'existe pas dans les fichiers actifs. Disponibles : %s%s",
          parsed$chrom, avail, if (nrow(chrom_index) > 5L) "…" else ""
        )
      ))
    }

    chrom_len <- as.integer(row$length[1L])
    if (!is.na(chrom_len) && chrom_len > 0L && parsed$end > chrom_len) {
      msgs <- c(msgs, sprintf(
        "La fin (%s) dépasse la taille connue du chromosome %s (%s bp).",
        format(parsed$end, big.mark = " "),
        parsed$chrom,
        format(chrom_len, big.mark = " ")
      ))
    }

    # Warn if chrom is only present in annotation, not in BigWig
    if (isTRUE(row$in_gff[1L]) && !isTRUE(row$in_bigwig[1L])) {
      msgs <- c(msgs, sprintf(
        "Avertissement : '%s' est présent dans le GFF/GTF mais absent des BigWig.",
        parsed$chrom
      ))
    }

    status <- if (length(msgs) == 0L) "ok" else "warning"
    return(list(status = status, messages = msgs))
  }

  # No index: just format check
  list(status = "ok", messages = character(0))
}

# -----------------------------------------------------------------------------
# 8. inspect_gff_genes (gene picker support)
# -----------------------------------------------------------------------------

#' Build a complete gene/feature index from a GFF/GFF3/GTF file
#'
#' Parses the entire file without any gene count limit. Extracts all features
#' matching feature_types, with ID, Name, locus_tag (GFF3) or gene_id,
#' gene_name (GTF). Returns a data.frame suitable for server-side selectize.
#'
#' @param file_path path to the GFF/GFF3/GTF file
#' @param feature_types character vector of feature types to include
#' @return data.frame(gene_id, name, locus_tag, chrom, start, end, strand,
#'   searchable_label, source_file) sorted by chrom+start, or NULL on error
#' @export
inspect_gff_genes <- function(file_path,
                              feature_types = c("gene", "mRNA", "transcript", "pseudogene")) {
  tryCatch({
    con  <- file(file_path, "r")
    on.exit(close(con), add = TRUE)
    rows <- list()

    repeat {
      line <- readLines(con, n = 1L, warn = FALSE)
      if (length(line) == 0L) break
      if (startsWith(line, "#") || nchar(trimws(line)) == 0L) next

      cols <- strsplit(line, "\t")[[1]]
      if (length(cols) < 9L) next

      feat <- cols[3]
      if (!feat %in% feature_types) next

      chrom  <- cols[1]
      s      <- suppressWarnings(as.integer(cols[4]))
      e      <- suppressWarnings(as.integer(cols[5]))
      strand <- if (length(cols) >= 7L && nchar(cols[7]) == 1L &&
                    cols[7] %in% c("+", "-", ".")) cols[7] else "."
      if (is.na(s) || is.na(e)) next

      attr_str <- cols[9]

      # GFF3 attributes: ID=, Name=, locus_tag=
      id_m  <- regmatches(attr_str, regexpr("(?i)\\bID=([^;]+)",        attr_str, perl = TRUE))
      nm_m  <- regmatches(attr_str, regexpr("(?i)\\bName=([^;]+)",      attr_str, perl = TRUE))
      lt_m  <- regmatches(attr_str, regexpr("(?i)\\blocus_tag=([^;]+)", attr_str, perl = TRUE))
      # GTF attributes: gene_id "...", gene_name "..."
      gid_m <- regmatches(attr_str, regexpr('gene_id "([^"]+)"',   attr_str, perl = TRUE))
      gnm_m <- regmatches(attr_str, regexpr('gene_name "([^"]+)"', attr_str, perl = TRUE))

      gene_id <- if (length(id_m) > 0L)
                   sub("(?i)^ID=", "", id_m, perl = TRUE)
                 else if (length(gid_m) > 0L)
                   gsub('"', "", sub('^gene_id ', "", gid_m))
                 else paste0(feat, "_", chrom, "_", s)

      name    <- if (length(nm_m) > 0L)
                   sub("(?i)^Name=", "", nm_m, perl = TRUE)
                 else if (length(gnm_m) > 0L)
                   gsub('"', "", sub('^gene_name ', "", gnm_m))
                 else gene_id

      locus_tag <- if (length(lt_m) > 0L)
                     sub("(?i)^locus_tag=", "", lt_m, perl = TRUE)
                   else NA_character_

      # Searchable label: all identifiers + coordinates + strand
      parts <- unique(c(gene_id,
                        if (!is.na(name) && name != gene_id) name else NULL,
                        if (!is.na(locus_tag)) locus_tag else NULL))
      searchable_label <- paste0(
        paste(parts, collapse = " | "),
        "  [", chrom, ":", s, "-", e, " ", strand, "]"
      )

      rows[[length(rows) + 1L]] <- list(
        gene_id          = gene_id,
        name             = name,
        locus_tag        = if (!is.na(locus_tag)) locus_tag else "",
        chrom            = chrom,
        start            = s,
        end              = e,
        strand           = strand,
        searchable_label = searchable_label
      )
    }

    if (length(rows) == 0L) return(NULL)
    df <- data.frame(
      gene_id          = vapply(rows, `[[`, character(1L), "gene_id"),
      name             = vapply(rows, `[[`, character(1L), "name"),
      locus_tag        = vapply(rows, `[[`, character(1L), "locus_tag"),
      chrom            = vapply(rows, `[[`, character(1L), "chrom"),
      start            = vapply(rows, `[[`, integer(1L),   "start"),
      end              = vapply(rows, `[[`, integer(1L),   "end"),
      strand           = vapply(rows, `[[`, character(1L), "strand"),
      searchable_label = vapply(rows, `[[`, character(1L), "searchable_label"),
      source_file      = file_path,
      stringsAsFactors = FALSE
    )
    df[order(df$chrom, df$start), ]
  }, error = function(e) {
    warning(sprintf("[genome_index] inspect_gff_genes(%s): %s",
                    basename(file_path), conditionMessage(e)))
    NULL
  })
}

# -----------------------------------------------------------------------------
# 9. Gene index cache
# -----------------------------------------------------------------------------

#' Save gene index to project metadata cache
#'
#' @param gene_df data.frame from inspect_gff_genes()
#' @param gff_file_path absolute path to the GFF/GFF3/GTF source file
#' @param gff_file_id file_id from the registry (used as cache key)
#' @param project_config project config list (must have $project_path)
#' @export
save_gene_index_cache <- function(gene_df, gff_file_path, gff_file_id, project_config) {
  tryCatch({
    cache_dir  <- file.path(project_config$project_path, "metadata")
    if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
    cache_path <- file.path(cache_dir,
                            paste0("gene_index_", gff_file_id, ".json"))
    fi <- file.info(gff_file_path)
    cache_obj <- list(
      generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      version      = "2",
      source_file  = gff_file_path,
      file_mtime   = as.numeric(fi$mtime),
      file_size    = as.numeric(fi$size),
      n_genes      = nrow(gene_df),
      genes        = gene_df
    )
    jsonlite::write_json(cache_obj, cache_path, auto_unbox = TRUE, pretty = FALSE)
    message(sprintf("[genome_index] Gene index saved: %d genes \u2192 %s",
                    nrow(gene_df), basename(cache_path)))
    invisible(cache_path)
  }, error = function(e) {
    warning(sprintf("[genome_index] Could not save gene cache: %s", conditionMessage(e)))
    invisible(NULL)
  })
}

#' Load gene index from project metadata cache
#'
#' @param gff_file_id file_id used when saving the cache
#' @param project_config project config list
#' @return cache list (with $genes data.frame and $source_file) or NULL
#' @export
load_gene_index_cache <- function(gff_file_id, project_config) {
  cache_path <- file.path(project_config$project_path, "metadata",
                          paste0("gene_index_", gff_file_id, ".json"))
  if (!file.exists(cache_path)) return(NULL)
  tryCatch({
    cache <- jsonlite::read_json(cache_path, simplifyVector = TRUE)
    if (is.null(cache$genes) || !is.data.frame(cache$genes)) return(NULL)
    cache
  }, error = function(e) NULL)
}

#' Check whether the gene index cache is still valid
#'
#' Compares the GFF file's current mtime against the one stored in cache.
#'
#' @param cache result of load_gene_index_cache()
#' @param gff_file_path absolute path to the GFF source file
#' @return TRUE if cache is valid, FALSE otherwise
#' @export
is_gene_index_cache_valid <- function(cache, gff_file_path) {
  if (is.null(cache)) return(FALSE)
  if (is.null(cache$source_file) || !identical(cache$source_file, gff_file_path)) return(FALSE)
  if (!file.exists(gff_file_path)) return(FALSE)
  cached_mtime  <- suppressWarnings(as.numeric(cache$file_mtime))
  current_mtime <- as.numeric(file.info(gff_file_path)$mtime)
  if (is.na(cached_mtime)) return(FALSE)
  abs(current_mtime - cached_mtime) <= 1
}

# -----------------------------------------------------------------------------
# 10. selectize helpers
# -----------------------------------------------------------------------------

#' Build named choices for server-side gene selectize
#'
#' @param gene_df data.frame from inspect_gff_genes()
#' @return named character vector(label -> row_index)
#' @export
build_gene_selectize_choices <- function(gene_df) {
  if (is.null(gene_df) || !is.data.frame(gene_df) || nrow(gene_df) == 0L) {
    return(setNames(character(0), character(0)))
  }
  labels <- if ("searchable_label" %in% names(gene_df)) {
    gene_df$searchable_label
  } else {
    paste0(gene_df$name, "  [", gene_df$chrom, ":", gene_df$start, "-", gene_df$end, "]")
  }
  stats::setNames(as.character(seq_len(nrow(gene_df))), labels)
}
