# =============================================================================
# r_script_generator.R — Generate reproducible rGenomeTracks R scripts
# =============================================================================

#' Generate the content of a run_rGenomeTracks.R script
#'
#' @param run_path path to run directory
#' @param tracks list of enabled track objects
#' @param schema schema list from load_track_schema
#' @param regions character vector of region strings
#' @param figure_settings named list of figure settings
#' @return character string with R script content
#' @export
generate_rgenometracks_script <- function(run_path, tracks, schema, regions, figure_settings) {
  fs <- figure_settings %||% list()
  output_format  <- fs$output_format  %||% "png"
  width          <- fs$width          %||% 38
  dpi            <- fs$dpi            %||% 150
  output_basename <- fs$output_basename %||% "figure"
  title          <- fs$title          %||% ""

  # Determine tracks that are NOT supported by rGenomeTracks
  not_supported_types <- c("hic_matrix", "scalebar", "vhighlight", "epilogos")
  has_unsupported <- any(vapply(tracks, function(t) t$track_type %in% not_supported_types, logical(1)))

  # Build track construction code
  track_lines <- character(0)
  track_vars  <- character(0)
  unsupported_warnings <- character(0)

  for (i in seq_along(tracks)) {
    t        <- tracks[[i]]
    ttype    <- t$track_type
    rfun     <- get_rgenometracks_function(schema, ttype)
    var_name <- sprintf("track_%d", i)

    if (is.null(rfun) || ttype %in% not_supported_types) {
      unsupported_warnings <- c(
        unsupported_warnings,
        sprintf("# WARNING: track '%s' (type '%s') is not directly supported by rGenomeTracks.",
                t$track_name %||% ttype, ttype),
        sprintf("# Use pyGenomeTracks (run_pyGenomeTracks.sh) for this track type.")
      )
      # Substitute with a spacer of same height to preserve layout
      height_val <- t$params$height %||% 1
      track_lines <- c(track_lines,
        "",
        sprintf("# PLACEHOLDER for '%s' (type '%s') — not supported by rGenomeTracks",
                t$track_name %||% ttype, ttype),
        sprintf("%s <- rGenomeTracks::track_spacer(height = %s)", var_name, height_val)
      )
      track_vars <- c(track_vars, var_name)
      next
    }

    # Build params list
    param_defs   <- get_track_params(schema, ttype)
    track_params <- t$params %||% list()
    param_parts  <- character(0)

    # File param (if required)
    if (track_requires_file(schema, ttype)) {
      file_path <- t$file_path %||% ""
      if (nchar(file_path) > 0) {
        param_parts <- c(param_parts, sprintf('  file = "%s"', gsub('"', '\\"', file_path)))
      }
    }

    # Named params
    for (pname in names(param_defs)) {
      val <- track_params[[pname]]
      if (is.null(val)) val <- param_defs[[pname]]$default
      if (is.null(val)) next
      ptype <- param_defs[[pname]]$type %||% "text"
      r_val <- switch(ptype,
        "boolean" = if (isTRUE(val)) "TRUE" else "FALSE",
        "numeric" = as.character(val),
        "color"   = sprintf('"%s"', val),
        "text"    = sprintf('"%s"', gsub('"', '\\"', as.character(val))),
        "select"  = sprintf('"%s"', as.character(val)),
        sprintf('"%s"', as.character(val))
      )
      # Skip auto string params if pyGenomeTracks default
      if (as.character(val) == "auto" && pname %in% c("min_value", "max_value")) next
      param_parts <- c(param_parts, sprintf("  %s = %s", pname, r_val))
    }

    if (length(param_parts) > 0) {
      param_str <- paste(param_parts, collapse = ",\n")
      track_lines <- c(track_lines, "",
        sprintf("%s <- rGenomeTracks::%s(\n%s\n)", var_name, rfun, param_str))
    } else {
      track_lines <- c(track_lines, "",
        sprintf("%s <- rGenomeTracks::%s()", var_name, rfun))
    }
    track_vars <- c(track_vars, var_name)
  }

  # Region loop
  region_lines <- character(0)
  if (length(regions) == 0) regions <- c("chr1:1-1000000")
  for (reg in regions) {
    safe_reg <- sanitize_region_for_filename(reg)
    region_lines <- c(region_lines,
      sprintf('  region <- "%s"', reg),
      sprintf('  out_file <- file.path(run_path, "outputs", "multi_region",'),
      sprintf('    sprintf("%s_%%s.%s", gsub("[:.-]", "_", region)))', output_basename, output_format),
      '  tryCatch({',
      '    rGenomeTracks::plot_gtracks(',
      sprintf('      tracks = tracks_combined,'),
      '      region = region,',
      sprintf('      title = "%s",', gsub('"', '\\"', title)),
      sprintf('      width = %s,', width),
      sprintf('      dpi = %s,', dpi),
      sprintf('      file = out_file'),
      '    )',
      sprintf('    cat(sprintf("[OK] Figure written: %%s\\n", out_file))'),
      '  }, error = function(e) {',
      '    cat(sprintf("[ERROR] Region %s: %%s\\n", e$message))',
      '    cat(sprintf("[ERROR] Region %s: %%s\\n", e$message), file = stderr_log, append = TRUE)',
      '  })'
    )
  }

  # Assemble script
  lines <- c(
    "#!/usr/bin/env Rscript",
    "# =============================================================================",
    "# run_rGenomeTracks.R — Generated by rGenomeTrackUI",
    sprintf("# Generated: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    "# Run with: Rscript scripts/run_rGenomeTracks.R",
    "# (from within the run directory, with conda env rgenometrackui active)",
    "# =============================================================================",
    "",
    "run_path <- dirname(dirname(normalizePath(sys.frame(1)$ofile, mustWork = FALSE)))",
    "if (!nchar(run_path)) run_path <- getwd()",
    "",
    "# Redirect stderr to log file",
    'stderr_log <- file.path(run_path, "logs", "stderr.log")',
    "",
    "# Write session info",
    'sink(file.path(run_path, "logs", "sessionInfo.txt"))',
    "sessionInfo()",
    "sink()",
    "",
    "# Load required packages",
    "suppressPackageStartupMessages({",
    "  library(rGenomeTracks)",
    "  library(jsonlite)",
    "})",
    ""
  )

  if (length(unsupported_warnings) > 0) {
    lines <- c(lines, unsupported_warnings, "")
  }

  lines <- c(lines, track_lines)

  # Combine tracks
  if (length(track_vars) > 0) {
    combine_expr <- paste(track_vars, collapse = " +\n  ")
    lines <- c(lines,
      "",
      "# Combine all tracks",
      sprintf("tracks_combined <- %s", combine_expr),
      "",
      "# Create output directory",
      'dir.create(file.path(run_path, "outputs", "multi_region"), recursive = TRUE, showWarnings = FALSE)',
      "",
      "# Render for each region",
      "for (region in c(" ,
      paste0('  "', regions, '"', collapse = ",\n"),
      ")) {",
      region_lines,
      "}",
      "",
      'cat("[rGenomeTracks] Done.\\n")'
    )
  } else {
    lines <- c(lines, "", "# No valid rGenomeTracks tracks found. Use pyGenomeTracks instead.")
  }

  script_text <- paste(lines, collapse = "\n")
  write_rgenometracks_script(run_path, script_text)
  invisible(script_text)
}

#' Write the R script to disk
#'
#' @param run_path path to run directory
#' @param script_text character string with script content
#' @return invisible path
#' @export
write_rgenometracks_script <- function(run_path, script_text) {
  path <- file.path(run_path, "scripts", "run_rGenomeTracks.R")
  ensure_dir(dirname(path))
  writeLines(script_text, path)
  invisible(path)
}
