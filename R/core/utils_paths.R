# =============================================================================
# utils_paths.R — Safe path utilities
# =============================================================================

#' Get application root directory
#'
#' Returns the directory containing app.R. Priority:
#' 1. RGENOMETRACKUI_ROOT environment variable (if set and exists)
#' 2. Walk up from cwd looking for app.R (handles test subdirectories)
#' 3. Fallback to cwd
#'
#' @return absolute path string
#' @export
get_app_root <- function() {
  env_root <- Sys.getenv("RGENOMETRACKUI_ROOT", unset = "")
  if (nchar(env_root) > 0 && dir.exists(env_root)) return(normalizePath(env_root))
  # Walk up from current directory looking for app.R (handles testthat subdirs)
  path <- tryCatch(normalizePath(getwd()), error = function(e) NULL)
  if (!is.null(path)) {
    for (i in seq_len(6)) {
      if (file.exists(file.path(path, "app.R"))) return(normalizePath(path))
      parent <- dirname(path)
      if (identical(parent, path)) break   # reached filesystem root
      path <- parent
    }
    # Fallback to original getwd()
    return(normalizePath(getwd()))
  }
  stop("Cannot determine application root directory")
}

#' Check if a name is safe as a relative path component (no traversal, no shell chars)
#'
#' @param x character string
#' @return logical
#' @export
is_safe_relative_name <- function(x) {
  if (!is.character(x) || length(x) != 1 || nchar(trimws(x)) == 0) return(FALSE)
  # Reject path traversal
  if (grepl("\\.\\.", x, fixed = TRUE)) return(FALSE)
  if (grepl("^/", x)) return(FALSE)
  if (grepl("^~", x)) return(FALSE)
  # Reject shell-dangerous characters
  danger_pattern <- "[|;&$`\\\"'!*?<>\\\\]"
  if (grepl(danger_pattern, x)) return(FALSE)
  if (grepl("[\n\r\t]", x)) return(FALSE)
  TRUE
}

#' Safely join path components, checking each component
#'
#' @param ... path components
#' @return joined path string
#' @export
safe_path <- function(...) {
  parts <- list(...)
  parts <- lapply(parts, as.character)
  # First part can be absolute (e.g. root dir)
  if (length(parts) > 1) {
    for (p in parts[-1]) {
      if (!is_safe_relative_name(p))
        stop(sprintf("safe_path: unsafe path component '%s'", p))
    }
  }
  do.call(file.path, parts)
}

#' Ensure a directory exists, creating it if needed
#'
#' @param path directory path to create
#' @param recursive create parent directories too (default TRUE)
#' @return invisible path
#' @export
ensure_dir <- function(path, recursive = TRUE) {
  if (!dir.exists(path)) {
    ok <- dir.create(path, showWarnings = FALSE, recursive = recursive)
    if (!ok && !dir.exists(path))
      stop(sprintf("ensure_dir: failed to create directory '%s'", path))
  }
  invisible(path)
}

#' Resolve a safe directory for preview generation
#'
#' If a project is active and has a valid project_path, returns
#' `<project_path>/preview`. Otherwise falls back to a tempdir-based path.
#' The directory is always created (ensure_dir).
#'
#' @param project_config project config list or NULL
#' @return absolute directory path (always exists)
#' @export
get_preview_dir <- function(project_config = NULL) {
  if (!is.null(project_config) &&
      !is.null(project_config$project_path) &&
      nzchar(project_config$project_path %||% "")) {
    path <- file.path(project_config$project_path, "preview")
  } else {
    path <- file.path(tempdir(), "rGenomeTrackUI_preview")
  }
  ensure_dir(path)
  normalizePath(path, mustWork = TRUE)
}

#' Normalize a project path to absolute
#'
#' @param path path to normalize
#' @return absolute path
#' @export
normalize_project_path <- function(path) {
  if (!is.character(path) || length(path) != 1 || nchar(path) == 0)
    stop("normalize_project_path: invalid path")
  normalizePath(path, mustWork = FALSE)
}
