# =============================================================================
# project_manager.R — Project creation and management
# =============================================================================

APP_VERSION <- "0.1.0"

#' Validate a project name
#'
#' @param project_name character string
#' @return invisible TRUE or stops with error
#' @export
validate_project_name <- function(project_name) {
  if (!is.character(project_name) || length(project_name) != 1)
    stop("validate_project_name: project_name must be a single string")
  project_name <- trimws(project_name)
  if (nchar(project_name) == 0) stop("Project name cannot be empty")
  if (nchar(project_name) > 128) stop("Project name too long (max 128 characters)")
  sanitize_name(project_name)  # will error on dangerous chars
  invisible(TRUE)
}

#' Create a new project
#'
#' Creates the project directory structure and project_config.json.
#'
#' @param project_name human-readable project name
#' @param genome_label genome label (e.g. "hg38", "mm10")
#' @param description optional description
#' @param root_dir root directory for projects (relative or absolute)
#' @return list with project config
#' @export
create_project <- function(project_name, genome_label, description = "", root_dir = "projects") {
  validate_project_name(project_name)
  if (!is.character(genome_label) || nchar(trimws(genome_label)) == 0)
    stop("genome_label cannot be empty")

  project_slug <- slugify(project_name)
  if (nchar(project_slug) == 0)
    stop("Project name produces an empty slug. Please use alphanumeric characters.")

  root_dir <- normalize_project_path(root_dir)
  project_path <- file.path(root_dir, project_slug)

  if (dir.exists(project_path))
    stop(sprintf("Project '%s' already exists at: %s", project_slug, project_path))

  # Create directory structure
  dirs <- c(
    project_path,
    file.path(project_path, "inputs", "raw"),
    file.path(project_path, "inputs", "linked"),
    file.path(project_path, "templates"),
    file.path(project_path, "runs"),
    file.path(project_path, "exports")
  )
  for (d in dirs) ensure_dir(d)

  # Build config
  config <- list(
    project_name   = project_name,
    project_slug   = project_slug,
    genome_label   = trimws(genome_label),
    description    = trimws(description),
    created_at     = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
    app_version    = APP_VERSION,
    project_path   = project_path
  )

  save_project_config(config, project_path)
  message(sprintf("[project_manager] Project '%s' created at: %s", project_name, project_path))
  invisible(config)
}

#' Load a project from its path
#'
#' @param project_path path to project directory
#' @return list with project config
#' @export
load_project <- function(project_path) {
  project_path <- normalize_project_path(project_path)
  config_path <- file.path(project_path, "project_config.json")
  if (!file.exists(config_path))
    stop(sprintf("No project_config.json found at: %s", project_path))
  config <- read_json_safe(config_path)
  # Ensure path is current (may have moved)
  config$project_path <- project_path
  config
}

#' List all projects in root directory
#'
#' @param root_dir root directory to scan
#' @return data.frame of projects
#' @export
list_projects <- function(root_dir = "projects") {
  root_dir <- normalize_project_path(root_dir)
  if (!dir.exists(root_dir)) return(data.frame())

  dirs <- list.dirs(root_dir, recursive = FALSE, full.names = TRUE)
  results <- lapply(dirs, function(d) {
    cfg_path <- file.path(d, "project_config.json")
    if (!file.exists(cfg_path)) return(NULL)
    cfg <- tryCatch(read_json_safe(cfg_path), error = function(e) NULL)
    if (is.null(cfg)) return(NULL)
    data.frame(
      project_name  = cfg$project_name  %||% basename(d),
      project_slug  = cfg$project_slug  %||% basename(d),
      genome_label  = cfg$genome_label  %||% "",
      description   = cfg$description   %||% "",
      created_at    = cfg$created_at    %||% "",
      project_path  = d,
      stringsAsFactors = FALSE
    )
  })
  results <- Filter(Negate(is.null), results)
  if (length(results) == 0) return(data.frame())
  do.call(rbind, results)
}

#' Get project config
#'
#' @param project_path path to project directory
#' @return list
#' @export
get_project_config <- function(project_path) {
  load_project(project_path)
}

#' Save project config to disk
#'
#' @param config list
#' @param project_path path to project directory
#' @return invisible path
#' @export
save_project_config <- function(config, project_path) {
  config_path <- file.path(project_path, "project_config.json")
  write_json_pretty(config, config_path)
  invisible(config_path)
}

# Utility: null-coalescing operator
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b
