# =============================================================================
# utils_json.R — JSON and YAML I/O helpers
# =============================================================================

#' Write an R object to a pretty-printed JSON file
#'
#' @param x R object to serialize
#' @param path target file path
#' @return invisible path
#' @export
write_json_pretty <- function(x, path) {
  ensure_dir(dirname(path))
  jsonlite::write_json(x, path, pretty = TRUE, auto_unbox = TRUE, null = "null")
  invisible(path)
}

#' Read a JSON file, returning NULL if the file doesn't exist
#'
#' @param path file path
#' @return parsed R object or NULL
#' @export
read_json_safe <- function(path) {
  if (!file.exists(path)) return(NULL)
  jsonlite::read_json(path, simplifyVector = FALSE)
}

#' Write an R object to a YAML file
#'
#' @param x R object to serialize
#' @param path target file path
#' @return invisible path
#' @export
write_yaml_safe <- function(x, path) {
  ensure_dir(dirname(path))
  yaml::write_yaml(x, path)
  invisible(path)
}

#' Read a YAML file, returning NULL if the file doesn't exist
#'
#' @param path file path
#' @return parsed R object or NULL
#' @export
read_yaml_safe <- function(path) {
  if (!file.exists(path)) return(NULL)
  yaml::read_yaml(path)
}
