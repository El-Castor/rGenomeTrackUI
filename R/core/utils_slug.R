# =============================================================================
# utils_slug.R — Slug and name sanitization utilities
# =============================================================================

# Characters that are dangerous in shell commands or file paths
.DANGEROUS_CHARS <- c("../", "./", "~", "|", ";", "&", "$", "`", "\\",
                       "\"", "'", "!", "*", "?", "[", "]", "{", "}", "(", ")",
                       "<", ">", "\n", "\r", "\t")

#' Convert a string to a safe slug (lowercase, underscores, ASCII only)
#'
#' @param x character string to convert
#' @param max_len maximum number of characters allowed (default 64)
#' @return cleaned slug string
#' @export
slugify <- function(x, max_len = 64) {
  if (nchar(x) == 0) stop("slugify: input is empty")
  if (!is.character(x) || length(x) != 1) stop("slugify: x must be a single character string")
  x <- trimws(x)
  # Transliterate accented characters (basic ASCII approximation)
  x <- iconv(x, to = "ASCII//TRANSLIT", sub = "")
  x <- tolower(x)
  # Replace spaces, hyphens, dots with underscore
  x <- gsub("[[:space:]\\-\\.]+", "_", x)
  # Remove all non-alphanumeric/underscore characters
  x <- gsub("[^a-z0-9_]", "", x)
  # Collapse multiple underscores
  x <- gsub("_+", "_", x)
  # Strip leading/trailing underscores
  x <- gsub("^_+|_+$", "", x)
  # Truncate
  if (nchar(x) > max_len) x <- substr(x, 1, max_len)
  if (nchar(x) == 0) stop("slugify: result is empty — input has no valid characters")
  x
}

#' Validate that a slug is safe and non-empty
#'
#' @param x string to validate
#' @return TRUE if valid, FALSE otherwise (never throws)
#' @export
validate_slug <- function(x) {
  if (!is.character(x) || length(x) != 1 || nchar(x) == 0) return(FALSE)
  grepl("^[a-z0-9][a-z0-9_]*$", x)
}

#' Sanitize a human-readable name for safe filesystem use
#' Less restrictive than slugify — allows mixed case and spaces but removes
#' shell-dangerous characters.
#'
#' @param x character string
#' @return sanitized string
#' @export
sanitize_name <- function(x) {
  if (!is.character(x) || length(x) != 1) stop("sanitize_name: x must be a single character string")
  x <- trimws(x)
  if (nchar(x) == 0) stop("sanitize_name: input is empty")
  # Reject absolute paths and path traversal (these are security risks)
  if (startsWith(x, "/"))
    stop(sprintf("sanitize_name: absolute path not allowed: '%s'", x))
  if (startsWith(x, ".."))
    stop(sprintf("sanitize_name: path traversal not allowed: '%s'", x))
  # Replace slashes with hyphens
  x <- gsub("/", "-", x, fixed = TRUE)
  # Replace consecutive dots (e.g. ".." → "--") with hyphens
  x <- gsub("\\.{2,}", "--", x, perl = TRUE)
  # Remove shell metacharacters
  x <- gsub("[|;&$`\\\\\"'!*?\\[\\]{}<>]", "", x, perl = TRUE)
  # Remove control characters
  x <- gsub("[[:cntrl:]]", "", x)
  x <- trimws(x)
  if (nchar(x) == 0) stop("sanitize_name: resulting name is empty")
  x
}

#' Check if a name is safe for use as a relative path component
#'
#' @param x character string
#' @return logical
#' @export
is_safe_name <- function(x) {
  tryCatch({ sanitize_name(x); TRUE }, error = function(e) FALSE)
}
