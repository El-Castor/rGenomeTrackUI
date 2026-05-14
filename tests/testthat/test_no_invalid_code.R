library(testthat)

# =============================================================================
# test_no_invalid_code.R — Static analysis: forbidden patterns in R source
# =============================================================================

r_sources <- list.files(
  file.path("..", "..", "R"),
  pattern = "\\.R$", recursive = TRUE, full.names = TRUE
)

read_source <- function(path) paste(readLines(path, warn = FALSE), collapse = "\n")

test_that("No occurrence of shiny::small() in R sources", {
  for (f in r_sources) {
    content <- read_source(f)
    expect_false(
      grepl("shiny::small\\s*\\(", content),
      info = sprintf("shiny::small() found in %s — use tags$small() instead", basename(f))
    )
  }
})

test_that("No hardcoded /preview path in R sources", {
  for (f in r_sources) {
    content <- read_source(f)
    # Must not pass the literal string "/preview" as a run_path argument
    has_hardcoded_preview <- grepl('["\'](/preview)["\']', content, perl = TRUE)
    expect_false(
      has_hardcoded_preview,
      info = sprintf('Hardcoded "/preview" path found in %s', basename(f))
    )
  }
})

test_that("get_preview_dir exists in utils_paths.R", {
  src <- read_source(file.path("..", "..", "R", "core", "utils_paths.R"))
  expect_true(grepl("get_preview_dir", src))
})

test_that("mod_preview.R uses get_preview_dir, not \"/preview\"", {
  src <- read_source(file.path("..", "..", "R", "modules", "mod_preview.R"))
  expect_true(grepl("get_preview_dir", src))
  # The literal string "/preview" must no longer appear as an argument
  expect_false(grepl('generate_rgenometracks_script\\s*\\(\\s*\\"\\s*/preview', src))
  expect_false(grepl('generate_pygenometracks_shell_script\\s*\\(\\s*\\"\\s*/preview', src))
})

test_that("mod_run.R uses tags$small, not shiny::small", {
  src <- read_source(file.path("..", "..", "R", "modules", "mod_run.R"))
  expect_false(grepl("shiny::small\\s*\\(", src))
  expect_true(grepl("tags\\$small|shiny::tags\\$small", src))
})
