library(testthat)

# =============================================================================
# test_ui_bindings_backend.R
# Static analysis: every observeEvent(input$btn_X) in a module server must
# have a matching actionButton / downloadButton / uiOutput(ns("btn_X")) in
# the corresponding module UI function.
# =============================================================================

# Helper: extract all input IDs from observeEvent(input$<id>) calls
extract_server_input_ids <- function(text) {
  m <- gregexpr("observeEvent\\(input\\$([A-Za-z0-9_]+)", text, perl = TRUE)
  matches <- regmatches(text, m)[[1]]
  sub("observeEvent\\(input\\$", "", matches)
}

# Helper: extract button / output IDs defined in UI via ns("...")
extract_ui_ns_ids <- function(text) {
  m <- gregexpr('ns\\("([A-Za-z0-9_]+)"\\)', text, perl = TRUE)
  matches <- regmatches(text, m)[[1]]
  sub('ns\\("', "", sub('"\\)', "", matches))
}

modules <- c(
  "mod_dashboard.R",
  "mod_inputs.R",
  "mod_preview.R",
  "mod_run.R",
  "mod_results.R",
  "mod_history.R",
  "mod_track_builder.R",
  "mod_region.R",
  "mod_settings.R",
  "mod_projects.R"
)

modules_dir <- file.path("..", "..", "R", "modules")

for (mod in modules) {
  mod_path <- file.path(modules_dir, mod)
  if (!file.exists(mod_path)) next

  local({
    m <- mod
    p <- mod_path

    test_that(sprintf("%s: tous les observeEvent(input$btn_X) ont un élément UI correspondant", m), {
      text <- paste(readLines(p, warn = FALSE), collapse = "\n")

      server_ids <- extract_server_input_ids(text)
      ui_ids     <- extract_ui_ns_ids(text)

      # Only check IDs that start with "btn_" (action buttons) or "confirm_"
      btn_server  <- server_ids[grepl("^btn_|^confirm_", server_ids)]
      btn_ui      <- ui_ids[grepl("^btn_|^confirm_", ui_ids)]

      missing_in_ui <- setdiff(btn_server, btn_ui)

      expect_equal(
        length(missing_in_ui), 0L,
        label = sprintf(
          "%s: les suivants sont observés côté serveur mais absents de la UI : %s",
          m, paste(missing_in_ui, collapse = ", ")
        )
      )
    })
  })
}

# Bonus: verify no module has a bare shiny::small() call (regression guard)
test_that("Aucun module n'utilise shiny::small() non exporté", {
  mod_files <- list.files(modules_dir, pattern = "\\.R$", full.names = TRUE)
  for (f in mod_files) {
    txt <- paste(readLines(f, warn = FALSE), collapse = "\n")
    # shiny::small( not followed by "tags$" is the problematic form
    has_bad <- grepl("shiny::small\\s*\\(", txt)
    expect_false(has_bad,
      label = sprintf("%s: contient shiny::small() non exporté", basename(f)))
  }
})

# Bonus: verify no module has a hardcoded "/preview" path literal
test_that("Aucun module n'a de chemin /preview codé en dur", {
  mod_files <- list.files(modules_dir, pattern = "\\.R$", full.names = TRUE)
  for (f in mod_files) {
    txt <- paste(readLines(f, warn = FALSE), collapse = "\n")
    # Look for the literal string "/preview" in quotes (not get_preview_dir)
    has_bad <- grepl('"[^"]*[^a-zA-Z_]/preview[^"]*"', txt, perl = TRUE)
    expect_false(has_bad,
      label = sprintf("%s: contient un chemin /preview codé en dur", basename(f)))
  }
})
