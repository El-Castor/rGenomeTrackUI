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

test_that("DataTables ne dépend pas d'un fichier de langue Ajax distant", {
  source_files <- c(
    list.files(modules_dir, pattern = "\\.R$", full.names = TRUE),
    list.files(file.path("..", "..", "R", "core"), pattern = "\\.R$", full.names = TRUE)
  )
  text <- paste(vapply(source_files, function(path) {
    paste(readLines(path, warn = FALSE), collapse = "\n")
  }, character(1)), collapse = "\n")

  expect_false(grepl("cdn\\.datatables\\.net", text))
  expect_false(grepl("language\\s*=\\s*list\\s*\\(\\s*url\\s*=", text, perl = TRUE))
})

test_that("le fallback de validation upload protège les fichiers binaires", {
  inputs_path <- file.path(modules_dir, "mod_inputs.R")
  text <- paste(readLines(inputs_path, warn = FALSE), collapse = "\n")

  expect_match(text, "is_known_binary")
  expect_match(text, "else if \\(is_known_binary\\)")
  expect_match(text, "detect_file_type\\(pf\\$name\\)")
})

test_that("l'upload accepte et enregistre plusieurs fichiers", {
  inputs_path <- file.path(modules_dir, "mod_inputs.R")
  text <- paste(readLines(inputs_path, warn = FALSE), collapse = "\n")

  expect_match(text, "multiple\\s*=\\s*TRUE")
  expect_match(text, "for \\(i in seq_len\\(nrow\\(f\\)\\)\\)")
  expect_match(text, "original_name = f\\$name\\[\\[i\\]\\]")
  expect_match(text, "all\\(file.exists\\(f\\$datapath\\)\\)")
})

test_that("l'aperçu INI diffère les statistiques BigWig coûteuses", {
  preview_path <- file.path(modules_dir, "mod_preview.R")
  text <- paste(readLines(preview_path, warn = FALSE), collapse = "\n")

  expect_match(text, "output\\$ini_preview")
  expect_match(text, "fs\\$light_prepare\\s*<-\\s*TRUE")
})

test_that("les résultats proposent zoom et profil multi-omique", {
  results_path <- file.path(modules_dir, "mod_results.R")
  text <- paste(readLines(results_path, warn = FALSE), collapse = "\n")

  expect_match(text, "rt-figure-viewer")
  expect_match(text, "rt-zoom-fullscreen")
  expect_match(text, "preset_multiomics")
  expect_match(text, "results_track_height.*value = 0.8")
  expect_match(text, "results_compact_genes")
  expect_match(text, "rt_init_figure_viewers")
  expect_match(text, "shared_by_modality")
})

test_that("l'éditeur multiple expose les couleurs des tracks", {
  tracks_path <- file.path(modules_dir, "mod_track_builder.R")
  text <- paste(readLines(tracks_path, warn = FALSE), collapse = "\n")

  expect_match(text, "btn_apply_selected_colors")
  expect_match(text, "Couleurs de la sélection")
  expect_match(text, "batch_track_color_input_id")
  expect_match(text, "Couleur / paramètres")
  expect_match(text, 'scrollY = "360px"', fixed = TRUE)
  expect_match(text, "btn_open_track_params")
  expect_match(text, "btn_auto_palette")
  expect_match(text, "assign_automatic_track_colours")
  expect_match(text, "display_filter_ui")
  expect_match(text, "btn_apply_display_filter")
  expect_match(text, "filter_tracks_for_display")
  expect_match(text, "btn_show_all_tracks")
  expect_match(text, "tracks_order_dragged")
  expect_match(text, "rt-track-order-table")
  expect_match(text, "rt-track-card-list")
  expect_match(text, "track_cards_ui")
  expect_match(text, "track_cards_selected")
  expect_match(text, "ordering = FALSE")
  expect_match(text, "rt_scroll_to")
  expect_match(text, "pdef\\$min %\\|\\|% NA_real_")

  js <- paste(readLines(file.path("..", "..", "www", "app.js"), warn = FALSE), collapse = "\n")
  expect_match(js, "initTrackRowSorting")
  expect_match(js, "initTrackCardSorting")
  expect_match(js, "rt-track-sort-card")
  expect_match(js, "rt_select_track_card")
  expect_match(js, "Shiny.setInputValue\\(inputId, ids")
  expect_match(js, "dragstart")
  expect_match(js, "dragover")
})
