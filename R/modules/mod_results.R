# =============================================================================
# mod_results.R — Results viewer module
# =============================================================================

#' Results module UI
#'
#' @param id module namespace ID
#' @export
mod_results_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(

    omics_banner(
      "Résultats",
      "Visualisez et téléchargez la figure et les fichiers générés.",
      small = TRUE
    ),

    shiny::uiOutput(ns("run_selector_ui")),

    shiny::fluidRow(
      # Left: run info + downloads
      shiny::column(4,
        shiny::tags$div(
          class = "rt-card",
          shiny::tags$div(
            class = "rt-card-header",
            shiny::tags$span(class = "rt-card-icon", shiny::icon("info-circle")),
            shiny::tags$h5("Informations du run")
          ),
          shiny::uiOutput(ns("run_info_ui"))
        ),
        shiny::tags$div(
          class = "rt-card",
          shiny::tags$div(
            class = "rt-card-header",
            shiny::tags$span(class = "rt-card-icon", shiny::icon("download")),
            shiny::tags$h5("Téléchargements")
          ),
          download_card_btn(ns("dl_figure"),   "Figure (PNG/PDF)", "image", "outputs"),
          download_card_btn(ns("dl_ini"),       "tracks.ini",       "file-code", "config"),
          download_card_btn(ns("dl_r_script"),  "Script R",         "r-project", ".R"),
          download_card_btn(ns("dl_sh_script"), "Script Shell",     "terminal", ".sh"),
          download_card_btn(ns("dl_logs"),      "Logs",             "file-alt", ".txt"),
          download_card_btn(ns("dl_zip"),       "Archive ZIP",      "file-archive", "tout")
        )
      ),
      # Right: figure + files + logs
      shiny::column(8,
        bslib::navset_tab(
          bslib::nav_panel(
            shiny::tagList(shiny::icon("image"), " Figure"),
            shiny::div(class = "mt-2", shiny::uiOutput(ns("figure_ui")))
          ),
          bslib::nav_panel(
            shiny::tagList(shiny::icon("folder-open"), " Fichiers"),
            shiny::div(class = "mt-2", shiny::uiOutput(ns("outputs_list_ui")))
          ),
          bslib::nav_panel(
            shiny::tagList(shiny::icon("align-left"), " Logs"),
            shiny::div(class = "mt-2",
              code_box(ns("logs_view"), lang = "log"))
          )
        )
      )
    )
  )
}

#' Results module server
#'
#' @param id module namespace ID
#' @param app_state reactive values
#' @export
mod_results_server <- function(id, app_state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    selected_run_path <- shiny::reactive({
      app_state$last_run_path %||% NULL
    })

    output$run_selector_ui <- shiny::renderUI({
      path <- selected_run_path()
      if (is.null(path)) {
        shiny::tags$div(
          class = "alert alert-info mb-3",
          shiny::icon("info-circle"),
          " Aucun run sélectionné. Lancez un run ou sélectionnez-en un dans l'historique."
        )
      } else {
        shiny::tags$div(
          class = "mb-3 flex-row gap-8",
          shiny::tags$span(style = "font-size:12px;color:var(--rt-muted);", "Run actif :"),
          path_block(path)
        )
      }
    })

    run_meta <- shiny::reactive({
      path <- selected_run_path()
      if (is.null(path)) return(NULL)
      tryCatch(load_run_metadata(path), error = function(e) NULL)
    })

    output$run_info_ui <- shiny::renderUI({
      meta <- run_meta()
      if (is.null(meta)) {
        return(empty_state("Aucun run", "Lancez ou sélectionnez un run.", icon_name = "play-circle"))
      }
      st <- meta$status %||% "unknown"
      shiny::tags$div(
        shiny::tags$div(
          class = "mb-2",
          status_badge(st)
        ),
        shiny::tags$div(
          style = "display:grid;grid-template-columns:auto 1fr;gap:5px 12px;align-items:baseline;",
          shiny::tags$span(style = "font-size:11px;font-weight:600;color:var(--rt-muted);text-transform:uppercase;", "Nom"),
          shiny::tags$span(style = "font-weight:600;font-size:13px;", meta$run_name %||% "?"),
          shiny::tags$span(style = "font-size:11px;font-weight:600;color:var(--rt-muted);text-transform:uppercase;", "Créé"),
          shiny::tags$span(style = "font-size:12px;color:var(--rt-muted);", meta$created_at %||% "?"),
          shiny::tags$span(style = "font-size:11px;font-weight:600;color:var(--rt-muted);text-transform:uppercase;", "Région"),
          shiny::tags$span(style = "font-size:12px;font-family:var(--rt-font-mono);", meta$region %||% "?"),
          shiny::tags$span(style = "font-size:11px;font-weight:600;color:var(--rt-muted);text-transform:uppercase;", "Renderer"),
          status_badge("info", label = meta$renderer %||% "?", show_dot = FALSE)
        )
      )
    })

    output$figure_ui <- shiny::renderUI({
      path <- selected_run_path()
      if (is.null(path)) {
        return(empty_state("Aucune figure", "Lancez un run pour générer une figure.",
                           icon_name = "image"))
      }
      out_dir <- file.path(path, "outputs", "multi_region")
      if (!dir.exists(out_dir)) {
        return(empty_state("Pas encore de sorties", "Le run n'a pas encore généré de fichiers.",
                           icon_name = "image"))
      }
      pngs <- list.files(out_dir, pattern = "\\.png$", full.names = TRUE)
      if (length(pngs) == 0) {
        return(empty_state("Aucune figure PNG", "Vérifiez les logs pour détecter l'erreur.",
                           icon_name = "image"))
      }
      shiny::addResourcePath("run_output_current", out_dir)
      shiny::tags$div(
        class = "figure-card",
        shiny::tags$img(
          src   = paste0("run_output_current/", basename(pngs[1])),
          style = "max-width:100%;",
          alt   = "Figure générée"
        ),
        shiny::tags$div(
          class = "figure-caption",
          basename(pngs[1]), " — ",
          sprintf("%.0f KB", file.info(pngs[1])$size / 1024)
        )
      )
    })

    output$outputs_list_ui <- shiny::renderUI({
      path <- selected_run_path()
      if (is.null(path)) return(NULL)
      out_dir <- file.path(path, "outputs", "multi_region")
      files <- if (dir.exists(out_dir)) list.files(out_dir, full.names = TRUE) else character(0)
      if (length(files) == 0) return(shiny::p("Aucun fichier de sortie."))
      shiny::tags$ul(lapply(files, function(f) {
        shiny::tags$li(shiny::code(basename(f)),
                       shiny::span(class = "text-muted small",
                                   sprintf(" — %.1f KB", file.info(f)$size / 1024)))
      }))
    })

    output$logs_view <- shiny::renderText({
      path <- selected_run_path()
      if (is.null(path)) return("")
      logs <- tryCatch(read_run_logs(path), error = function(e) list(stdout = "", stderr = ""))
      paste0("=== STDOUT ===\n", logs$stdout, "\n\n=== STDERR ===\n", logs$stderr)
    })

    # ---- Conditional download buttons ----
    shiny::observe({
      path <- selected_run_path()
      if (is.null(path)) {
        for (btn in c("dl_figure", "dl_ini", "dl_r_script", "dl_sh_script", "dl_logs", "dl_zip"))
          shinyjs::disable(btn)
        return()
      }
      out_dir <- file.path(path, "outputs", "multi_region")
      pngs    <- if (dir.exists(out_dir)) list.files(out_dir, pattern = "\\.png$") else character(0)
      shinyjs::toggleState("dl_figure",    condition = length(pngs) > 0)
      shinyjs::toggleState("dl_ini",       condition = file.exists(file.path(path, "config", "tracks.ini")))
      shinyjs::toggleState("dl_r_script",  condition = file.exists(file.path(path, "scripts", "run_rGenomeTracks.R")))
      shinyjs::toggleState("dl_sh_script", condition = file.exists(file.path(path, "scripts", "run_pyGenomeTracks.sh")))
      shinyjs::toggleState("dl_logs",      condition = dir.exists(file.path(path, "logs")))
      shinyjs::toggleState("dl_zip",       condition = dir.exists(path))
    })

    # Downloads
    output$dl_figure <- shiny::downloadHandler("figure.png", function(file) {
      path <- selected_run_path()
      if (is.null(path)) return()
      out_dir <- file.path(path, "outputs", "multi_region")
      pngs    <- list.files(out_dir, pattern = "\\.png$", full.names = TRUE)
      if (length(pngs) > 0) file.copy(pngs[1], file)
    })

    output$dl_ini <- shiny::downloadHandler("tracks.ini", function(file) {
      path <- selected_run_path()
      if (is.null(path)) return()
      src <- file.path(path, "config", "tracks.ini")
      if (file.exists(src)) file.copy(src, file)
    })

    output$dl_r_script <- shiny::downloadHandler("run_rGenomeTracks.R", function(file) {
      path <- selected_run_path()
      if (is.null(path)) return()
      src <- file.path(path, "scripts", "run_rGenomeTracks.R")
      if (file.exists(src)) file.copy(src, file)
    })

    output$dl_sh_script <- shiny::downloadHandler("run_pyGenomeTracks.sh", function(file) {
      path <- selected_run_path()
      if (is.null(path)) return()
      src <- file.path(path, "scripts", "run_pyGenomeTracks.sh")
      if (file.exists(src)) file.copy(src, file)
    })

    output$dl_logs <- shiny::downloadHandler("logs.txt", function(file) {
      path <- selected_run_path()
      if (is.null(path)) return()
      logs <- tryCatch(read_run_logs(path), error = function(e) list(stdout = "", stderr = ""))
      writeLines(c("=== STDOUT ===", logs$stdout, "", "=== STDERR ===", logs$stderr), file)
    })

    output$dl_zip <- shiny::downloadHandler(
      filename = function() {
        meta <- run_meta()
        paste0(meta$run_id %||% "run", ".zip")
      },
      content = function(file) {
        path <- selected_run_path()
        if (is.null(path)) return()
        zip_path <- tryCatch(zip_run(path), error = function(e) NULL)
        if (!is.null(zip_path) && file.exists(zip_path)) file.copy(zip_path, file)
      }
    )
  })
}
