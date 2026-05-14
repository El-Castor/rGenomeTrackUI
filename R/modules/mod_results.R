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
    shiny::h3("Résultats"),
    shiny::uiOutput(ns("run_selector_ui")),
    shiny::fluidRow(
      shiny::column(4,
        shiny::wellPanel(
          shiny::h5("Informations du run"),
          shiny::uiOutput(ns("run_info_ui")),
          shiny::hr(),
          shiny::h5("Téléchargements"),
          shiny::downloadButton(ns("dl_figure"),   "Figure (PNG/PDF)",    class = "btn btn-sm btn-outline-primary d-block mb-1"),
          shiny::downloadButton(ns("dl_ini"),       "tracks.ini",          class = "btn btn-sm btn-outline-secondary d-block mb-1"),
          shiny::downloadButton(ns("dl_r_script"),  "Script R",            class = "btn btn-sm btn-outline-secondary d-block mb-1"),
          shiny::downloadButton(ns("dl_sh_script"), "Script Shell",        class = "btn btn-sm btn-outline-secondary d-block mb-1"),
          shiny::downloadButton(ns("dl_logs"),      "Logs",                class = "btn btn-sm btn-outline-secondary d-block mb-1"),
          shiny::downloadButton(ns("dl_zip"),       "Archive ZIP complète",class = "btn btn-sm btn-outline-dark d-block mb-1")
        )
      ),
      shiny::column(8,
        shiny::tabsetPanel(
          shiny::tabPanel("Figure",
            shiny::br(),
            shiny::uiOutput(ns("figure_ui"))
          ),
          shiny::tabPanel("Fichiers de sortie",
            shiny::br(),
            shiny::uiOutput(ns("outputs_list_ui"))
          ),
          shiny::tabPanel("Logs",
            shiny::verbatimTextOutput(ns("logs_view"))
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
        shiny::div(class = "alert alert-info",
                   "Aucun run sélectionné. Lancez un run ou sélectionnez-en un dans l'historique.")
      } else {
        shiny::div(class = "alert alert-success",
                   shiny::strong("Run actif : "), shiny::code(path))
      }
    })

    run_meta <- shiny::reactive({
      path <- selected_run_path()
      if (is.null(path)) return(NULL)
      tryCatch(load_run_metadata(path), error = function(e) NULL)
    })

    output$run_info_ui <- shiny::renderUI({
      meta <- run_meta()
      if (is.null(meta)) return(shiny::p(class = "text-muted", "Aucun run."))
      st_class <- switch(meta$status %||% "unknown",
        "completed" = "text-success", "failed" = "text-danger",
        "running" = "text-warning", "text-secondary")
      shiny::tagList(
        shiny::tags$dl(
          shiny::tags$dt("Nom"),     shiny::tags$dd(meta$run_name %||% "?"),
          shiny::tags$dt("Statut"),  shiny::tags$dd(shiny::span(class = st_class, meta$status %||% "?")),
          shiny::tags$dt("Créé"),    shiny::tags$dd(meta$created_at %||% "?"),
          shiny::tags$dt("Région"),  shiny::tags$dd(meta$region %||% "?"),
          shiny::tags$dt("Renderer"),shiny::tags$dd(meta$renderer %||% "?")
        )
      )
    })

    output$figure_ui <- shiny::renderUI({
      path <- selected_run_path()
      if (is.null(path)) return(shiny::p("Pas de run sélectionné."))
      out_dir <- file.path(path, "outputs", "multi_region")
      if (!dir.exists(out_dir)) return(shiny::p("Pas encore de sorties."))
      pngs <- list.files(out_dir, pattern = "\\.png$", full.names = TRUE)
      if (length(pngs) == 0) return(shiny::p("Aucune figure PNG générée."))
      # Expose le répertoire de sortie comme ressource statique Shiny
      shiny::addResourcePath("run_output_current", out_dir)
      shiny::tags$img(
        src   = paste0("run_output_current/", basename(pngs[1])),
        style = "max-width:100%;",
        alt   = "Figure générée"
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
