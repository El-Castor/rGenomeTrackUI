# =============================================================================
# mod_settings.R — Settings & dependency checker module
# =============================================================================

#' Settings module UI
#'
#' @param id module namespace ID
#' @export
mod_settings_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Paramètres & Dépendances"),
    shiny::fluidRow(
      shiny::column(6,
        shiny::wellPanel(
          shiny::h4("Environnement"),
          shiny::uiOutput(ns("env_ui")),
          shiny::hr(),
          shiny::actionButton(ns("btn_check"), "Relancer la vérification",
                              class = "btn btn-primary")
        )
      ),
      shiny::column(6,
        shiny::wellPanel(
          shiny::h4("Chemins de l'application"),
          shiny::uiOutput(ns("paths_ui"))
        )
      )
    ),
    shiny::hr(),
    shiny::h4("Rapport de dépendances"),
    shiny::verbatimTextOutput(ns("dep_report"))
  )
}

#' Settings module server
#'
#' @param id module namespace ID
#' @param app_state reactive values
#' @export
mod_settings_server <- function(id, app_state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    dep_result <- shiny::reactiveVal(NULL)

    shiny::observe({
      if (is.null(dep_result())) {
        dep_result(run_dependency_check())
      }
    })

    shiny::observeEvent(input$btn_check, {
      shiny::withProgress(message = "Vérification en cours...", {
        dep_result(run_dependency_check())
      })
      shiny::showNotification("Vérification terminée.", type = "message")
    })

    output$env_ui <- shiny::renderUI({
      dc <- dep_result()
      if (is.null(dc)) return(shiny::p("Non vérifié."))
      st_class <- switch(dc$status,
        "OK"      = "alert alert-success",
        "WARNING" = "alert alert-warning",
        "ERROR"   = "alert alert-danger",
        "alert alert-secondary"
      )
      shiny::tagList(
        shiny::div(class = st_class, shiny::strong(sprintf("Statut global : %s", dc$status))),
        shiny::tags$dl(
          shiny::tags$dt("Conda env actif"),  shiny::tags$dd(shiny::code(dc$conda_active %||% "?")),
          shiny::tags$dt("R"),               shiny::tags$dd(shiny::code(dc$r_path %||% "?")),
          shiny::tags$dt("Python"),          shiny::tags$dd(shiny::code(dc$python_path %||% "non trouvé")),
          shiny::tags$dt("pyGenomeTracks"),  shiny::tags$dd(shiny::tagList(
            shiny::code(dc$pygenometracks_path %||% "non trouvé"),
            shiny::span(sprintf(" — version: %s", dc$pygenometracks_version %||% "N/A"))
          )),
          shiny::tags$dt("rGenomeTracks"),   shiny::tags$dd(if (isTRUE(dc$rGenomeTracks)) "✓ disponible" else "✗ manquant"),
          shiny::tags$dt("BEDTools"),        shiny::tags$dd(if (isTRUE(dc$bedtools)) "✓ disponible" else "✗ non trouvé"),
          shiny::tags$dt("Vérifié le"),      shiny::tags$dd(dc$checked_at %||% "?")
        )
      )
    })

    output$paths_ui <- shiny::renderUI({
      root     <- app_state$projects_root %||% "projects"
      app_root <- tryCatch(get_app_root(), error = function(e) "<erreur>")
      shiny::tags$dl(
        shiny::tags$dt("Racine de l'application"), shiny::tags$dd(shiny::code(app_root)),
        shiny::tags$dt("Dossier projets"),         shiny::tags$dd(shiny::code(normalizePath(root, mustWork = FALSE))),
        shiny::tags$dt("Dossier logs"),            shiny::tags$dd(shiny::code(normalizePath("logs", mustWork = FALSE))),
        shiny::tags$dt("Dossier config"),          shiny::tags$dd(shiny::code(normalizePath("config", mustWork = FALSE))),
        shiny::tags$dt("Dossier templates"),       shiny::tags$dd(shiny::code(normalizePath("templates", mustWork = FALSE)))
      )
    })

    output$dep_report <- shiny::renderText({
      dc <- dep_result()
      if (is.null(dc)) return("Non vérifié.")
      msgs <- dc$messages %||% character(0)
      paste(c(
        sprintf("=== Rapport de dépendances — %s ===", dc$checked_at %||% "?"),
        sprintf("Statut global : %s", dc$status),
        "",
        msgs
      ), collapse = "\n")
    })
  })
}
