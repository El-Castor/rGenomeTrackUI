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

    omics_banner(
      "Paramètres & Dépendances",
      "Vérifiez l'état de l'environnement et les chemins de l'application.",
      small = TRUE
    ),

    shiny::fluidRow(
      shiny::column(7,
        shiny::tags$div(
          class = "rt-card",
          shiny::tags$div(
            class = "rt-card-header flex-between",
            shiny::tags$div(
              class = "flex-row gap-8",
              shiny::tags$span(class = "rt-card-icon", shiny::icon("server")),
              shiny::tags$h5("Environnement")
            ),
            shiny::actionButton(ns("btn_check"),
              shiny::tagList(shiny::icon("sync"), " Relancer"),
              class = "btn btn-primary btn-sm")
          ),
          shiny::uiOutput(ns("env_ui"))
        )
      ),
      shiny::column(5,
        shiny::tags$div(
          class = "rt-card",
          shiny::tags$div(
            class = "rt-card-header",
            shiny::tags$span(class = "rt-card-icon", shiny::icon("folder-open")),
            shiny::tags$h5("Chemins de l'application")
          ),
          shiny::uiOutput(ns("paths_ui"))
        )
      )
    ),

    shiny::tags$div(
      class = "rt-card mt-3",
      shiny::tags$div(
        class = "rt-card-header",
        shiny::tags$span(class = "rt-card-icon", shiny::icon("file-alt")),
        shiny::tags$h5("Rapport de dépendances")
      ),
      code_box(ns("dep_report"), lang = "text", title = "Rapport complet", allow_copy = TRUE,
               height = "260px")
    )
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
      if (is.null(dc)) {
        return(shiny::div(class = "dep-item dep-warning",
          shiny::icon("hourglass-half"), " Vérification non encore lancée."))
      }
      overall_status <- switch(dc$status,
        "OK"      = "ok",
        "WARNING" = "warning",
        "ERROR"   = "error",
        "disabled"
      )
      shiny::tagList(
        shiny::div(
          class = "mb-3",
          status_badge(overall_status, sprintf("Statut global : %s", dc$status))
        ),
        dep_item_ui("Conda env",     ok = !is.null(dc$conda_active),       detail = dc$conda_active %||% "?"),
        dep_item_ui("R",             ok = !is.null(dc$r_path),             detail = dc$r_path %||% "?"),
        dep_item_ui("Python",        ok = !is.null(dc$python_path),        detail = dc$python_path %||% "non trouvé"),
        dep_item_ui("pyGenomeTracks",
          ok = !is.null(dc$pygenometracks_path),
          detail = paste0(dc$pygenometracks_path %||% "non trouvé",
                 if (!is.null(dc$pygenometracks_version)) paste0(" (v", dc$pygenometracks_version, ")") else "")
        ),
        dep_item_ui("rGenomeTracks", ok = isTRUE(dc$rGenomeTracks), detail = if (isTRUE(dc$rGenomeTracks)) "disponible" else "manquant"),
        dep_item_ui("BEDTools",      ok = isTRUE(dc$bedtools),       detail = if (isTRUE(dc$bedtools)) "disponible" else "non trouvé"),
        shiny::tags$small(class = "text-muted", sprintf("Vérifié le : %s", dc$checked_at %||% "?"))
      )
    })

    output$paths_ui <- shiny::renderUI({
      root     <- app_state$projects_root %||% "projects"
      app_root <- tryCatch(get_app_root(), error = function(e) "<erreur>")
      labels <- c("Racine", "Projets", "Logs", "Config", "Templates")
      paths  <- c(
        app_root,
        normalizePath(root,          mustWork = FALSE),
        normalizePath("logs",        mustWork = FALSE),
        normalizePath("config",      mustWork = FALSE),
        normalizePath("templates",   mustWork = FALSE)
      )
      shiny::tagList(
        mapply(function(lbl, pth) {
          shiny::tags$div(class = "mb-2",
            shiny::tags$small(class = "text-muted d-block", lbl),
            path_block(pth)
          )
        }, labels, paths, SIMPLIFY = FALSE)
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
