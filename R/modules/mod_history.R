# =============================================================================
# mod_history.R — Run history module
# =============================================================================

#' History module UI
#'
#' @param id module namespace ID
#' @export
mod_history_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Historique des runs"),
    shiny::fluidRow(
      shiny::column(3,
        shiny::selectInput(ns("filter_project"), "Filtrer par projet",
                           choices = c("Tous" = ""), width = "100%"),
        shiny::actionButton(ns("btn_refresh"), "Actualiser", class = "btn btn-sm btn-outline-secondary")
      ),
      shiny::column(9,
        DT::DTOutput(ns("history_table"))
      )
    ),
    shiny::hr(),
    shiny::fluidRow(
      shiny::column(12,
        shiny::fluidRow(
          shiny::column(2, shinyjs::disabled(shiny::actionButton(ns("btn_view"),   "Voir",           class = "btn btn-primary"))),
          shiny::column(2, shinyjs::disabled(shiny::actionButton(ns("btn_dup"),    "Dupliquer config",class = "btn btn-outline-secondary"))),
          shiny::column(2, shinyjs::disabled(shiny::actionButton(ns("btn_rerun"),  "Relancer",       class = "btn btn-outline-success"))),
          shiny::column(2, shinyjs::disabled(shiny::actionButton(ns("btn_delete"), "Supprimer",      class = "btn btn-outline-danger")))
        )
      )
    ),
    shiny::br(),
    shiny::uiOutput(ns("run_detail_ui"))
  )
}

#' History module server
#'
#' @param id module namespace ID
#' @param app_state reactive values
#' @param schema schema list
#' @export
mod_history_server <- function(id, app_state, schema) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    all_runs  <- shiny::reactiveVal(data.frame())
    confirm_delete <- shiny::reactiveVal(FALSE)

    refresh_history <- function() {
      root <- app_state$projects_root %||% "projects"
      if (!dir.exists(root)) { all_runs(data.frame()); return() }
      projects <- tryCatch(list_projects(root), error = function(e) data.frame())
      if (nrow(projects) == 0) { all_runs(data.frame()); return() }
      runs_list <- lapply(seq_len(nrow(projects)), function(i) {
        r <- tryCatch(list_runs(projects$project_path[i]), error = function(e) NULL)
        if (!is.null(r) && nrow(r) > 0) {
          r$project_name <- projects$project_name[i]
          r
        }
      })
      runs_list <- Filter(Negate(is.null), runs_list)
      if (length(runs_list) == 0) { all_runs(data.frame()); return() }
      combined <- do.call(rbind, runs_list)
      combined <- combined[order(combined$created_at, decreasing = TRUE), ]
      all_runs(combined)
    }

    shiny::observe({ refresh_history() })
    shiny::observeEvent(input$btn_refresh, { refresh_history() })

    # Populate project filter
    shiny::observe({
      df <- all_runs()
      if (nrow(df) == 0) return()
      projects <- unique(df$project_name)
      shiny::updateSelectInput(session, "filter_project",
                               choices = c("Tous" = "", setNames(projects, projects)))
    })

    filtered_runs <- shiny::reactive({
      df <- all_runs()
      filt <- input$filter_project
      if (is.null(filt) || nchar(filt) == 0) return(df)
      df[df$project_name == filt, ]
    })

    output$history_table <- DT::renderDT({
      df <- filtered_runs()
      if (nrow(df) == 0) return(data.frame(message = "Aucun run trouvé."))
      cols <- intersect(c("project_name", "run_name", "region", "renderer",
                           "status", "created_at", "run_path"), names(df))
      df[, cols, drop = FALSE]
    }, selection = "single", options = list(pageLength = 15, scrollX = TRUE))

    shiny::observeEvent(input$history_table_rows_selected, {
      for (b in c("btn_view", "btn_dup", "btn_rerun", "btn_delete")) shinyjs::enable(b)
    })

    selected_run_row <- shiny::reactive({
      sel <- input$history_table_rows_selected
      if (is.null(sel)) return(NULL)
      filtered_runs()[sel, ]
    })

    output$run_detail_ui <- shiny::renderUI({
      row <- selected_run_row()
      if (is.null(row)) return(NULL)
      meta <- tryCatch(load_run_metadata(row$run_path), error = function(e) NULL)
      if (is.null(meta)) return(shiny::p("Impossible de charger les métadonnées."))
      shiny::wellPanel(
        shiny::h5("Détails du run"),
        shiny::tags$dl(
          shiny::tags$dt("ID"),       shiny::tags$dd(shiny::code(meta$run_id %||% "?")),
          shiny::tags$dt("Nom"),      shiny::tags$dd(meta$run_name %||% "?"),
          shiny::tags$dt("Région"),   shiny::tags$dd(meta$region %||% "?"),
          shiny::tags$dt("Renderer"), shiny::tags$dd(meta$renderer %||% "?"),
          shiny::tags$dt("Statut"),   shiny::tags$dd(meta$status %||% "?"),
          shiny::tags$dt("Créé"),     shiny::tags$dd(meta$created_at %||% "?"),
          shiny::tags$dt("Terminé"),  shiny::tags$dd(meta$completed_at %||% "—"),
          shiny::tags$dt("Chemin"),   shiny::tags$dd(shiny::code(row$run_path))
        )
      )
    })

    shiny::observeEvent(input$btn_view, {
      row <- selected_run_row()
      if (is.null(row)) return()
      app_state$last_run_path <- row$run_path
      app_state$nav_to <- "results"
      shiny::showNotification("Run chargé dans 'Résultats'.", type = "message")
    })

    shiny::observeEvent(input$btn_dup, {
      row <- selected_run_row()
      if (is.null(row)) return()
      proj <- app_state$project_config
      if (is.null(proj)) { shiny::showNotification("Aucun projet actif.", type = "error"); return() }
      new_name <- paste0(row$run_name, "_dup")
      tryCatch({
        new_meta <- duplicate_run_config(row$run_path, proj, new_name)
        refresh_history()
        shiny::showNotification(sprintf("Configuration dupliquée : %s", new_meta$run_id), type = "message")
      }, error = function(e) {
        shiny::showNotification(sprintf("Erreur : %s", e$message), type = "error")
      })
    })

    shiny::observeEvent(input$btn_rerun, {
      row  <- selected_run_row()
      if (is.null(row)) return()
      meta <- tryCatch(load_run_metadata(row$run_path), error = function(e) NULL)
      if (is.null(meta)) return()
      fs <- app_state$figure_settings %||% list()
      renderer <- meta$renderer %||% fs$renderer %||% "pyGenomeTracks"
      shiny::withProgress(message = "Relancement...", {
        tryCatch({
          run_analysis(row$run_path, renderer)
          app_state$last_run_path <- row$run_path
          refresh_history()
          shiny::showNotification(
            shiny::HTML("Run relancé. <a href='#'>Consultez 'Résultats'</a> pour voir la figure."),
            type = "message", duration = 6)
        }, error = function(e) {
          shiny::showNotification(sprintf("Erreur : %s", e$message), type = "error")
        })
      })
    })

    shiny::observeEvent(input$btn_delete, {
      row <- selected_run_row()
      if (is.null(row)) return()
      shiny::showModal(shiny::modalDialog(
        title = "Confirmer la suppression",
        shiny::p(sprintf("Supprimer définitivement le run : %s ?", row$run_path)),
        footer = shiny::tagList(
          shiny::modalButton("Annuler"),
          shiny::actionButton(ns("btn_confirm_delete"), "Supprimer", class = "btn btn-danger")
        )
      ))
    })

    shiny::observeEvent(input$btn_confirm_delete, {
      shiny::removeModal()
      row <- selected_run_row()
      if (is.null(row)) return()
      tryCatch({
        delete_run(row$run_path, confirm = TRUE)
        refresh_history()
        shiny::showNotification("Run supprimé.", type = "message")
      }, error = function(e) {
        shiny::showNotification(sprintf("Erreur : %s", e$message), type = "error")
      })
    })
  })
}
