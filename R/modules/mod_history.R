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

    omics_banner(
      "Historique des runs",
      "Retrouvez, dupliquez ou relancez vos analyses passées.",
      small = TRUE
    ),

    shiny::tags$div(
      class = "rt-card",
      shiny::tags$div(
        class = "rt-card-header flex-between",
        shiny::tags$div(
          class = "flex-row gap-8",
          shiny::tags$span(class = "rt-card-icon", shiny::icon("history")),
          shiny::tags$h5("Runs")
        ),
        shiny::tags$div(
          class = "d-flex gap-2 align-items-center",
          shiny::selectInput(ns("filter_project"), NULL,
                             choices = c("Tous les projets" = ""),
                             width = "200px"),
          shiny::actionButton(ns("btn_refresh"),
            shiny::tagList(shiny::icon("sync"), " Actualiser"),
            class = "btn btn-secondary btn-sm")
        )
      ),
      DT::DTOutput(ns("history_table")),
      shiny::tags$div(
        class = "mt-3 d-flex gap-2",
        shinyjs::disabled(shiny::actionButton(ns("btn_view"),
          shiny::tagList(shiny::icon("eye"), " Voir"),
          class = "btn btn-primary")),
        shinyjs::disabled(shiny::actionButton(ns("btn_dup"),
          shiny::tagList(shiny::icon("copy"), " Dupliquer"),
          class = "btn btn-secondary")),
        shinyjs::disabled(shiny::actionButton(ns("btn_rerun"),
          shiny::tagList(shiny::icon("redo"), " Relancer"),
          class = "btn btn-secondary")),
        shinyjs::disabled(shiny::actionButton(ns("btn_delete"),
          shiny::tagList(shiny::icon("trash"), " Supprimer"),
          class = "btn btn-danger"))
      )
    ),

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
      if (is.null(meta)) {
        return(shiny::tags$div(
          class = "alert alert-warning mt-2",
          shiny::icon("exclamation-triangle"),
          " Impossible de charger les métadonnées."
        ))
      }
      shiny::tags$div(
        class = "rt-card mt-2",
        shiny::tags$div(
          class = "rt-card-header",
          shiny::tags$span(class = "rt-card-icon", shiny::icon("info-circle")),
          shiny::tags$h5("Détails du run"),
          shiny::tags$div(class = "ms-auto", status_badge(meta$status %||% "unknown"))
        ),
        shiny::tags$div(
          style = "display:grid;grid-template-columns:auto 1fr;gap:5px 14px;align-items:baseline;",
          shiny::tags$span(style = "font-size:11px;font-weight:600;color:var(--rt-muted);text-transform:uppercase;", "ID"),
          shiny::tags$code(meta$run_id %||% "?"),
          shiny::tags$span(style = "font-size:11px;font-weight:600;color:var(--rt-muted);text-transform:uppercase;", "Nom"),
          shiny::tags$span(style = "font-weight:600;", meta$run_name %||% "?"),
          shiny::tags$span(style = "font-size:11px;font-weight:600;color:var(--rt-muted);text-transform:uppercase;", "Région"),
          shiny::tags$span(style = "font-family:var(--rt-font-mono);font-size:12px;", meta$region %||% "?"),
          shiny::tags$span(style = "font-size:11px;font-weight:600;color:var(--rt-muted);text-transform:uppercase;", "Renderer"),
          status_badge("info", label = meta$renderer %||% "?", show_dot = FALSE),
          shiny::tags$span(style = "font-size:11px;font-weight:600;color:var(--rt-muted);text-transform:uppercase;", "Créé"),
          shiny::tags$span(style = "color:var(--rt-muted);font-size:12px;", meta$created_at %||% "?"),
          shiny::tags$span(style = "font-size:11px;font-weight:600;color:var(--rt-muted);text-transform:uppercase;", "Terminé"),
          shiny::tags$span(style = "color:var(--rt-muted);font-size:12px;", meta$completed_at %||% "—"),
          shiny::tags$span(style = "font-size:11px;font-weight:600;color:var(--rt-muted);text-transform:uppercase;", "Chemin"),
          path_block(row$run_path)
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
