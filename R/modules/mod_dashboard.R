# =============================================================================
# mod_dashboard.R — Dashboard module
# =============================================================================

#' Dashboard UI
#'
#' @param id module namespace ID
#' @return Shiny UI element
#' @export
mod_dashboard_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$head(shiny::tags$style(shiny::HTML("
      .workflow-step-dash { display:flex; align-items:flex-start; gap:0.75rem;
        padding:0.6rem 0.75rem; border-radius:6px; margin-bottom:0.5rem; }
      .workflow-step-dash.done   { background:#d1e7dd; }
      .workflow-step-dash.active { background:#cfe2ff; border-left:4px solid #0d6efd; }
      .workflow-step-dash.todo   { background:#f8f9fa; }
      .step-num-dash { font-weight:bold; min-width:1.5rem; color:#6c757d; }
    "))),
    shiny::fluidRow(
      shiny::column(12,
        shiny::h2(shiny::icon("chart-bar"), " rGenomeTrackUI"),
        shiny::p(shiny::em("Interface graphique pour pyGenomeTracks — visualisation de données génomiques multi-niveaux."),
                 class = "lead text-muted mb-3"),
        shiny::hr()
      )
    ),
    shiny::fluidRow(
      # ---- Colonne gauche : Workflow + Actions ----
      shiny::column(7,
        bslib::card(
          bslib::card_header(shiny::icon("route"), " Workflow — Par où commencer ?"),
          shiny::uiOutput(ns("workflow_steps_ui")),
          shiny::hr(),
          shiny::div(class = "d-flex gap-2 flex-wrap",
            shiny::actionButton(ns("btn_new_project"),
              shiny::icon("folder-plus"), " Créer un projet",
              class = "btn btn-primary btn-sm"),
            shiny::actionButton(ns("btn_open_project"),
              shiny::icon("folder-open"), " Ouvrir un projet",
              class = "btn btn-outline-secondary btn-sm"),
            shiny::actionButton(ns("btn_demo"),
              shiny::icon("flask"), " Projet démo",
              class = "btn btn-outline-info btn-sm"),
            shiny::actionButton(ns("btn_docs"),
              shiny::icon("book"), " Documentation",
              class = "btn btn-outline-dark btn-sm"),
            shiny::actionButton(ns("btn_history"),
              shiny::icon("history"), " Historique",
              class = "btn btn-outline-secondary btn-sm"),
            shiny::downloadButton(ns("dl_templates_dash"),
              "Templates (ZIP)",
              class = "btn btn-outline-secondary btn-sm")
          )
        ),
        bslib::card(
          bslib::card_header(shiny::icon("project-diagram"), " Projet actif"),
          shiny::uiOutput(ns("active_project_ui"))
        )
      ),
      # ---- Colonne droite : Dépendances ----
      shiny::column(5,
        bslib::card(
          bslib::card_header(shiny::icon("check-circle"), " État des dépendances"),
          shiny::actionButton(ns("btn_recheck"),
            shiny::icon("sync"), " Revérifier",
            class = "btn btn-outline-secondary btn-sm mb-2"),
          shiny::uiOutput(ns("dep_status_ui"))
        ),
        bslib::card(
          bslib::card_header(shiny::icon("exclamation-circle"), " Avertissements actifs"),
          shiny::uiOutput(ns("current_warnings_ui"))
        )
      )
    )
  )
}

#' Dashboard server
#'
#' @param id module namespace ID
#' @param app_state reactive values shared across modules
#' @export
mod_dashboard_server <- function(id, app_state) {
  shiny::moduleServer(id, function(input, output, session) {

    recheck_trigger <- shiny::reactiveVal(0)

    shiny::observeEvent(input$btn_recheck, {
      recheck_trigger(recheck_trigger() + 1)
    })

    # Dependency check
    dep_check <- shiny::reactive({
      recheck_trigger()
      shiny::withProgress(message = "Vérification des dépendances…", {
        run_dependency_check()
      })
    })

    dep_item <- function(icon_name, label, ok, detail = NULL) {
      cls <- if (ok) "text-success" else "text-danger"
      ico <- if (ok) "check-circle" else "times-circle"
      shiny::div(class = "d-flex align-items-start gap-2 mb-1",
        shiny::span(shiny::icon(ico), class = cls),
        shiny::div(
          shiny::strong(label),
          if (!is.null(detail)) shiny::div(class = "text-muted small", detail) else NULL
        )
      )
    }

    output$dep_status_ui <- shiny::renderUI({
      dc <- dep_check()
      overall_ok <- identical(dc$status, "OK")
      shiny::tagList(
        shiny::div(class = paste0("badge bg-", if (overall_ok) "success" else "danger", " mb-2"),
          dc$status),
        dep_item("r-project", "R", TRUE, dc$r_path %||% "?"),
        dep_item("python", "Python",
                 !is.null(dc$python_path) && !grepl("non trouvé|not found", dc$python_path %||% "", ignore.case = TRUE),
                 dc$python_path %||% "non trouvé"),
        dep_item("tools", "pyGenomeTracks",
                 !is.null(dc$pygenometracks_path) && !grepl("non trouvé|not found", dc$pygenometracks_path %||% "", ignore.case = TRUE),
                 if (!is.null(dc$pygenometracks_version)) paste0("v", dc$pygenometracks_version) else "non trouvé"),
        dep_item("cubes", "rGenomeTracks",
                 isTRUE(dc$rGenomeTracks),
                 if (isTRUE(dc$rGenomeTracks)) "package R disponible" else "manquant"),
        dep_item("cut", "BEDTools",
                 isTRUE(dc$bedtools),
                 if (isTRUE(dc$bedtools)) "disponible" else "non trouvé"),
        dep_item("terminal", "Conda env",
                 !is.null(dc$conda_active) && dc$conda_active != "base",
                 dc$conda_active %||% "inconnu")
      )
    })

    # ---- Workflow steps ----
    output$workflow_steps_ui <- shiny::renderUI({
      proj    <- app_state$project_config
      reg     <- app_state$registry
      tracks  <- app_state$tracks
      regions <- app_state$regions %||% character(0)
      fs      <- app_state$figure_settings %||% list()

      steps <- list(
        list(done = !is.null(proj),          label = "Créer / ouvrir un projet",         nav = "project"),
        list(done = !is.null(reg) && nrow(reg %||% data.frame()) > 0, label = "Importer des fichiers de données", nav = "inputs"),
        list(done = length(tracks) > 0,      label = "Configurer les tracks",             nav = "track_builder"),
        list(done = length(regions) > 0,     label = "Définir les régions",               nav = "regions"),
        list(done = !is.null(fs$renderer),   label = "Vérifier l'aperçu de config",       nav = "preview"),
        list(done = !is.null(app_state$last_run_path), label = "Lancer et voir les résultats", nav = "run")
      )

      # Trouver la première étape non faite
      first_todo <- which(!vapply(steps, function(s) isTRUE(s$done), logical(1)))
      active_idx <- if (length(first_todo) > 0) first_todo[1] else length(steps) + 1

      items <- lapply(seq_along(steps), function(i) {
        s <- steps[[i]]
        status_cls <- if (isTRUE(s$done)) "done" else if (i == active_idx) "active" else "todo"
        ico <- if (isTRUE(s$done)) shiny::icon("check-circle", class = "text-success")
               else if (i == active_idx) shiny::icon("arrow-right", class = "text-primary")
               else shiny::icon("circle", class = "text-muted")
        lnk <- shiny::actionLink(session$ns(paste0("wf_step_", i)), s$label,
                                  class = if (i == active_idx) "fw-bold" else "")
        shiny::div(class = paste("workflow-step-dash", status_cls),
          shiny::div(class = "step-num-dash", sprintf("%d.", i)),
          ico,
          lnk
        )
      })

      shiny::tagList(items)
    })

    # Navigation depuis les liens du workflow
    for (i in seq_len(6)) {
      local({
        idx <- i
        nav_targets <- c("project", "inputs", "track_builder", "regions", "preview", "run")
        shiny::observeEvent(input[[paste0("wf_step_", idx)]], {
          app_state$nav_to <- nav_targets[idx]
        }, ignoreNULL = TRUE)
      })
    }

    # ---- Avertissements actifs ----
    output$current_warnings_ui <- shiny::renderUI({
      warns <- c()
      if (is.null(app_state$project_config))
        warns <- c(warns, "Aucun projet actif.")
      if (length(app_state$regions %||% character(0)) == 0)
        warns <- c(warns, "Aucune région définie.")
      if (length(app_state$tracks) == 0)
        warns <- c(warns, "Aucune track configurée.")
      dc <- tryCatch(dep_check(), error = function(e) NULL)
      if (!is.null(dc)) {
        if (!isTRUE(dc$bedtools))
          warns <- c(warns, "BEDTools non trouvé dans le PATH.")
        if (!isTRUE(dc$rGenomeTracks) &&
            is.null(dc$pygenometracks_path))
          warns <- c(warns, "Ni rGenomeTracks ni pyGenomeTracks disponible.")
        if (!is.null(dc$conda_active) && dc$conda_active == "base")
          warns <- c(warns, "Environnement conda 'base' actif — préférez 'rgenometrackui'.")
      }
      if (length(warns) == 0) {
        shiny::div(class = "text-success small", shiny::icon("check"), " Aucun avertissement.")
      } else {
        shiny::div(class = "alert alert-warning p-2",
          shiny::tags$ul(class = "mb-0 ps-3",
            lapply(warns, function(w) shiny::tags$li(class = "small", w)))
        )
      }
    })

    # ---- Projet actif ----
    output$active_project_ui <- shiny::renderUI({
      proj <- app_state$project_config
      if (is.null(proj)) {
        shiny::div(class = "text-muted",
          shiny::icon("info-circle"),
          " Aucun projet actif. Créez ou ouvrez un projet.")
      } else {
        shiny::tags$dl(class = "row mb-0",
          shiny::tags$dt(class = "col-4", "Nom"),
          shiny::tags$dd(class = "col-8", proj$project_name %||% "?"),
          shiny::tags$dt(class = "col-4", "Génome"),
          shiny::tags$dd(class = "col-8", proj$genome_label %||% "?"),
          shiny::tags$dt(class = "col-4", "Description"),
          shiny::tags$dd(class = "col-8", proj$description %||% "—"),
          shiny::tags$dt(class = "col-4", "Chemin"),
          shiny::tags$dd(class = "col-8", shiny::code(proj$project_path %||% "?"))
        )
      }
    })

    # ---- Boutons de navigation ----
    shiny::observeEvent(input$btn_new_project,  { app_state$nav_to <- "project" })
    shiny::observeEvent(input$btn_open_project, { app_state$nav_to <- "project" })
    shiny::observeEvent(input$btn_docs,         { app_state$nav_to <- "documentation" })
    shiny::observeEvent(input$btn_history,      { app_state$nav_to <- "history" })

    shiny::observeEvent(input$btn_demo, {
      shiny::withProgress(message = "Création du projet démo…", {
        tryCatch({
          root <- app_state$projects_root %||% file.path(get_app_root(), "projects")
          demo <- create_demo_project(projects_root = root)

          if (is.null(demo$project_config) || is.null(demo$project_config$project_path))
            stop("create_demo_project a retourné un project_config invalide.")

          app_state$project_config  <- demo$project_config
          app_state$registry        <- demo$registry
          app_state$tracks          <- demo$demo_tracks  %||% list()
          app_state$regions         <- vapply(
            demo$demo_regions %||% list(), function(r) r$region, character(1))
          app_state$figure_settings <- demo$demo_figure  %||% list()

          shiny::showNotification(
            shiny::HTML("<strong>Projet démo chargé !</strong><br>
                         Fichiers, tracks, régions et configuration pré-remplis."),
            type = "message", duration = 5)
          app_state$nav_to <- "track_builder"
        }, error = function(e) {
          shiny::showNotification(sprintf("Erreur démo : %s", e$message), type = "error")
        })
      })
    })

    output$dl_templates_dash <- shiny::downloadHandler(
      filename = function() "rGenomeTrackUI_input_templates.zip",
      content  = function(file) {
        zip_path <- "templates/input_files/rGenomeTrackUI_input_templates.zip"
        if (file.exists(zip_path)) file.copy(zip_path, file)
      }
    )
  })
}
