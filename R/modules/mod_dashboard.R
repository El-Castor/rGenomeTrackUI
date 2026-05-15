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
  shiny::tags$div(
    class = "dashboard-page",

    # ── Hero (section sémantique) ─────────────────────────────────────────
    shiny::tags$section(
      class = "dashboard-hero",
      shiny::tags$div(
        class = "dashboard-hero-content",
        shiny::tags$h1(
          shiny::tags$span(class = "accent", "rGenome"),
          "TrackUI"
        ),
        shiny::tags$p(
          "Build reproducible genomic track figures from BED, BigWig, GTF, peaks and regions."
        ),
        shiny::tags$div(
          class = "dashboard-hero-actions",
          shiny::actionButton(
            ns("btn_demo"),
            shiny::tagList(shiny::icon("flask"), " Demo project"),
            class = "btn btn-primary btn-cta"
          ),
          shiny::actionButton(
            ns("btn_new_project"),
            shiny::tagList(shiny::icon("folder-plus"), " New project"),
            class = "btn btn-secondary btn-cta"
          )
        )
      )
    ),

    # ── Stepper (dynamic) ─────────────────────────────────────────────────
    shiny::uiOutput(ns("workflow_steps_ui")),

    # ── Dashboard grid ────────────────────────────────────────────────────
    shiny::tags$div(
      class = "dashboard-grid",

      # ── Main column ──────────────────────────────────────────────────
      shiny::tags$main(
        class = "dashboard-main",

        # Métrique cards (chaque renderUI retourne un .rt-metric-card)
        shiny::tags$div(
          class = "dashboard-metric-grid",
          shiny::uiOutput(ns("stat_inputs")),
          shiny::uiOutput(ns("stat_tracks")),
          shiny::uiOutput(ns("stat_regions"))
        ),

        # Projet actif (renderUI retourne .rt-card.project-card complet)
        shiny::uiOutput(ns("active_project_ui")),

        # Quick actions (card statique)
        shiny::tags$div(
          class = "rt-card quick-actions-card",
          shiny::tags$span(class = "rt-card-title", "Actions rapides"),
          shiny::tags$div(
            class = "quick-actions",
            shiny::actionButton(
              ns("btn_demo"),
              shiny::tagList(shiny::icon("flask"), " Charger un projet d\u00e9mo"),
              class = "btn btn-secondary"
            ),
            shiny::actionButton(
              ns("btn_new_project"),
              shiny::tagList(shiny::icon("folder-plus"), " Nouveau projet"),
              class = "btn btn-secondary"
            ),
            shiny::actionButton(
              ns("btn_docs"),
              shiny::tagList(shiny::icon("book"), " Documentation"),
              class = "btn btn-secondary"
            ),
            shiny::downloadButton(
              ns("dl_templates_dash"),
              shiny::tagList(shiny::icon("download"), " Templates (ZIP)"),
              class = "btn btn-secondary"
            )
          )
        )
      ),

      # ── Side column ──────────────────────────────────────────────────
      shiny::tags$aside(
        class = "dashboard-side",
        # D\u00e9pendances (renderUI retourne .rt-card.dependencies-card complet)
        shiny::uiOutput(ns("dep_status_ui")),
        # Avertissements (renderUI retourne .rt-card.warnings-card complet)
        shiny::uiOutput(ns("current_warnings_ui"))
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

    output$dep_status_ui <- shiny::renderUI({
      dc <- dep_check()
      overall_ok <- identical(dc$status, "OK")

      make_dep_row <- function(label, ok, detail = NULL) {
        icon_name <- if (isTRUE(ok)) "check-circle" else "times-circle"
        icon_color <- if (isTRUE(ok)) "var(--rt-success)" else "var(--rt-danger)"
        shiny::tags$div(
          class = "dep-row",
          shiny::tags$span(
            style = paste0("color:", icon_color, ";"),
            shiny::icon(icon_name)
          ),
          shiny::tags$div(
            class = "dep-main",
            shiny::tags$div(class = "dep-label", label),
            if (!is.null(detail))
              shiny::tags$div(class = "dep-value", detail)
          )
        )
      }

      shiny::tags$div(
        class = "rt-card dependencies-card",
        shiny::tags$div(
          class = "dep-card-header",
          shiny::tags$div(
            class = "dep-card-title-row",
            shiny::tags$span(class = "rt-card-title", "D\u00e9pendances"),
            status_badge(if (overall_ok) "ok" else "error", label = dc$status)
          ),
          shiny::actionButton(
            session$ns("btn_recheck"),
            shiny::tagList(shiny::icon("sync"), " Refresh"),
            class = "btn btn-secondary btn-sm"
          )
        ),
        make_dep_row("R", TRUE, dc$r_path %||% "?"),
        make_dep_row("Python",
          !is.null(dc$python_path) && !grepl("non trouv\u00e9|not found",
            dc$python_path %||% "", ignore.case = TRUE),
          dc$python_path %||% "non trouv\u00e9"),
        make_dep_row("pyGenomeTracks",
          !is.null(dc$pygenometracks_path) && !grepl("non trouv\u00e9|not found",
            dc$pygenometracks_path %||% "", ignore.case = TRUE),
          if (!is.null(dc$pygenometracks_version))
            paste0("v", dc$pygenometracks_version) else "non trouv\u00e9"),
        make_dep_row("rGenomeTracks",
          isTRUE(dc$rGenomeTracks),
          if (isTRUE(dc$rGenomeTracks)) "disponible" else "manquant"),
        make_dep_row("BEDTools",
          isTRUE(dc$bedtools),
          if (isTRUE(dc$bedtools)) "disponible" else "non trouv\u00e9"),
        make_dep_row("Conda env",
          !is.null(dc$conda_active) && dc$conda_active != "base",
          dc$conda_active %||% "inconnu")
      )
    })

    # ---- Stat cards ----
    output$stat_inputs <- shiny::renderUI({
      n <- if (!is.null(app_state$registry)) nrow(app_state$registry) else 0
      info_card("Input files", n,
                subtitle = if (n == 0) "No files yet" else "registered",
                icon_name = "file-alt", color = "blue")
    })

    output$stat_tracks <- shiny::renderUI({
      n <- length(app_state$tracks)
      info_card("Tracks", n,
                subtitle = if (n == 0) "No tracks yet" else "configured",
                icon_name = "layer-group", color = "violet")
    })

    output$stat_regions <- shiny::renderUI({
      n <- length(app_state$regions %||% character(0))
      info_card("Regions", n,
                subtitle = if (n == 0) "No regions yet" else "defined",
                icon_name = "map-marker-alt", color = "accent")
    })

    # ---- Workflow steps ----
    output$workflow_steps_ui <- shiny::renderUI({
      proj    <- app_state$project_config
      reg     <- app_state$registry
      tracks  <- app_state$tracks
      regions <- app_state$regions %||% character(0)
      fs      <- app_state$figure_settings %||% list()

      step_names  <- c("Project", "Inputs", "Tracks", "Regions", "Preview", "Run")
      nav_targets <- c("project", "inputs", "track_builder", "regions", "preview", "run")

      step_done <- c(
        !is.null(proj),
        !is.null(reg) && nrow(reg %||% data.frame()) > 0,
        length(tracks) > 0,
        length(regions) > 0,
        !is.null(fs$renderer),
        !is.null(app_state$last_run_path)
      )
      first_todo <- which(!step_done)
      active_idx <- if (length(first_todo) > 0) first_todo[1] else length(step_names) + 1

      step_states <- setNames(
        lapply(seq_along(step_names), function(i) {
          if (step_done[i]) "done" else if (i == active_idx) "active" else ""
        }),
        step_names
      )

      workflow_stepper(
        steps    = step_names,
        current  = active_idx,
        states   = step_states,
        nav_ids  = paste0("wf_step_", seq_along(step_names))
      )
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
      body_content <- if (length(warns) == 0) {
        shiny::tags$p(
          class = "rt-card-muted",
          style = "margin:0.5rem 0 0;",
          shiny::icon("check-circle"),
          " No active warnings."
        )
      } else {
        shiny::tags$ul(
          class = "warning-list",
          lapply(warns, function(w) shiny::tags$li(w))
        )
      }
      shiny::tags$div(
        class = "rt-card warnings-card",
        shiny::tags$span(class = "rt-card-title", "Avertissements"),
        body_content
      )
    })

    # ---- Projet actif ----
    output$active_project_ui <- shiny::renderUI({
      proj <- app_state$project_config
      body_content <- if (is.null(proj)) {
        empty_state(
          "Aucun projet actif",
          "Créez un nouveau projet ou ouvrez un projet existant pour commencer.",
          shiny::actionButton(
            session$ns("btn_new_project"),
            shiny::tagList(shiny::icon("folder-plus"), " Créer un projet"),
            class = "btn btn-primary"
          ),
          icon_name = "folder-open"
        )
      } else {
        shiny::tags$div(
          class = "flex-row gap-12 flex-wrap",
          shiny::tags$div(
            style = "flex:1;min-width:200px;",
            shiny::tags$div(
              style = "display:grid;grid-template-columns:auto 1fr;gap:4px 12px;align-items:baseline;",
              shiny::tags$span(style = "font-size:11px;font-weight:600;color:var(--rt-muted);text-transform:uppercase;", "Nom"),
              shiny::tags$span(style = "font-weight:600;", proj$project_name %||% "?"),
              shiny::tags$span(style = "font-size:11px;font-weight:600;color:var(--rt-muted);text-transform:uppercase;", "Génome"),
              shiny::tags$span(
                status_badge("accent", label = proj$genome_label %||% "?", show_dot = FALSE)
              ),
              shiny::tags$span(style = "font-size:11px;font-weight:600;color:var(--rt-muted);text-transform:uppercase;", "Description"),
              shiny::tags$span(style = "color:var(--rt-muted);", proj$description %||% "—")
            )
          ),
          shiny::tags$div(
            style = "flex:1;min-width:200px;",
            shiny::tags$div(
              style = "font-size:11px;font-weight:600;color:var(--rt-muted);text-transform:uppercase;margin-bottom:4px;",
              "Chemin"
            ),
            path_block(proj$project_path %||% "?")
          )
        )
      }
      shiny::tags$div(
        class = "rt-card project-card",
        shiny::tags$div(
          class = "dep-card-header",
          shiny::tags$div(
            class = "dep-card-title-row",
            shiny::tags$span(class = "rt-card-title", "Projet actif")
          ),
          if (!is.null(proj))
            shiny::tags$div(
              class = "d-flex gap-2",
              shiny::actionButton(
                session$ns("btn_open_project"),
                shiny::tagList(shiny::icon("folder-open"), " Ouvrir"),
                class = "btn btn-secondary btn-sm"
              ),
              shiny::actionButton(
                session$ns("btn_history"),
                shiny::tagList(shiny::icon("history"), " Historique"),
                class = "btn btn-secondary btn-sm"
              )
            )
        ),
        body_content
      )
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
