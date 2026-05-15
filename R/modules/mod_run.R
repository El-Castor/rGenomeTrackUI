# =============================================================================
# mod_run.R — Run execution module
# =============================================================================

#' Run module UI
#'
#' @param id module namespace ID
#' @export
mod_run_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(

    omics_banner(
      "Lancer un run",
      "Préparez et exécutez l'analyse pyGenomeTracks ou rGenomeTracks.",
      small = TRUE
    ),

    shiny::fluidRow(
      # Left: config + checklist + buttons
      shiny::column(5,
        shiny::tags$div(
          class = "rt-card",
          shiny::tags$div(
            class = "rt-card-header",
            shiny::tags$span(class = "rt-card-icon", shiny::icon("play-circle")),
            shiny::tags$h5("Configuration du run")
          ),
          shiny::textInput(ns("run_name"), "Nom du run",
                           placeholder = "ex: H3K27ac_GENE1_test"),
          shiny::tags$small(class = "text-muted", "Laissez vide pour un nom auto-généré."),
          shiny::tags$hr(class = "divider"),
          shiny::tags$div(
            class = "param-section-title",
            shiny::icon("clipboard-check"), " Validation avant lancement"
          ),
          shiny::uiOutput(ns("pre_run_checklist")),
          shiny::tags$hr(class = "divider"),
          shiny::actionButton(ns("btn_prepare"),
            shiny::tagList(shiny::icon("cogs"), " Préparer le run"),
            class = "btn btn-secondary w-100"),
          shiny::div(class = "mt-2"),
          shinyjs::disabled(
            shiny::actionButton(ns("btn_run"),
              shiny::tagList(shiny::icon("rocket"), " Lancer l'analyse"),
              class = "btn btn-primary w-100 btn-cta")
          ),
          shiny::div(class = "mt-3"),
          shiny::uiOutput(ns("run_path_display"))
        )
      ),
      # Right: status + logs
      shiny::column(7,
        shiny::tags$div(
          class = "rt-card",
          shiny::tags$div(
            class = "rt-card-header",
            shiny::tags$span(class = "rt-card-icon", shiny::icon("terminal")),
            shiny::tags$h5("Statut & logs")
          ),
          shiny::uiOutput(ns("run_status_ui")),
          shiny::uiOutput(ns("btn_goto_results_ui")),
          shiny::tags$div(class = "mt-2"),
          bslib::navset_tab(
            bslib::nav_panel(
              shiny::tagList(shiny::icon("align-left"), " stdout"),
              shiny::tags$div(class = "mt-2",
                code_box(ns("log_stdout"), lang = "log", title = "Standard output"))
            ),
            bslib::nav_panel(
              shiny::tagList(shiny::icon("exclamation-circle"), " stderr"),
              shiny::tags$div(class = "mt-2",
                code_box(ns("log_stderr"), lang = "log", title = "Standard error"))
            )
          )
        )
      )
    )
  )
}

#' Run module server
#'
#' @param id module namespace ID
#' @param app_state reactive values
#' @param schema schema list
#' @export
mod_run_server <- function(id, app_state, schema) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    run_status  <- shiny::reactiveVal("idle")  # idle | prepared | running | completed | failed
    current_run <- shiny::reactiveVal(NULL)

    # ---- Checklist pré-run ----
    check_readiness <- function() {
      proj     <- app_state$project_config
      regions  <- app_state$regions %||% character(0)
      fs       <- app_state$figure_settings %||% list()
      tracks   <- app_state$tracks
      enabled  <- Filter(function(t) isTRUE(t$enabled), tracks)
      reg      <- app_state$registry %||% data.frame()
      renderer <- fs$renderer %||% ""

      # Check renderer availability (only meaningful when renderer is configured)
      renderer_avail <- if (nchar(renderer) == 0) {
        TRUE   # "renderer configured" check already handles empty case
      } else if (renderer == "pyGenomeTracks") {
        nchar(Sys.which("pyGenomeTracks")) > 0
      } else if (renderer == "rGenomeTracks") {
        requireNamespace("rGenomeTracks", quietly = TRUE)
      } else if (renderer == "both") {
        nchar(Sys.which("pyGenomeTracks")) > 0 &&
          requireNamespace("rGenomeTracks", quietly = TRUE)
      } else {
        FALSE
      }

      # Check source file existence for tracks that reference a file
      missing_files <- character(0)
      if (length(enabled) > 0 && is.data.frame(reg) && nrow(reg) > 0) {
        for (t in enabled) {
          fid <- t$file_id %||% ""
          if (nchar(fid) > 0) {
            entry <- tryCatch(get_file_by_id(reg, fid), error = function(e) NULL)
            if (!is.null(entry) && nchar(entry$stored_path %||% "") > 0 &&
                !file.exists(entry$stored_path)) {
              missing_files <- c(missing_files, entry$original_name %||% fid)
            }
          }
        }
      }
      files_with_refs <- sum(vapply(enabled, function(t) nchar(t$file_id %||% "") > 0, logical(1)))

      list(
        list(ok = !is.null(proj),
             label = "Projet actif",
             msg   = if (is.null(proj)) "Aucun projet — allez dans 'Projets'" else proj$project_name),
        list(ok = length(regions) > 0,
             label = "Région(s)",
             msg   = if (length(regions) == 0) "Ajoutez au moins une région" else sprintf("%d région(s)", length(regions))),
        list(ok = length(enabled) > 0,
             label = "Track(s) activée(s)",
             msg   = if (length(enabled) == 0) "Activez au moins une track" else sprintf("%d track(s)", length(enabled))),
        list(ok = nchar(renderer) > 0,
             label = "Renderer configuré",
             msg   = if (nchar(renderer) == 0) "Sélectionnez un renderer dans 'Régions & Figure'"
                     else renderer),
        list(ok = renderer_avail,
             label = "Renderer disponible",
             msg   = if (!renderer_avail)
                       sprintf("%s introuvable — vérifiez l'environnement Conda", renderer)
                     else if (nchar(renderer) == 0) "Configurez un renderer d'abord"
                     else sprintf("%s détecté", renderer)),
        list(ok = length(missing_files) == 0,
             label = "Fichiers sources",
             msg   = if (length(missing_files) == 0) {
               if (files_with_refs == 0) "Aucun fichier requis"
               else sprintf("%d fichier(s) présent(s)", files_with_refs)
             } else {
               sprintf("Introuvable(s) : %s", paste(missing_files, collapse = ", "))
             })
      )
    }

    output$pre_run_checklist <- shiny::renderUI({
      checks <- check_readiness()
      items <- lapply(checks, function(chk) {
        checklist_item(
          shiny::tagList(
            shiny::tags$span(style = "font-weight:600;", chk$label),
            shiny::tags$span(style = "color:var(--rt-muted);font-size:11px;margin-left:6px;",
                             paste0("— ", chk$msg))
          ),
          status = if (chk$ok) "ok" else "error"
        )
      })
      checklist_ui(items)
    })

    output$run_status_ui <- shiny::renderUI({
      st <- run_status()
      status_badge(st, label = switch(st,
        "idle"      = "En attente",
        "prepared"  = "Prêt à lancer",
        "running"   = "En cours…",
        "completed" = "Terminé avec succès",
        "failed"    = "Échec",
        st
      ))
    })

    output$run_path_display <- shiny::renderUI({
      meta <- current_run()
      if (is.null(meta)) return(NULL)
      shiny::tags$div(
        shiny::tags$div(
          style = "font-size:11px;font-weight:600;color:var(--rt-muted);text-transform:uppercase;margin-bottom:4px;",
          shiny::icon("folder-open"), " Dossier du run"
        ),
        path_block(meta$run_path)
      )
    })

    # ---- Préparer le run ----
    shiny::observeEvent(input$btn_prepare, {
      checks  <- check_readiness()
      errors  <- Filter(function(c) !c$ok, checks)
      if (length(errors) > 0) {
        msgs <- paste(vapply(errors, function(e) paste0("• ", e$label, " : ", e$msg), character(1)),
                      collapse = "\n")
        shiny::showNotification(
          shiny::HTML(paste0("<strong>Configuration incomplète :</strong><br>",
                             paste(vapply(errors, function(e) paste0("• ", e$msg), character(1)),
                                   collapse = "<br>"))),
          type = "error", duration = 8
        )
        return()
      }

      proj    <- app_state$project_config
      regions <- app_state$regions
      fs      <- app_state$figure_settings %||% list()
      tracks  <- app_state$tracks
      enabled <- Filter(function(t) isTRUE(t$enabled), tracks)
      reg     <- app_state$registry

      run_name <- trimws(input$run_name %||% "")
      if (nchar(run_name) == 0) run_name <- paste0("run_", format(Sys.time(), "%H%M%S"))
      renderer <- fs$renderer %||% "pyGenomeTracks"

      shiny::withProgress(message = "Préparation du run…", {
        tryCatch({
          meta <- create_run(proj, run_name, regions[1], renderer)
          prepare_run_files(proj, meta$run_path, enabled, schema,
                            reg %||% data.frame(), regions, fs)
          current_run(meta)
          run_status("prepared")
          app_state$current_run <- meta
          shinyjs::enable("btn_run")
          shiny::showNotification(
            shiny::HTML(sprintf("<strong>Run préparé !</strong><br>Dossier : <code>%s</code>", meta$run_path)),
            type = "message", duration = 5
          )
        }, error = function(e) {
          run_status("failed")
          shiny::showNotification(sprintf("Erreur préparation : %s", e$message), type = "error", duration = 10)
        })
      })
    })

    # ---- Lancer le run ----
    shiny::observeEvent(input$btn_run, {
      meta <- current_run()
      if (is.null(meta)) return()
      fs       <- app_state$figure_settings %||% list()
      renderer <- fs$renderer %||% "pyGenomeTracks"

      run_status("running")
      shinyjs::disable("btn_run")
      shinyjs::disable("btn_prepare")

      result <- shiny::withProgress(message = "Exécution en cours…", {
        tryCatch(
          run_analysis(meta$run_path, renderer),
          error = function(e) list(error = e$message)
        )
      })

      all_ok <- if (is.list(result) && !is.null(result$error)) FALSE
                else all(vapply(result, function(r) identical(r$status, "completed"), logical(1)))

      if (all_ok) {
        run_status("completed")
        app_state$last_run_path <- meta$run_path
        shiny::showNotification(
          shiny::HTML("Run terminé avec succès ! <strong><a href='#'>Voir les résultats</a></strong>"),
          type = "message", duration = 7)
      } else {
        run_status("failed")
        err_msg <- if (is.list(result) && !is.null(result$error)) result$error else "Voir les logs."
        shiny::showNotification(
          shiny::HTML(sprintf("<strong>Run échoué</strong><br>%s", err_msg)),
          type = "error", duration = 10
        )
      }
      shinyjs::enable("btn_prepare")
    })

    # ---- Bouton Voir les résultats (visible uniquement après succès) ----
    output$btn_goto_results_ui <- shiny::renderUI({
      if (!identical(run_status(), "completed")) return(NULL)
      shiny::div(class = "mt-2",
        shiny::actionButton(ns("btn_goto_results"),
          shiny::tagList(shiny::icon("chart-bar"), " Voir les résultats"),
          class = "btn btn-success btn-sm w-100")
      )
    })

    shiny::observeEvent(input$btn_goto_results, {
      app_state$nav_to <- "results"
    })

    # ---- Logs ----
    output$log_stdout <- shiny::renderText({
      meta <- current_run()
      if (is.null(meta)) return("(aucun log)")
      logs <- tryCatch(read_run_logs(meta$run_path), error = function(e) list(stdout = ""))
      logs$stdout %||% "(vide)"
    })

    output$log_stderr <- shiny::renderText({
      meta <- current_run()
      if (is.null(meta)) return("(aucun log)")
      logs <- tryCatch(read_run_logs(meta$run_path), error = function(e) list(stderr = ""))
      logs$stderr %||% "(vide)"
    })
  })
}
