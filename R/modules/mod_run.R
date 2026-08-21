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
          shiny::uiOutput(ns("run_path_display")),
          shiny::uiOutput(ns("run_diagnostics_ui"))
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
    last_region_signature <- shiny::reactiveVal(NULL)
    last_can_launch_signature <- shiny::reactiveVal(NULL)

    set_run_button_state <- function(enabled) {
      shinyjs::toggleState(selector = paste0("#", ns("btn_run")), condition = isTRUE(enabled))
    }

    set_prepare_button_state <- function(enabled) {
      shinyjs::toggleState(selector = paste0("#", ns("btn_prepare")), condition = isTRUE(enabled))
    }

    renderer_available_cached <- function(renderer) {
      if (nchar(renderer %||% "") == 0) return(TRUE)
      cache <- app_state$cache %||% list()
      renderer_cache <- cache$renderer_status %||% list()
      key <- paste(renderer, Sys.getenv("PATH"), sep = "|")
      if (!is.null(renderer_cache[[key]])) {
        message("[CACHE] Renderer status cache hit")
        return(isTRUE(renderer_cache[[key]]$available))
      }
      available <- if (renderer == "pyGenomeTracks") {
        nchar(Sys.which("pyGenomeTracks")) > 0
      } else if (renderer == "rGenomeTracks") {
        requireNamespace("rGenomeTracks", quietly = TRUE)
      } else if (renderer == "both") {
        nchar(Sys.which("pyGenomeTracks")) > 0 &&
          requireNamespace("rGenomeTracks", quietly = TRUE)
      } else {
        FALSE
      }
      renderer_cache[[key]] <- list(
        renderer = renderer,
        available = available,
        checked_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
      )
      cache$renderer_status <- renderer_cache
      app_state$cache <- cache
      message("[CACHE] Renderer status cache miss")
      available
    }

    current_ui_region <- shiny::reactive({
      get_current_region(input, app_state)
    })

    selected_region_signature <- shiny::reactive({
      region_state_signature(app_state$regions %||% character(0))
    })

    shiny::observe({
      ready <- can_launch_run(app_state)
      set_run_button_state(ready)
      set_prepare_button_state(!isTRUE(app_state$is_preparing) && !isTRUE(app_state$is_running))
      sig <- paste(
        ready,
        isTRUE(app_state$prepared_run_ready),
        !is.null(app_state$prepared_config),
        !is.null(app_state$run_command),
        isTRUE(app_state$is_preparing),
        isTRUE(app_state$is_running),
        sep = "|"
      )
      if (!identical(sig, last_can_launch_signature())) {
        msg <- sprintf(
          "[STATE] can_launch_run=%s | prepared_run_ready=%s | has_config=%s | has_command=%s | is_preparing=%s | is_running=%s",
          ready,
          isTRUE(app_state$prepared_run_ready),
          !is.null(app_state$prepared_config),
          !is.null(app_state$run_command),
          isTRUE(app_state$is_preparing),
          isTRUE(app_state$is_running)
        )
        message(msg)
        prepared <- app_state$prepared_config %||% NULL
        if (!is.null(prepared$run_path) && dir.exists(prepared$run_path)) {
          log_info(prepared$run_path, msg)
        }
        last_can_launch_signature(sig)
      }
    })

    shiny::observeEvent(selected_region_signature(), {
      sig <- selected_region_signature()
      old_sig <- last_region_signature()
      region <- current_ui_region()
      if (!is.null(old_sig) && !identical(old_sig, sig)) {
        invalidate_prepared_run_state(app_state, region)
        current_run(NULL)
        run_status("idle")
        app_state$region_dirty <- TRUE
        app_state$config_dirty <- TRUE
        msg <- sprintf("[STATE] Region changed to %s. Prepared run invalidated.",
                       region %||% "<none>")
        message(msg)
      }
      last_region_signature(sig)
    }, ignoreInit = FALSE)

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
      renderer_avail <- renderer_available_cached(renderer)

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

    output$run_diagnostics_ui <- shiny::renderUI({
      prepared <- app_state$prepared_config %||% list()
      shiny::tags$div(
        class = "mt-3",
        shiny::tags$div(
          style = "font-size:11px;font-weight:600;color:var(--rt-muted);text-transform:uppercase;margin-bottom:4px;",
          shiny::icon("route"), " Diagnostic région/run"
        ),
        shiny::tags$div(
          style = "display:grid;grid-template-columns:auto 1fr;gap:4px 10px;align-items:baseline;font-size:12px;",
          shiny::tags$span(style = "color:var(--rt-muted);", "Région UI"),
          shiny::tags$code(current_ui_region() %||% "?"),
          shiny::tags$span(style = "color:var(--rt-muted);", "Dernier prepare"),
          shiny::tags$code(prepared$region %||% "?"),
          shiny::tags$span(style = "color:var(--rt-muted);", "Dernier run"),
          shiny::tags$code(app_state$last_run_region %||% "?"),
          shiny::tags$span(style = "color:var(--rt-muted);", "Config"),
          shiny::tags$code(prepared$config_file %||% "?"),
          shiny::tags$span(style = "color:var(--rt-muted);", "Image"),
          shiny::tags$code(app_state$last_output_file %||% prepared$output_file %||% "?"),
          shiny::tags$span(style = "color:var(--rt-muted);", "prepared_run_ready"),
          shiny::tags$code(as.character(isTRUE(app_state$prepared_run_ready))),
          shiny::tags$span(style = "color:var(--rt-muted);", "has prepared_config"),
          shiny::tags$code(as.character(!is.null(app_state$prepared_config))),
          shiny::tags$span(style = "color:var(--rt-muted);", "has run_command"),
          shiny::tags$code(as.character(!is.null(app_state$run_command))),
          shiny::tags$span(style = "color:var(--rt-muted);", "is_preparing"),
          shiny::tags$code(as.character(isTRUE(app_state$is_preparing))),
          shiny::tags$span(style = "color:var(--rt-muted);", "is_running"),
          shiny::tags$code(as.character(isTRUE(app_state$is_running))),
          shiny::tags$span(style = "color:var(--rt-muted);", "can_launch_run"),
          shiny::tags$code(as.character(can_launch_run(app_state)))
        ),
        if (identical(run_status(), "prepared") && !isTRUE(can_launch_run(app_state))) {
          shiny::tags$div(
            class = "alert alert-danger mt-2 mb-0 p-2",
            "Incohérence d'état : le statut indique prêt, mais le bouton Run est bloqué."
          )
        } else NULL
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
      regions <- app_state$regions %||% character(0)
      region  <- current_ui_region()
      if (is.null(region)) {
        shiny::showNotification("Aucune région courante disponible pour préparer le run.",
                                type = "error", duration = 8)
        return()
      }
      fs      <- app_state$figure_settings %||% list()
      tracks  <- app_state$tracks
      enabled <- Filter(function(t) isTRUE(t$enabled), tracks)
      reg     <- app_state$registry
      if (length(enabled) > 0L && is.data.frame(reg) && nrow(reg) > 0L) {
        enabled <- lapply(enabled, function(t) {
          fid <- t$file_id %||% ""
          if (nzchar(fid)) {
            entry <- tryCatch(get_file_by_id(reg, fid), error = function(e) NULL)
            if (!is.null(entry) && nzchar(entry$stored_path %||% "")) {
              t$file_path <- entry$stored_path
              t$file_name <- entry$original_name %||% basename(entry$stored_path)
            }
          }
          t
        })
      }

      run_name <- trimws(input$run_name %||% "")
      if (nchar(run_name) == 0) run_name <- paste0("run_", format(Sys.time(), "%H%M%S"))
      renderer <- fs$renderer %||% "pyGenomeTracks"
      n_signal_tracks <- sum(vapply(enabled, is_signal_track, logical(1)))
      state_hash <- hash_run_state(region, regions, enabled, fs, renderer)

      previous <- app_state$prepared_config %||% NULL
      if (!is.null(previous) &&
          identical(previous$hash %||% NULL, state_hash) &&
          !isTRUE(app_state$config_dirty) &&
          !is.null(previous$run_path) &&
          dir.exists(previous$run_path)) {
        meta <- tryCatch(load_run_metadata(previous$run_path), error = function(e) NULL)
        if (!is.null(meta)) {
          current_run(meta)
          run_status("prepared")
          app_state$prepared_run_ready <- TRUE
          app_state$last_prepare_status <- "ready"
          app_state$is_preparing <- FALSE
          app_state$is_running <- FALSE
          shiny::showNotification(
            "Run déjà préparé pour cette région et ces tracks.",
            type = "message", duration = 4
          )
          return()
        }
      }

      shinyjs::disable("btn_prepare")
      app_state$is_preparing <- TRUE
      app_state$is_running <- FALSE
      app_state$prepared_run_ready <- FALSE
      app_state$last_prepare_status <- "preparing"
      shiny::withProgress(message = "Préparation du run…", value = 0, {
        tryCatch({
          if (!is.null(previous$region) && !identical(previous$region, region)) {
            warning("Prepared config region mismatch. Rebuilding config.")
          }

          run_t0 <- Sys.time()
          message("[PREPARE] Starting prepare run")
          shiny::incProgress(0.1, detail = "Résolution région")
          region <- time_step(
            "Resolve region",
            current_ui_region(),
            project_config = proj, run_id = run_name, region = region,
            n_tracks = length(enabled), n_signal_tracks = n_signal_tracks
          )
          shiny::incProgress(0.15, detail = "Validation légère")
          time_step(
            "Validate project",
            {
              if (is.null(proj)) stop("No active project")
              TRUE
            },
            project_config = proj, run_id = run_name, region = region,
            n_tracks = length(enabled), n_signal_tracks = n_signal_tracks
          )
          time_step(
            "Validate tracks light",
            {
              validate_tracks_table(enabled)
              validate_files_light(enabled)
            },
            project_config = proj, run_id = run_name, region = region,
            n_tracks = length(enabled), n_signal_tracks = n_signal_tracks
          )
          renderer_ok <- time_step(
            "Check renderer",
            renderer_available_cached(renderer),
            project_config = proj, run_id = run_name, region = region,
            n_tracks = length(enabled), n_signal_tracks = n_signal_tracks
          )
          if (!isTRUE(renderer_ok)) stop(sprintf("Renderer unavailable: %s", renderer))

          shiny::incProgress(0.2, detail = "Création du run")
          meta <- time_step(
            "Create run directory",
            create_run(proj, run_name, region, renderer),
            project_config = proj, run_id = run_name, region = region,
            n_tracks = length(enabled), n_signal_tracks = n_signal_tracks
          )

          fs_prepare <- fs
          fs_prepare$apply_shared_scale_to_signal_tracks <- FALSE
          fs_prepare$compute_signal_scale_during_prepare <- COMPUTE_SIGNAL_SCALE_DURING_PREPARE
          fs_prepare$light_prepare <- TRUE

          shiny::incProgress(0.35, detail = "Écriture config légère")
          time_step(
            "Write config template",
            prepare_run_files(proj, meta$run_path, enabled, schema,
                              reg %||% data.frame(), regions, fs_prepare),
            run_path = meta$run_path, project_config = proj, run_id = run_name,
            region = region, n_tracks = length(enabled),
            n_signal_tracks = n_signal_tracks
          )
          output_file <- time_step(
            "Resolve output file",
            expected_output_file_for_region(meta$run_path, region, fs),
            run_path = meta$run_path, project_config = proj, run_id = run_name,
            region = region, n_tracks = length(enabled),
            n_signal_tracks = n_signal_tracks
          )
          command <- time_step(
            "Build command preview",
            build_pygenometracks_command_preview(meta$run_path, region, output_file, fs),
            run_path = meta$run_path, project_config = proj, run_id = run_name,
            region = region, n_tracks = length(enabled),
            n_signal_tracks = n_signal_tracks
          )
          app_state$prepared_config <- list(
            region = region,
            regions = regions,
            run_path = meta$run_path,
            config_file = file.path(meta$run_path, "config", "tracks.ini"),
            output_file = output_file,
            tracks = enabled,
            figure_settings = fs,
            prepare_figure_settings = fs_prepare,
            hash = state_hash,
            prepared_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
          )
          app_state$run_command <- command
          app_state$last_output_file <- NULL
          app_state$last_run_region <- NULL
          meta$prepared_region <- region
          meta$expected_output_file <- output_file
          meta$status <- "prepared"
          save_run_metadata(meta, meta$run_path)
          app_state$config_dirty <- FALSE
          app_state$region_dirty <- FALSE
          app_state$tracks_dirty <- FALSE
          log_info(meta$run_path, sprintf("[PREPARE] Region used: %s", region))
          log_info(meta$run_path, sprintf("[PREPARE] Config file: %s", app_state$prepared_config$config_file))
          log_info(meta$run_path, sprintf("[PREPARE] Expected output: %s", output_file))
          log_info(meta$run_path, sprintf("[PREPARE] Command: %s", command))
          log_info(meta$run_path, sprintf("[PREPARE] Done in %.3f sec",
                                          as.numeric(difftime(Sys.time(), run_t0, units = "secs"))))
          current_run(meta)
          run_status("prepared")
          app_state$current_run <- meta
          app_state$is_preparing <- FALSE
          app_state$prepared_run_ready <- TRUE
          app_state$last_prepare_status <- "ready"
          app_state$is_running <- FALSE
          log_info(meta$run_path, sprintf(
            "[STATE] prepared_run_ready=%s | can_launch_run=%s",
            isTRUE(app_state$prepared_run_ready),
            can_launch_run(app_state)
          ))
          shiny::showNotification(
            shiny::HTML(sprintf(
              "<strong>Préparation terminée — l’analyse n’est pas encore lancée.</strong><br>Cliquez maintenant sur <strong>« Lancer l’analyse »</strong>.<br>Dossier : <code>%s</code>",
              meta$run_path
            )),
            type = "message", duration = 5
          )
        }, error = function(e) {
          run_status("failed")
          app_state$prepared_run_ready <- FALSE
          app_state$last_prepare_status <- "failed"
          shiny::showNotification(sprintf("Erreur préparation : %s", e$message), type = "error", duration = 10)
        }, finally = {
          app_state$is_preparing <- FALSE
          shinyjs::enable("btn_prepare")
        })
      })
    })

    # ---- Lancer le run ----
    shiny::observeEvent(input$btn_run, {
      message("[RUN_CLICK] Launch analysis button received by server.")
      if (!can_launch_run(app_state)) {
        message("[RUN_CLICK] Rejected: prepared state is not launchable.")
        shiny::showNotification(
          "Le run n'est pas prêt. Cliquez d'abord sur Préparer le run.",
          type = "warning", duration = 6
        )
        return()
      }
      prepared <- app_state$prepared_config %||% NULL
      meta <- current_run() %||% app_state$current_run %||% NULL
      if (is.null(meta) && !is.null(prepared$run_path) && dir.exists(prepared$run_path)) {
        meta <- tryCatch(load_run_metadata(prepared$run_path), error = function(e) NULL)
      }
      if (is.null(meta)) {
        message("[RUN_CLICK] Rejected: run metadata could not be recovered.")
        shiny::showNotification(
          "Le run préparé existe, mais ses métadonnées sont introuvables. Préparez à nouveau le run.",
          type = "error", duration = 10
        )
        app_state$prepared_run_ready <- FALSE
        app_state$last_prepare_status <- "invalidated"
        return()
      }
      current_run(meta)
      app_state$current_run <- meta
      fs       <- app_state$figure_settings %||% list()
      renderer <- fs$renderer %||% "pyGenomeTracks"
      ui_region <- current_ui_region()

      if (is.null(prepared) || !identical(prepared$run_path, meta$run_path)) {
        shiny::showNotification("Le run préparé est introuvable. Cliquez à nouveau sur 'Préparer le run'.",
                                type = "error", duration = 8)
        current_run(NULL)
        run_status("idle")
        app_state$prepared_run_ready <- FALSE
        app_state$last_prepare_status <- "invalidated"
        return()
      }

      if (is.null(ui_region) || !identical(prepared$region, ui_region)) {
        msg <- sprintf(
          "Erreur: la région préparée (%s) ne correspond pas à la région sélectionnée (%s). Préparez à nouveau le run.",
          prepared$region %||% "?", ui_region %||% "?"
        )
        log_error(meta$run_path, sprintf("[ERROR] Region mismatch: UI region = %s | Prepared region = %s | Run region = %s",
                                         ui_region %||% "?", prepared$region %||% "?", prepared$region %||% "?"))
        shiny::showNotification(msg, type = "error", duration = 10)
        invalidate_prepared_run_state(app_state, ui_region)
        current_run(NULL)
        run_status("idle")
        return()
      }

      run_status("running")
      app_state$is_running <- TRUE
      app_state$last_prepare_status <- "running"
      update_run_status(meta$run_path, "running")
      message(sprintf("[RUN_CLICK] Starting %s for %s", renderer, meta$run_path))
      shinyjs::disable("btn_prepare")
      on.exit({
        app_state$is_running <- FALSE
        shinyjs::enable("btn_prepare")
      }, add = TRUE)
      fs_run <- prepared$figure_settings %||% fs
      cache_dir <- project_cache_dir(app_state$project_config)
      fs_run$signal_scaling_cache_file <- file.path(cache_dir, "signal_scaling_cache.rds")
      fs_run$annotation_cache_dir <- file.path(cache_dir, "annotations")
      output_file <- expected_output_file_for_region(meta$run_path, prepared$region, fs_run)
      command <- build_pygenometracks_command_preview(meta$run_path, prepared$region, output_file, fs_run)
      log_info(meta$run_path, sprintf("[RUN] Region used: %s", prepared$region))
      log_info(meta$run_path, "[RUN] Final config rebuild requested for current region/tracks")
      execution <- tryCatch(shiny::withProgress(
        message = "Analyse génomique en cours",
        value = 0,
        {
          shiny::incProgress(0.05, detail = "1/4 · Démarrage du run — durée habituelle : 2 à 5 min")
          log_info(meta$run_path, "[RUN_STAGE] 1/4 Démarrage confirmé")

          final_t0 <- Sys.time()
          shiny::incProgress(
            0.15,
            detail = sprintf(
              "2/4 · Échelles BigWig (%d tracks) + annotation — cache réutilisé si disponible",
              sum(vapply(prepared$tracks %||% list(), is_signal_track, logical(1)))
            )
          )
          log_info(meta$run_path, "[RUN_STAGE] 2/4 Calcul des échelles BigWig et config finale")
          final_error <- NULL
          final_ok <- tryCatch({
            time_step(
              "Compute signal scaling + write final config",
              prepare_run_files(app_state$project_config, meta$run_path,
                                prepared$tracks %||% list(), schema,
                                app_state$registry %||% data.frame(),
                                prepared$regions %||% prepared$region,
                                fs_run),
              run_path = meta$run_path,
              project_config = app_state$project_config,
              run_id = meta$run_id %||% "",
              region = prepared$region,
              n_tracks = length(prepared$tracks %||% list()),
              n_signal_tracks = sum(vapply(prepared$tracks %||% list(), is_signal_track, logical(1)))
            )
            TRUE
          }, error = function(e) {
            final_error <<- conditionMessage(e)
            FALSE
          })
          if (!isTRUE(final_ok)) {
            stop("Erreur génération config finale : ", final_error)
          }

          prepared$figure_settings <- fs_run
          prepared$output_file <- output_file
          app_state$prepared_config <- prepared
          app_state$run_command <- command
          log_info(meta$run_path, sprintf("[PERF] Final config rebuild total: %.3f sec",
                                          as.numeric(difftime(Sys.time(), final_t0, units = "secs"))))
          log_info(meta$run_path, sprintf("[RUN] Command: %s", app_state$run_command %||% "<script>"))

          shiny::incProgress(0.45, detail = "3/4 · Configuration terminée — rendu pyGenomeTracks en cours")
          log_info(meta$run_path, "[RUN_STAGE] 3/4 Lancement du moteur de rendu")
          render_result <- tryCatch(
            run_analysis(meta$run_path, renderer),
            error = function(e) list(error = conditionMessage(e))
          )
          shiny::incProgress(0.9, detail = "4/4 · Rendu terminé — vérification de la figure")
          log_info(meta$run_path, "[RUN_STAGE] 4/4 Vérification des sorties")
          render_result
        }
      ), error = function(e) list(error = conditionMessage(e)))
      result <- execution

      all_ok <- if (is.list(result) && !is.null(result$error)) FALSE
                else all(vapply(result, function(r) identical(r$status, "completed"), logical(1)))

      if (all_ok) {
        run_status("completed")
        app_state$prepared_run_ready <- FALSE
        app_state$last_prepare_status <- "completed"
        app_state$run_command <- NULL
        app_state$last_run_path <- meta$run_path
        output_file <- prepared$output_file %||% expected_output_file_for_region(meta$run_path, prepared$region, fs_run)
        if (!file.exists(output_file)) {
          output_file <- select_run_output_figure(meta$run_path, metadata = meta)
        }
        app_state$last_output_file <- output_file
        app_state$last_run_region <- prepared$region
        app_state$last_render <- list(
          region = prepared$region,
          tracks = prepared$tracks %||% list(),
          figure_settings = prepared$figure_settings %||% fs,
          config_template = prepared$config_file,
          output_dir = file.path(meta$run_path, "outputs", "multi_region"),
          run_path = meta$run_path,
          last_ini = prepared$config_file,
          last_image = output_file,
          render_index = 0L,
          signal_scaling_summary = NULL,
          command = app_state$run_command %||% ""
        )
        meta_done <- tryCatch(load_run_metadata(meta$run_path), error = function(e) meta)
        meta_done$last_run_region <- prepared$region
        meta_done$last_output_file <- output_file %||% ""
        meta_done$last_render_ini <- prepared$config_file
        meta_done$last_render_index <- 0L
        save_run_metadata(meta_done, meta$run_path)
        log_info(meta$run_path, sprintf("[RESULT] Displaying image: %s", output_file %||% "<none>"))
        if (!identical(prepared$region, app_state$last_run_region)) {
          log_error(meta$run_path, sprintf("[ERROR] Region mismatch: UI region = %s | Prepared region = %s | Run region = %s",
                                           ui_region %||% "?", prepared$region %||% "?", app_state$last_run_region %||% "?"))
          shiny::showNotification(
            "Erreur: la région utilisée pour le run ne correspond pas à la région sélectionnée.",
            type = "error", duration = 10
          )
        }
        shiny::showNotification(
          shiny::HTML("Run terminé avec succès ! <strong><a href='#'>Voir les résultats</a></strong>"),
          type = "message", duration = 7)
        app_state$nav_to <- "results"
      } else {
        run_status("failed")
        app_state$prepared_run_ready <- FALSE
        app_state$last_prepare_status <- "failed"
        err_msg <- if (is.list(result) && !is.null(result$error)) result$error else "Voir les logs."
        log_error(meta$run_path, sprintf("[RUN] Analysis failed: %s", err_msg))
        shiny::showNotification(
          shiny::HTML(sprintf("<strong>Run échoué</strong><br>%s", err_msg)),
          type = "error", duration = 10
        )
      }
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
