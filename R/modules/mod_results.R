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
        ),
        shiny::tags$div(
          class = "rt-card",
          shiny::tags$div(
            class = "rt-card-header",
            shiny::tags$span(class = "rt-card-icon", shiny::icon("sliders-h")),
            shiny::tags$h5("Réglages rapides de la figure")
          ),
          shiny::tags$p(
            class = "small text-muted",
            "Ces réglages modifient uniquement l'échelle d'affichage. Ils ne changent pas les données bigWig/bedGraph."
          ),
          shiny::fluidRow(
            shiny::column(6, shiny::numericInput(ns("results_width"), "Largeur finale (cm)", value = 12, min = 5, max = 50, step = 0.5)),
            shiny::column(6, shiny::numericInput(ns("results_dpi"), "Résolution (DPI)", value = 300, min = 72, max = 600, step = 25))
          ),
          shiny::selectInput(
            ns("signal_scale_mode_results"),
            "Mode d'échelle des signaux",
            choices = c(
              "Échelle automatique par track" = "auto_per_track",
              "Partagée par modalité (ATAC/WGBS)" = "shared_by_modality",
              "Maximum global partagé" = "shared_global_max",
              "Maximum global partagé avec marge" = "shared_global_max_padded",
              "Quantile global partagé" = "shared_global_quantile",
              "Manuel" = "manual"
            ),
            selected = "shared_by_modality"
          ),
          shiny::fluidRow(
            shiny::column(6, shiny::numericInput(ns("results_min_value"), "Valeur min", value = 0, step = 0.1)),
            shiny::column(6, shiny::numericInput(ns("results_manual_max_value"), "Valeur max manuelle", value = 100, step = 0.1))
          ),
          shiny::fluidRow(
            shiny::column(6, shiny::numericInput(ns("results_padding_factor"), "Marge au-dessus du max signal", value = 1.15, min = 1, max = 2, step = 0.05)),
            shiny::column(6, shiny::numericInput(ns("results_quantile"), "Quantile global", value = 0.99, min = 0.90, max = 1, step = 0.005))
          ),
          shiny::fluidRow(
            shiny::column(4, shiny::numericInput(ns("results_track_height"), "Hauteur signaux", value = 0.8, min = 0.25, max = 8, step = 0.05)),
            shiny::column(4, shiny::numericInput(ns("results_annotation_height"), "Hauteur BED", value = 0.15, min = 0.1, max = 8, step = 0.05)),
            shiny::column(4, shiny::numericInput(ns("results_gene_track_height"), "Hauteur gènes", value = 0.55, min = 0.25, max = 5, step = 0.05))
          ),
          shiny::fluidRow(
            shiny::column(6, shiny::numericInput(ns("results_spacer_height"), "Hauteur spacer avant gènes", value = 0.05, min = 0, max = 3, step = 0.05)),
            shiny::column(6, shiny::numericInput(ns("results_fontsize"), "Fontsize", value = 6, min = 4, max = 20, step = 1))
          ),
          shiny::numericInput(ns("results_gene_rows"), "Lignes de gènes (0 = automatique)", value = 0, min = 0, max = 20, step = 1),
          shiny::checkboxInput(ns("results_compact_genes"), "Gènes compacts (fusionner les isoformes)", value = TRUE),
          shiny::checkboxInput(ns("results_apply_to_all_signal_tracks"), "Appliquer aux tracks de signal", value = TRUE),
          shiny::checkboxInput(ns("results_lock_shared_y_axis"), "Verrouiller l'axe Y partagé", value = TRUE),
          shiny::tags$div(
            class = "d-flex flex-wrap gap-2 mb-2",
            shiny::actionButton(ns("preset_publication"), "Panel publication", class = "btn btn-primary btn-sm"),
            shiny::actionButton(ns("preset_multiomics"), "Multi-omique compacte", class = "btn btn-outline-primary btn-sm"),
            shiny::actionButton(ns("preset_compare"), "Comparer honnêtement", class = "btn btn-outline-primary btn-sm"),
            shiny::actionButton(ns("preset_small_peaks"), "Voir petits pics", class = "btn btn-outline-primary btn-sm"),
            shiny::actionButton(ns("preset_manual"), "Échelle manuelle", class = "btn btn-outline-secondary btn-sm"),
            shiny::actionButton(ns("preset_auto"), "Auto par track", class = "btn btn-outline-secondary btn-sm")
          ),
          shiny::actionButton(ns("btn_recalc_scale"), "Recalculer l'échelle depuis la région", class = "btn btn-secondary btn-sm w-100"),
          shiny::div(class = "mt-2"),
          shiny::actionButton(ns("btn_quick_preview"), "Aperçu rapide", class = "btn btn-outline-primary btn-sm w-100"),
          shiny::div(class = "mt-2"),
          shiny::actionButton(ns("btn_apply_rerender"), "Appliquer et régénérer", class = "btn btn-primary btn-sm w-100"),
          shiny::uiOutput(ns("render_warning_ui")),
          shiny::uiOutput(ns("render_summary_ui"))
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
      completed_path <- app_state$last_run_path %||% NULL
      if (!is.null(completed_path) && dir.exists(completed_path)) return(completed_path)

      # A prepared run is useful for diagnostics even before an image exists.
      current <- app_state$current_run %||% NULL
      current_path <- current$run_path %||% NULL
      if (!is.null(current_path) && dir.exists(current_path)) return(current_path)

      # Recover the latest run after a page/session refresh.
      project_path <- (app_state$project_config %||% list())$project_path %||% NULL
      runs_dir <- if (!is.null(project_path)) file.path(project_path, "runs") else NULL
      if (is.null(runs_dir) || !dir.exists(runs_dir)) return(NULL)
      candidates <- list.dirs(runs_dir, recursive = FALSE, full.names = TRUE)
      candidates <- candidates[file.exists(file.path(candidates, "run_metadata.json"))]
      if (length(candidates) == 0L) return(NULL)
      candidates[order(file.info(candidates)$mtime, decreasing = TRUE)][[1L]]
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

    ensure_last_render <- function() {
      path <- selected_run_path()
      if (is.null(path) || !dir.exists(path)) return(NULL)
      current <- app_state$last_render %||% NULL
      if (!is.null(current) && identical(current$run_path %||% "", path)) return(current)

      meta <- tryCatch(load_run_metadata(path), error = function(e) NULL)
      tracks_file <- file.path(path, "config", "tracks_config.json")
      run_cfg_file <- file.path(path, "config", "run_config.yaml")
      tracks <- if (file.exists(tracks_file)) tryCatch(read_json_safe(tracks_file), error = function(e) list()) else list()
      run_cfg <- if (file.exists(run_cfg_file)) tryCatch(read_yaml_safe(run_cfg_file), error = function(e) list()) else list()
      region <- meta$last_run_region %||% meta$region %||% NULL
      image <- select_run_output_figure(path, app_state = app_state, metadata = meta)
      if (is.null(region) || length(tracks) == 0L) return(NULL)

      app_state$last_render <- list(
        region = region,
        tracks = tracks,
        figure_settings = run_cfg$figure_settings %||% app_state$figure_settings %||% list(),
        config_template = file.path(path, "config", "tracks.ini"),
        output_dir = file.path(path, "outputs", "multi_region"),
        run_path = path,
        last_ini = meta$last_render_ini %||% file.path(path, "config", "tracks.ini"),
        last_image = image,
        render_index = as.integer(meta$last_render_index %||% 0L),
        signal_scaling_summary = NULL,
        command = ""
      )
      app_state$last_render
    }

    collect_results_render_settings <- function() {
      list(
        width = input$results_width %||% 12,
        dpi = input$results_dpi %||% 300,
        signal_scale_mode = input$signal_scale_mode_results %||% "shared_by_modality",
        min_value = input$results_min_value %||% 0,
        manual_max_value = input$results_manual_max_value %||% 100,
        padding_factor = input$results_padding_factor %||% 1.15,
        quantile = input$results_quantile %||% 0.99,
        track_height = input$results_track_height %||% 1.1,
        annotation_height = input$results_annotation_height %||% 0.25,
        annotation_labels = FALSE,
        gene_track_height = input$results_gene_track_height %||% 0.9,
        spacer_height = input$results_spacer_height %||% 0.05,
        fontsize = input$results_fontsize %||% 6,
        gene_rows = input$results_gene_rows %||% 0,
        compact_genes = isTRUE(input$results_compact_genes),
        apply_to_all_signal_tracks = isTRUE(input$results_apply_to_all_signal_tracks),
        lock_shared_y_axis = isTRUE(input$results_lock_shared_y_axis)
      )
    }

    shiny::observeEvent(selected_run_path(), {
      lr <- ensure_last_render()
      if (is.null(lr)) return()
      fs <- lr$figure_settings %||% list()
      tracks <- lr$tracks %||% list()
      signal_heights <- vapply(Filter(is_signal_track, tracks), function(t) {
        suppressWarnings(as.numeric((t$params %||% list())$height %||% NA_real_))
      }, numeric(1))
      signal_heights <- signal_heights[is.finite(signal_heights)]
      shiny::updateSelectInput(session, "signal_scale_mode_results",
                               selected = fs$signal_scale_mode %||% "shared_by_modality")
      shiny::updateNumericInput(session, "results_width", value = fs$width %||% 12)
      shiny::updateNumericInput(session, "results_dpi", value = fs$dpi %||% 300)
      shiny::updateNumericInput(session, "results_min_value", value = fs$shared_min_value %||% fs$manual_min_value %||% 0)
      shiny::updateNumericInput(session, "results_manual_max_value", value = fs$manual_max_value %||% 100)
      shiny::updateNumericInput(session, "results_padding_factor", value = fs$signal_max_padding_factor %||% 1.15)
      shiny::updateNumericInput(session, "results_quantile", value = fs$shared_quantile %||% 0.99)
      shiny::updateNumericInput(session, "results_track_height", value = fs$signal_track_height %||% if (length(signal_heights) > 0L) signal_heights[[1]] else 0.8)
      shiny::updateNumericInput(session, "results_annotation_height", value = fs$annotation_track_height %||% 0.25)
      shiny::updateNumericInput(session, "results_gene_track_height", value = fs$gene_track_height %||% 0.55)
      shiny::updateNumericInput(session, "results_spacer_height", value = fs$spacer_before_genes_height %||% 0.05)
      shiny::updateNumericInput(session, "results_fontsize", value = fs$gene_label_fontsize %||% 6)
      shiny::updateNumericInput(session, "results_gene_rows", value = fs$gene_rows %||% 0)
    }, ignoreNULL = FALSE)

    output$render_warning_ui <- shiny::renderUI({
      mode <- input$signal_scale_mode_results %||% "shared_by_modality"
      lr <- app_state$last_render %||% NULL
      signal_count <- if (is.null(lr)) 0L else length(Filter(is_signal_track, lr$tracks %||% list()))
      warnings <- character(0)
      if (signal_count >= 8L) {
        warnings <- c(warnings,
          sprintf("%d pistes de signal : utilisez le preset « Multi-omique compacte » et la visionneuse zoomable.", signal_count))
      }
      if (identical(mode, "auto_per_track") && signal_count > 1L) {
        warnings <- c(warnings, "Attention : une échelle automatique indépendante peut rendre des signaux faibles visuellement comparables à des signaux forts.")
      }
      if (identical(mode, "shared_global_quantile")) {
        warnings <- c(warnings, "Attention : les pics extrêmes au-dessus du quantile peuvent être tronqués.")
      }
      if (length(warnings) == 0L) return(NULL)
      shiny::tags$div(class = "alert alert-warning mt-2 small", paste(warnings, collapse = " "))
    })

    output$render_summary_ui <- shiny::renderUI({
      lr <- app_state$last_render %||% NULL
      if (is.null(lr)) return(NULL)
      s <- lr$signal_scaling_summary %||% list()
      settings <- collect_results_render_settings()
      shiny::tags$div(
        class = "mt-3 small",
        shiny::tags$div(style = "font-weight:700;margin-bottom:4px;", "Current render settings"),
        shiny::tags$div(
          style = "display:grid;grid-template-columns:auto 1fr;gap:3px 8px;",
          shiny::tags$span("Region"), shiny::tags$code(lr$region %||% "?"),
          shiny::tags$span("Scaling mode"), shiny::tags$code(settings$signal_scale_mode),
          shiny::tags$span("Raw global max"), shiny::tags$code(format_scale_value(s$raw_global_max %||% NA_real_)),
          shiny::tags$span("Final min_value"), shiny::tags$code(format_scale_value(s$shared_min_value %||% settings$min_value %||% NA_real_)),
          shiny::tags$span("Final max_value"), shiny::tags$code(format_scale_value(s$shared_max_value %||% settings$manual_max_value %||% NA_real_)),
          shiny::tags$span("Quantile"), shiny::tags$code(settings$quantile),
          shiny::tags$span("Padding factor"), shiny::tags$code(settings$padding_factor),
          shiny::tags$span("Signal height"), shiny::tags$code(settings$track_height),
          shiny::tags$span("Gene height"), shiny::tags$code(settings$gene_track_height),
          shiny::tags$span("Spacer height"), shiny::tags$code(settings$spacer_height),
          shiny::tags$span("Render file"), shiny::tags$code(basename(lr$last_image %||% ""))
        )
      )
    })

    shiny::observeEvent(input$preset_compare, {
      shiny::updateSelectInput(session, "signal_scale_mode_results", selected = "shared_by_modality")
      shiny::updateNumericInput(session, "results_min_value", value = 0)
      shiny::updateNumericInput(session, "results_padding_factor", value = 1.15)
    })

    shiny::observeEvent(input$preset_publication, {
      shiny::updateNumericInput(session, "results_width", value = 10)
      shiny::updateNumericInput(session, "results_dpi", value = 300)
      shiny::updateSelectInput(session, "signal_scale_mode_results", selected = "shared_by_modality")
      shiny::updateNumericInput(session, "results_min_value", value = 0)
      shiny::updateNumericInput(session, "results_padding_factor", value = 1.15)
      shiny::updateNumericInput(session, "results_track_height", value = 0.70)
      shiny::updateNumericInput(session, "results_annotation_height", value = 0.12)
      shiny::updateNumericInput(session, "results_gene_track_height", value = 0.50)
      shiny::updateNumericInput(session, "results_spacer_height", value = 0.05)
      shiny::updateNumericInput(session, "results_fontsize", value = 6)
      shiny::updateNumericInput(session, "results_gene_rows", value = 0)
      shiny::showNotification(
        "Proportions publication appliquées. Cliquez sur Appliquer et régénérer.",
        type = "message", duration = 6
      )
    })

    shiny::observeEvent(input$preset_multiomics, {
      shiny::updateNumericInput(session, "results_width", value = 10)
      shiny::updateNumericInput(session, "results_dpi", value = 300)
      shiny::updateSelectInput(session, "signal_scale_mode_results", selected = "shared_by_modality")
      shiny::updateNumericInput(session, "results_min_value", value = 0)
      shiny::updateNumericInput(session, "results_padding_factor", value = 1.10)
      shiny::updateNumericInput(session, "results_track_height", value = 0.32)
      shiny::updateNumericInput(session, "results_annotation_height", value = 0.10)
      shiny::updateNumericInput(session, "results_gene_track_height", value = 0.35)
      shiny::updateNumericInput(session, "results_spacer_height", value = 0)
      shiny::updateNumericInput(session, "results_fontsize", value = 4)
      shiny::updateNumericInput(session, "results_gene_rows", value = 0)
      shiny::updateCheckboxInput(session, "results_compact_genes", value = TRUE)
      shiny::showNotification(
        "Profil multi-omique compact appliqué. Cliquez sur Appliquer et régénérer.",
        type = "message", duration = 7
      )
    })

    shiny::observeEvent(input$preset_small_peaks, {
      shiny::updateSelectInput(session, "signal_scale_mode_results", selected = "shared_global_quantile")
      shiny::updateNumericInput(session, "results_quantile", value = 0.99)
      shiny::updateNumericInput(session, "results_padding_factor", value = 1.05)
    })

    shiny::observeEvent(input$preset_manual, {
      shiny::updateSelectInput(session, "signal_scale_mode_results", selected = "manual")
    })

    shiny::observeEvent(input$preset_auto, {
      shiny::updateSelectInput(session, "signal_scale_mode_results", selected = "auto_per_track")
      shiny::showNotification(
        "Attention : l'autoscaling indépendant peut rendre les comparaisons entre conditions trompeuses.",
        type = "warning", duration = 8
      )
    })

    shiny::observeEvent(input$btn_recalc_scale, {
      lr <- ensure_last_render()
      if (is.null(lr)) {
        shiny::showNotification("Aucune figure précédente à ajuster.", type = "warning")
        return()
      }
      settings <- collect_results_render_settings()
      fs <- merge_results_render_settings(lr$figure_settings %||% list(), settings)
      tracks <- apply_results_render_track_settings(lr$tracks %||% list(), settings)
      summary <- compute_render_signal_summary(tracks, lr$region, fs)
      lr$signal_scaling_summary <- summary
      app_state$last_render <- lr
      if (!is.null(summary$shared_max_value) && is.finite(summary$shared_max_value)) {
        shiny::updateNumericInput(session, "results_manual_max_value", value = summary$shared_max_value)
      }
      shiny::showNotification("Échelle recalculée depuis la région courante.", type = "message")
    })

    run_rerender <- function(preview = FALSE) {
      if (isTRUE(app_state$is_rendering)) {
        shiny::showNotification("Un rendu est déjà en cours.", type = "message")
        return(NULL)
      }
      lr <- ensure_last_render()
      if (is.null(lr)) {
        shiny::showNotification("Aucune figure précédente à ajuster.", type = "warning")
        return(NULL)
      }
      app_state$is_rendering <- TRUE
      shinyjs::disable("btn_quick_preview")
      shinyjs::disable("btn_apply_rerender")
      on.exit({
        app_state$is_rendering <- FALSE
        shinyjs::enable("btn_quick_preview")
        shinyjs::enable("btn_apply_rerender")
      }, add = TRUE)
      tryCatch({
        res <- shiny::withProgress(message = if (preview) "Aperçu rapide en cours..." else "Re-rendu en cours...", {
          rerender_last_figure(app_state, collect_results_render_settings(), schema = NULL, preview = preview)
        })
        shiny::showNotification(
          sprintf("Figure régénérée : %s", basename(res$image)),
          type = "message", duration = 6
        )
        res
      }, error = function(e) {
        shiny::showNotification(e$message, type = "error", duration = 10)
        NULL
      })
    }

    shiny::observeEvent(input$btn_quick_preview, {
      run_rerender(preview = TRUE)
    })

    shiny::observeEvent(input$btn_apply_rerender, {
      run_rerender(preview = FALSE)
    })

    output$run_info_ui <- shiny::renderUI({
      meta <- run_meta()
      if (is.null(meta)) {
        return(empty_state("Aucun run", "Lancez ou sélectionnez un run.", icon_name = "play-circle"))
      }
      st <- meta$status %||% "unknown"
      path <- selected_run_path()
      tracks_cfg <- tryCatch(read_json_safe(file.path(path, "config", "tracks_config.json")), error = function(e) NULL)
      scale_modes <- character(0)
      if (!is.null(tracks_cfg) && length(tracks_cfg) > 0L) {
        scale_modes <- unique(vapply(tracks_cfg, function(t) {
          params <- t$params %||% list()
          if ((t$track_type %||% "") %in% c("bigwig", "bedgraph")) {
            paste0(params$track_group %||% "Custom", "/", params$scale_mode %||% "independent")
          } else {
            NA_character_
          }
        }, character(1)))
        scale_modes <- scale_modes[!is.na(scale_modes)]
      }
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
          shiny::tags$span(style = "font-size:11px;font-weight:600;color:var(--rt-muted);text-transform:uppercase;", "Dernier run"),
          shiny::tags$span(style = "font-size:12px;font-family:var(--rt-font-mono);", meta$last_run_region %||% app_state$last_run_region %||% "?"),
          shiny::tags$span(style = "font-size:11px;font-weight:600;color:var(--rt-muted);text-transform:uppercase;", "Renderer"),
          status_badge("info", label = meta$renderer %||% "?", show_dot = FALSE),
          shiny::tags$span(style = "font-size:11px;font-weight:600;color:var(--rt-muted);text-transform:uppercase;", "Image"),
          shiny::tags$span(style = "font-size:12px;font-family:var(--rt-font-mono);word-break:break-all;",
                           basename(meta$last_output_file %||% app_state$last_output_file %||% "")),
          shiny::tags$span(style = "font-size:11px;font-weight:600;color:var(--rt-muted);text-transform:uppercase;", "Échelles"),
          shiny::tags$span(style = "font-size:12px;color:var(--rt-muted);",
                           if (length(scale_modes) > 0L) paste(scale_modes, collapse = ", ") else "indépendantes")
        )
      )
    })

    output$figure_ui <- shiny::renderUI({
      path <- selected_run_path()
      if (is.null(path)) {
        return(empty_state("Aucune figure", "Lancez un run pour générer une figure.",
                           icon_name = "image"))
      }
      meta <- run_meta()
      status <- meta$status %||% "unknown"
      if (status %in% c("created", "prepared")) {
        return(shiny::div(
          class = "alert alert-warning",
          shiny::icon("exclamation-triangle"), " ",
          shiny::tags$strong("Analyse non exécutée."),
          shiny::tags$br(),
          "La configuration est prête, mais aucune figure n’a encore été générée. ",
          "Retournez dans l’onglet Run et cliquez sur « Lancer l’analyse »."
        ))
      }
      if (identical(status, "failed")) {
        return(empty_state(
          "Le run a échoué",
          "Consultez l’onglet Logs pour connaître la cause.",
          icon_name = "times-circle"
        ))
      }
      out_dir <- file.path(path, "outputs", "multi_region")
      if (!dir.exists(out_dir)) {
        return(empty_state("Pas encore de sorties", "Le run n'a pas encore généré de fichiers.",
                           icon_name = "image"))
      }
      fig <- select_run_output_figure(path, app_state = app_state, metadata = meta)
      if (is.null(fig) || !file.exists(fig)) {
        return(empty_state("Aucune figure PNG", "Vérifiez les logs pour détecter l'erreur.",
                           icon_name = "image"))
      }
      resource_name <- paste0("run_output_", gsub("[^A-Za-z0-9_]", "_", basename(path)))
      shiny::addResourcePath(resource_name, out_dir)
      cache_buster <- as.integer(file.info(fig)$mtime)
      message("[RESULT] Displaying image: ", fig)
      session$onFlushed(function() {
        session$sendCustomMessage("rt_init_figure_viewers", list())
      }, once = TRUE)
      shiny::tags$div(
        class = "figure-card rt-figure-viewer",
        shiny::tags$div(
          class = "rt-figure-toolbar",
          shiny::tags$button(type = "button", class = "btn btn-sm btn-outline-secondary rt-zoom-out",
                             title = "Réduire", shiny::icon("search-minus")),
          shiny::tags$button(type = "button", class = "btn btn-sm btn-outline-secondary rt-zoom-reset",
                             "100 %"),
          shiny::tags$button(type = "button", class = "btn btn-sm btn-outline-secondary rt-zoom-in",
                             title = "Agrandir", shiny::icon("search-plus")),
          shiny::tags$button(type = "button", class = "btn btn-sm btn-outline-primary rt-zoom-fullscreen",
                             shiny::icon("expand"), " Plein écran"),
          shiny::tags$span(class = "rt-zoom-help", "Ctrl/Cmd + molette : zoom · glisser : déplacer")
        ),
        shiny::tags$div(
          class = "rt-figure-stage",
          shiny::tags$img(
            src = paste0(resource_name, "/", basename(fig), "?v=", cache_buster),
            class = "rt-zoomable-figure",
            alt = "Figure générée",
            draggable = "false"
          )
        ),
        shiny::tags$div(
          class = "figure-caption",
          basename(fig), " — ",
          sprintf("%.0f KB", file.info(fig)$size / 1024)
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
      fig <- select_run_output_figure(path, app_state = app_state, metadata = run_meta())
      if (!is.null(fig) && file.exists(fig)) file.copy(fig, file)
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
