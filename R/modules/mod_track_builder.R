# =============================================================================
# mod_track_builder.R — Track configuration module
# =============================================================================

batch_track_color_input_id <- function(track_id) {
  paste0("batch_color_", digest::digest(as.character(track_id), algo = "xxhash32"))
}

#' Track Builder UI
#'
#' @param id module namespace ID
#' @export
mod_track_builder_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(

    omics_banner(
      "Track Builder",
      "Configurez les tracks génomiques à visualiser.",
      small = TRUE
    ),

    shiny::uiOutput(ns("project_check")),

    shiny::fluidRow(
      shiny::column(4,
        shiny::tags$div(
          class = "rt-card",
          shiny::tags$div(
            class = "rt-card-header",
            shiny::tags$span(class = "rt-card-icon", shiny::icon("plus-circle")),
            shiny::tags$h5("Ajouter une track")
          ),
          shiny::selectInput(ns("new_track_type"), "Type de track",
                             choices = c("—" = "")),
          shiny::uiOutput(ns("track_type_help")),
          shiny::textInput(ns("new_track_name"), "Nom de la track",
                           placeholder = "ex: H3K27ac signal"),
          shiny::uiOutput(ns("file_selector_ui")),
          shiny::uiOutput(ns("file_compat_warning")),
          shiny::div(class = "d-flex gap-2 mb-2",
            shiny::actionButton(ns("btn_add_track"),
              shiny::tagList(shiny::icon("plus"), " Ajouter"),
              class = "btn btn-primary flex-fill"),
            shiny::actionButton(ns("btn_add_xaxis"),
              "+ x-axis", class = "btn btn-secondary btn-sm"),
            shiny::actionButton(ns("btn_add_spacer"),
              "+ espace", class = "btn btn-secondary btn-sm")
          ),
          shiny::tags$hr(class = "divider"),
          shiny::tags$h6(class = "text-muted mb-2", "Templates"),
          shiny::tags$small(class = "text-muted", "Template de track"),
          shiny::selectInput(ns("template_select"), NULL,
                             choices = c("—" = "")),
          shiny::div(class = "d-flex gap-2",
            shiny::actionButton(ns("btn_apply_template"),
              shiny::tagList(shiny::icon("layer-group"), " Appliquer"),
              class = "btn btn-secondary btn-sm flex-fill"),
            shiny::actionButton(ns("btn_tmpl_replace"),
              shiny::tagList(shiny::icon("sync"), " Remplacer"),
              class = "btn btn-secondary btn-sm"),
            shiny::actionButton(ns("btn_tmpl_append"),
              shiny::tagList(shiny::icon("plus"), " Ajouter"),
              class = "btn btn-secondary btn-sm")
          ),
          shiny::tags$hr(class = "divider"),
          shiny::tags$small(class = "text-muted", "Ensemble de tracks"),
          shiny::selectInput(ns("track_set_select"), NULL,
                             choices = c("—" = "")),
          shiny::div(class = "d-flex gap-2",
            shiny::actionButton(ns("btn_trackset_append"),
              shiny::tagList(shiny::icon("plus"), " Ajouter ensemble"),
              class = "btn btn-secondary btn-sm flex-fill"),
            shiny::actionButton(ns("btn_trackset_replace"),
              shiny::tagList(shiny::icon("sync"), " Remplacer"),
              class = "btn btn-secondary btn-sm"),
            shiny::actionButton(ns("btn_trackset_delete"),
              shiny::tagList(shiny::icon("trash")),
              class = "btn btn-danger btn-sm")
          )
        )
      ),
      shiny::column(8,
        shiny::tags$div(
          class = "rt-card",
          shiny::tags$div(
            class = "rt-card-header flex-between",
            shiny::tags$div(
              class = "flex-row gap-8",
              shiny::tags$span(class = "rt-card-icon", shiny::icon("list")),
              shiny::tags$h5("Tracks configurées")
            ),
            shiny::tags$div(
              class = "d-flex gap-2",
              shiny::actionButton(
                ns("btn_open_track_params"),
                shiny::tagList(shiny::icon("palette"), " Couleur / paramètres"),
                class = "btn btn-outline-primary btn-sm"
              ),
              shiny::actionButton(
                ns("btn_auto_palette"),
                shiny::tagList(shiny::icon("magic"), " Palette auto"),
                class = "btn btn-outline-primary btn-sm",
                title = "Couleurs distinctes par modalité et dégradé selon le temps"
              ),
              shiny::actionButton(ns("btn_toggle"),
                shiny::tagList(shiny::icon("eye"), " On/Off"),
                class = "btn btn-secondary btn-sm"),
              shiny::actionButton(ns("btn_dup"),
                shiny::tagList(shiny::icon("copy"), " Dup"),
                class = "btn btn-secondary btn-sm"),
              shiny::actionButton(ns("btn_delete"),
                shiny::tagList(shiny::icon("trash")),
                class = "btn btn-danger btn-sm"),
              shiny::actionButton(ns("btn_up"),
                shiny::icon("arrow-up"),
                class = "btn btn-secondary btn-sm"),
              shiny::actionButton(ns("btn_down"),
                shiny::icon("arrow-down"),
                class = "btn btn-secondary btn-sm")
            )
          ),
          shiny::uiOutput(ns("display_filter_ui")),
          shiny::tags$div(
            class = "rt-track-order-hint",
            shiny::icon("grip-lines"),
            " Glissez-déposez les cartes pour construire l’ordre de la figure. Cmd/Ctrl-clic : sélection multiple."
          ),
          shiny::tags$div(
            class = "rt-track-card-list",
            `data-order-input` = ns("tracks_order_dragged"),
            `data-selection-input` = ns("track_cards_selected"),
            shiny::uiOutput(ns("track_cards_ui"))
          ),
          shiny::tags$details(
            class = "rt-track-table-details",
            shiny::tags$summary(shiny::icon("table"), " Vue tableau détaillée"),
            shiny::tags$div(
              class = "rt-track-order-table",
              DT::DTOutput(ns("tracks_table"))
            )
          )
        ),
        shiny::tags$div(
          id = ns("track_params_card"),
          class = "rt-card mt-3",
          shiny::tags$div(
            class = "rt-card-header",
            shiny::tags$span(class = "rt-card-icon", shiny::icon("sliders-h")),
            shiny::tags$h5("Paramètres de la track sélectionnée")
          ),
          shiny::uiOutput(ns("edit_params_ui")),
          shiny::tags$div(
            class = "d-flex gap-2 mt-2",
            shiny::actionButton(ns("btn_save_params"),
              shiny::tagList(shiny::icon("save"), " Enregistrer"),
              class = "btn btn-primary"),
            shiny::actionButton(ns("btn_save_as_template"),
              shiny::tagList(shiny::icon("bookmark"), " Sauvegarder comme template"),
              class = "btn btn-secondary")
          )
        )
      )
    )
  )
}

#' Track Builder server
#'
#' @param id module namespace ID
#' @param app_state reactive values
#' @param schema schema list
#' @export
mod_track_builder_server <- function(id, app_state, schema) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    specs <- load_input_specs()

    save_tracks_and_invalidate <- function(message_text = "[TRACK] Project tracks saved") {
      proj <- app_state$project_config
      if (!is.null(proj)) {
        path <- save_project_tracks(proj, app_state$tracks %||% list())
        message(sprintf("[TRACK] Project tracks saved to %s", path))
      }
      app_state$prepared_config <- NULL
      app_state$run_command <- NULL
      app_state$prepared_run_ready <- FALSE
      app_state$last_prepare_status <- "invalidated"
      app_state$tracks_dirty <- TRUE
      app_state$config_dirty <- TRUE
      message("[STATE] Track configuration changed. Prepared run invalidated.")
      invisible(TRUE)
    }

    selected_track_ids <- function() {
      card_ids <- as.character(unlist(input$track_cards_selected %||% character(0)))
      valid_ids <- vapply(app_state$tracks %||% list(), function(track) track$track_id %||% "", character(1))
      card_ids <- card_ids[nzchar(card_ids) & card_ids %in% valid_ids]
      if (!is.null(input$track_cards_selected)) return(card_ids)
      get_selected_track_ids(input, app_state$tracks %||% list())
    }

    selected_track_id <- function() {
      ids <- selected_track_ids()
      if (has_one_selection(ids)) return(ids[[1]])
      app_state$selected_track_id %||% NULL
    }

    selected_track <- function() {
      id <- selected_track_id()
      if (is.null(id) || length(id) != 1L || is.na(id) || !nzchar(id)) return(NULL)
      get_track_by_id(app_state$tracks %||% list(), id)
    }

    template_refresh <- shiny::reactiveVal(0L)
    track_set_refresh <- shiny::reactiveVal(0L)

    # Track type choices
    shiny::observe({
      choices <- c("—" = "", get_track_type_choices(schema))
      shiny::updateSelectInput(session, "new_track_type", choices = choices)
    })

    # Template choices
    shiny::observe({
      template_refresh()
      tmpls <- tryCatch(load_track_templates(), error = function(e) list())
      choices <- c("—" = "")
      if (length(tmpls) > 0) {
        ids <- vapply(tmpls, function(t) t$template_id %||% "", character(1))
        labels <- vapply(tmpls, function(t) t$template_name %||% t$template_id %||% "Template", character(1))
        choices <- c("—" = "", setNames(ids, labels))
      }
      shiny::updateSelectInput(session, "template_select", choices = choices)
    })

    shiny::observe({
      track_set_refresh()
      sets <- tryCatch(load_track_set_templates(), error = function(e) list())
      choices <- c("—" = "")
      if (length(sets) > 0) {
        ids <- vapply(sets, function(s) s$track_set_id %||% "", character(1))
        labels <- vapply(sets, function(s) sprintf("%s (%d)", s$name %||% s$track_set_id %||% "Track set", as.integer(s$n_tracks %||% length(s$tracks %||% list()))), character(1))
        choices <- c("—" = "", setNames(ids, labels))
      }
      shiny::updateSelectInput(session, "track_set_select", choices = choices)
    })

    output$project_check <- shiny::renderUI({
      if (is.null(app_state$project_config))
        shiny::div(class = "alert alert-warning",
          shiny::icon("exclamation-triangle"), " Aucun projet actif.")
    })

    # Aide contextuelle par type de track
    output$track_type_help <- shiny::renderUI({
      ttype <- input$new_track_type
      if (is.null(ttype) || ttype == "") return(NULL)
      fmts <- get_formats_for_track_type(ttype, specs)
      exts <- get_extensions_for_track_type(ttype, specs)
      if (length(fmts) == 0) return(NULL)
      shiny::div(class = "alert alert-info p-2 mb-2",
        shiny::icon("info-circle"),
        sprintf(" Formats acceptés : %s (%s)",
                paste(toupper(fmts), collapse = ", "),
                paste(exts, collapse = ", "))
      )
    })

    # Show file selector, with filtering by track type compatibility
    output$file_selector_ui <- shiny::renderUI({
      ttype <- input$new_track_type
      if (is.null(ttype) || nchar(ttype) == 0) return(NULL)
      requires <- tryCatch(track_requires_file(schema, ttype), error = function(e) FALSE)
      if (!requires) return(NULL)
      reg <- app_state$registry
      if (is.null(reg) || nrow(reg) == 0) {
        return(shiny::div(class = "text-warning small",
          shiny::icon("exclamation-triangle"),
          " Aucun fichier dans le registre. Allez dans 'Inputs'."))
      }
      # Filtrer par compatibilité d'extension
      exts <- get_extensions_for_track_type(ttype, specs)
      if (length(exts) > 0 && "stored_path" %in% names(reg)) {
        file_exts <- paste0(".", tools::file_ext(tolower(reg$stored_path)))
        compat_mask <- file_exts %in% tolower(exts) |
                       (reg$track_type_selected == ttype)
        reg_filtered <- reg[compat_mask, , drop = FALSE]
      } else {
        reg_filtered <- reg
      }
      if (nrow(reg_filtered) == 0) {
        return(shiny::div(class = "text-warning small",
          shiny::icon("exclamation-triangle"),
          sprintf(" Aucun fichier compatible avec le type '%s'.", ttype)
        ))
      }
      choices <- setNames(
        reg_filtered$file_id,
        paste0(reg_filtered$original_name,
               ifelse(reg_filtered$track_type_selected != "", paste0(" (", reg_filtered$track_type_selected, ")"), ""))
      )
      shiny::selectInput(ns("file_select"), "Fichier source", choices = c("—" = "", choices))
    })

    # Avertissement si aucun fichier compatible
    output$file_compat_warning <- shiny::renderUI({
      ttype <- input$new_track_type
      if (is.null(ttype) || ttype == "") return(NULL)
      requires <- tryCatch(track_requires_file(schema, ttype), error = function(e) FALSE)
      if (!requires) return(NULL)
      reg  <- app_state$registry
      exts <- get_extensions_for_track_type(ttype, specs)
      if (!is.null(reg) && nrow(reg) > 0 && length(exts) > 0 && "stored_path" %in% names(reg)) {
        file_exts <- paste0(".", tools::file_ext(tolower(reg$stored_path)))
        ok <- any(file_exts %in% tolower(exts) | reg$track_type_selected == ttype)
        if (!ok) {
          return(shiny::div(class = "alert alert-warning p-2 mt-1",
            shiny::icon("exclamation-triangle"),
            sprintf(" Aucun fichier compatible avec '%s' dans le registre.", ttype)
          ))
        }
      }
      NULL
    })

    # Render tracks table
    output$tracks_table <- DT::renderDT({
      tracks <- app_state$tracks
      if (length(tracks) == 0)
        return(data.frame(Message = "Aucune track. Ajoutez-en une."))
      tracks_to_df(tracks)
    }, selection = list(mode = "multiple", target = "row"),
       rownames = FALSE,
       options = list(pageLength = 50, paging = FALSE, dom = "ti",
                      ordering = FALSE,
                      scrollY = "360px", scrollCollapse = TRUE,
                      columnDefs = list(list(visible = FALSE, targets = 0)),
                      language = rt_dt_language()))

    output$track_cards_ui <- shiny::renderUI({
      tracks <- app_state$tracks %||% list()
      if (length(tracks) == 0L) {
        return(shiny::div(class = "rt-track-card-empty", "Aucune track configurée."))
      }
      selected <- as.character(unlist(input$track_cards_selected %||% character(0)))
      shiny::tagList(lapply(seq_along(tracks), function(i) {
        track <- tracks[[i]]
        id <- track$track_id %||% paste0("track_", i)
        colour <- as.character((track$params %||% list())$color %||% "#64748b")
        family <- track_colour_family(track)
        time <- track_timepoint(track)
        time_label <- if (is.finite(time)) paste0("D", format(time, trim = TRUE)) else NULL
        shiny::tags$div(
          class = paste("rt-track-sort-card", if (id %in% selected) "is-selected" else "",
                        if (!isTRUE(track$enabled)) "is-disabled" else ""),
          draggable = "true",
          `data-track-id` = id,
          shiny::tags$div(class = "rt-track-drag-handle", shiny::icon("grip-vertical")),
          shiny::tags$div(class = "rt-track-order-number", i),
          shiny::tags$span(class = "rt-track-color-swatch", style = paste0("background:", colour)),
          shiny::tags$div(
            class = "rt-track-card-main",
            shiny::tags$strong(track$track_name %||% "Track"),
            shiny::tags$small(basename(track$file_path %||% "") %||% "")
          ),
          shiny::tags$div(
            class = "rt-track-card-badges",
            shiny::tags$span(class = "rt-track-chip", family),
            if (!is.null(time_label)) shiny::tags$span(class = "rt-track-chip rt-track-chip-time", time_label),
            shiny::tags$span(class = "rt-track-chip", track$track_type %||% "")
          ),
          shiny::tags$div(
            class = "rt-track-card-state",
            shiny::icon(if (isTRUE(track$enabled)) "eye" else "eye-slash"),
            if (isTRUE(track$enabled)) " Affichée" else " Masquée"
          )
        )
      }))
    })

    output$display_filter_ui <- shiny::renderUI({
      tracks <- app_state$tracks %||% list()
      if (length(tracks) == 0L) return(NULL)
      families <- unique(vapply(tracks, track_colour_family, character(1)))
      families <- families[!families %in% c("GENES", "BED")]
      times <- vapply(tracks, track_timepoint, numeric(1))
      time_keys <- unique(ifelse(is.finite(times), paste0("D", format(times, trim = TRUE)), "Sans temps"))
      enabled <- vapply(tracks, function(track) isTRUE(track$enabled), logical(1))
      enabled_families <- unique(vapply(tracks[enabled], track_colour_family, character(1)))
      enabled_families <- intersect(families, enabled_families)
      enabled_times <- times[enabled]
      enabled_time_keys <- unique(ifelse(is.finite(enabled_times), paste0("D", format(enabled_times, trim = TRUE)), "Sans temps"))
      context_types <- c("gtf", "genes", "bed", "narrowpeak", "broadpeak", "x_axis", "x-axis", "spacer", "scalebar")
      context_tracks <- tracks[vapply(tracks, function(track) (track$track_type %||% "") %in% context_types, logical(1))]
      keep_context <- length(context_tracks) == 0L || all(vapply(context_tracks, function(track) isTRUE(track$enabled), logical(1)))
      shiny::tags$div(
        class = "rt-track-display-filter",
        shiny::tags$div(
          class = "rt-track-display-filter-title",
          shiny::icon("filter"),
          shiny::tags$strong(" Choisir les tracks affichées"),
          shiny::tags$small("Les tracks sont activées/masquées, jamais supprimées.")
        ),
        shiny::fluidRow(
          shiny::column(5, shiny::selectizeInput(
            ns("display_modalities"), "Modalités",
            choices = families, selected = enabled_families, multiple = TRUE,
            options = list(plugins = list("remove_button"), placeholder = "Choisir les modalités")
          )),
          shiny::column(4, shiny::selectizeInput(
            ns("display_times"), "Temps",
            choices = time_keys, selected = intersect(time_keys, enabled_time_keys), multiple = TRUE,
            options = list(plugins = list("remove_button"), placeholder = "Choisir les temps")
          )),
          shiny::column(3,
            shiny::checkboxInput(ns("display_keep_context"), "Garder annotations et gènes", keep_context)
          )
        ),
        shiny::div(
          class = "d-flex gap-2",
          shiny::actionButton(ns("btn_apply_display_filter"),
            shiny::tagList(shiny::icon("eye"), " Afficher ce choix"),
            class = "btn btn-primary btn-sm flex-fill"),
          shiny::actionButton(ns("btn_show_all_tracks"),
            shiny::tagList(shiny::icon("list"), " Tout afficher"),
            class = "btn btn-outline-primary btn-sm")
        )
      )
    })

    shiny::observeEvent(input$tracks_order_dragged, {
      requested_ids <- as.character(unlist(input$tracks_order_dragged %||% character(0)))
      tracks <- app_state$tracks %||% list()
      current_ids <- vapply(tracks, function(track) track$track_id %||% "", character(1))
      requested_ids <- requested_ids[nzchar(requested_ids) & requested_ids %in% current_ids]
      if (length(requested_ids) != length(current_ids) || anyDuplicated(requested_ids)) {
        shiny::showNotification("Le nouvel ordre reçu est incomplet; aucun changement appliqué.", type = "warning")
        return()
      }
      reordered <- lapply(requested_ids, function(id) get_track_by_id(tracks, id))
      for (i in seq_along(reordered)) {
        reordered[[i]]$order <- i
        reordered[[i]]$updated_at <- track_now()
      }
      app_state$tracks <- standardize_tracks(reordered, app_state$project_config)
      save_tracks_and_invalidate()
      shiny::showNotification("Ordre des tracks enregistré.", type = "message", duration = 3)
      message("[TRACK_ORDER] Drag-and-drop order saved: ", paste(requested_ids, collapse = ", "))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$track_cards_selected, {
      ids <- selected_track_ids()
      app_state$selected_track_id <- if (has_one_selection(ids)) ids[[1]] else NULL
      has_any <- !is_empty(ids)
      has_one <- has_one_selection(ids)
      shinyjs::toggleState("btn_toggle", condition = has_any)
      shinyjs::toggleState("btn_dup", condition = has_any)
      shinyjs::toggleState("btn_delete", condition = has_any)
      shinyjs::toggleState("btn_up", condition = has_one)
      shinyjs::toggleState("btn_down", condition = has_one)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$btn_open_track_params, {
      ids <- selected_track_ids()
      tracks <- app_state$tracks %||% list()
      if (is_empty(ids)) {
        if (length(tracks) == 0L) {
          shiny::showNotification("Ajoutez d'abord une track.", type = "warning")
          return()
        }
        app_state$selected_track_id <- tracks[[1]]$track_id %||% NULL
        session$sendCustomMessage("rt_select_track_card", list(
          container = ns("track_cards_ui"), id = tracks[[1]]$track_id
        ))
      }
      session$sendCustomMessage("rt_scroll_to", list(id = ns("track_params_card")))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$btn_auto_palette, {
      tracks <- app_state$tracks %||% list()
      if (length(tracks) == 0L) {
        shiny::showNotification("Aucune track à colorer.", type = "warning")
        return()
      }
      app_state$tracks <- standardize_tracks(
        assign_automatic_track_colours(tracks),
        app_state$project_config
      )
      save_tracks_and_invalidate()
      shiny::showNotification(
        "Palette appliquée : teinte par modalité, dégradé clair à foncé selon le temps.",
        type = "message", duration = 7
      )
      message("[TRACK_COLOR] Automatic modality/time palette applied to ", length(tracks), " tracks")
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$btn_apply_display_filter, {
      tracks <- app_state$tracks %||% list()
      if (length(tracks) == 0L) return()
      modalities <- input$display_modalities %||% character(0)
      selected_times <- input$display_times %||% character(0)
      keep_context <- isTRUE(input$display_keep_context)
      tracks <- filter_tracks_for_display(tracks, modalities, selected_times, keep_context)
      enabled_count <- sum(vapply(tracks, function(track) isTRUE(track$enabled), logical(1)))
      if (enabled_count == 0L) {
        shiny::showNotification("Cette combinaison ne sélectionne aucune track.", type = "warning")
        return()
      }
      app_state$tracks <- standardize_tracks(tracks, app_state$project_config)
      save_tracks_and_invalidate()
      shiny::showNotification(
        sprintf("Affichage mis à jour : %d track(s) active(s) sur %d.", enabled_count, length(tracks)),
        type = "message", duration = 6
      )
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$btn_show_all_tracks, {
      tracks <- app_state$tracks %||% list()
      if (length(tracks) == 0L) return()
      tracks <- lapply(tracks, function(track) {
        track$enabled <- TRUE
        track$updated_at <- track_now()
        track
      })
      app_state$tracks <- standardize_tracks(tracks, app_state$project_config)
      save_tracks_and_invalidate()
      shiny::showNotification(sprintf("Les %d tracks sont affichées.", length(tracks)), type = "message")
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$tracks_table_rows_selected, {
      ids <- selected_track_ids()
      app_state$selected_track_id <- if (has_one_selection(ids)) ids[[1]] else NULL
      message("[SELECTION] Selected track ids: ", paste(ids, collapse = ", "))
      has_any <- !is_empty(ids)
      has_one <- has_one_selection(ids)
      shinyjs::toggleState("btn_toggle", condition = has_any)
      shinyjs::toggleState("btn_dup", condition = has_any)
      shinyjs::toggleState("btn_delete", condition = has_any)
      shinyjs::toggleState("btn_up", condition = has_one)
      shinyjs::toggleState("btn_down", condition = has_one)
    })

    # Ajouter un track x-axis
    shiny::observeEvent(input$btn_add_xaxis, {
      ttype <- "x_axis"
      tname <- "Axe X"
      current <- app_state$tracks
      new_t <- list(
        track_id   = new_track_id(),
        track_name = tname, track_type = ttype,
        file_id = NULL, file_path = "",
        enabled = TRUE, order = length(current) + 1,
        params = get_track_default_params(schema, ttype),
        created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
        updated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
      )
      app_state$tracks <- standardize_tracks(c(current, list(new_t)), app_state$project_config)
      save_tracks_and_invalidate()
      message("[TRACK] Added track: ", new_t$track_id)
      shiny::showNotification("Axe de coordonnées ajouté.", type = "message")
    })

    # Ajouter un spacer
    shiny::observeEvent(input$btn_add_spacer, {
      ttype <- "spacer"
      tname <- "Espace"
      current <- app_state$tracks
      new_t <- list(
        track_id   = new_track_id(),
        track_name = tname, track_type = ttype,
        file_id = NULL, file_path = "",
        enabled = TRUE, order = length(current) + 1,
        params = get_track_default_params(schema, ttype),
        created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
        updated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
      )
      app_state$tracks <- standardize_tracks(c(current, list(new_t)), app_state$project_config)
      save_tracks_and_invalidate()
      message("[TRACK] Added track: ", new_t$track_id)
      shiny::showNotification("Track 'spacer' ajoutée.", type = "message")
    })

    # Add a track
    shiny::observeEvent(input$btn_add_track, {
      ttype <- input$new_track_type
      if (is.null(ttype) || nchar(ttype) == 0) {
        shiny::showNotification("Choisissez un type de track.", type = "warning")
        return()
      }
      tname <- trimws(input$new_track_name %||% "")
      if (nchar(tname) == 0) {
        entry <- schema$tracks[[ttype]]
        tname <- entry$label %||% ttype
      }
      file_id   <- input$file_select %||% ""
      file_path <- ""
      reg <- app_state$registry
      if (nchar(file_id) > 0 && !is.null(reg) && nrow(reg) > 0) {
        entry <- get_file_by_id(reg, file_id)
        if (!is.null(entry)) file_path <- entry$stored_path
      }
      current_tracks <- app_state$tracks
      new_order <- length(current_tracks) + 1
      new_track <- list(
        track_id   = new_track_id(),
        track_name = tname,
        track_type = ttype,
        file_id    = if (nchar(file_id) > 0) file_id else NULL,
        file_path  = file_path,
        enabled    = TRUE,
        order      = new_order,
        params     = get_track_default_params(schema, ttype),
        created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
        updated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
      )
      app_state$tracks <- standardize_tracks(c(current_tracks, list(new_track)), app_state$project_config)
      save_tracks_and_invalidate()
      message("[TRACK] Added track: ", new_track$track_id)
      shiny::showNotification(sprintf("Track '%s' ajoutée.", tname), type = "message")
    })

    # Toggle enable/disable
    shiny::observeEvent(input$btn_toggle, {
      ids <- selected_track_ids()
      if (is_empty(ids)) {
        shiny::showNotification("Aucune track sélectionnée.", type = "warning")
        return()
      }
      tracks <- app_state$tracks
      for (i in seq_along(tracks)) {
        if (tracks[[i]]$track_id %in% ids) {
          tracks[[i]]$enabled <- !isTRUE(tracks[[i]]$enabled)
          tracks[[i]]$updated_at <- track_now()
        }
      }
      app_state$tracks <- standardize_tracks(tracks, app_state$project_config)
      save_tracks_and_invalidate()
      message("[TRACK] Toggled selected tracks: ", paste(ids, collapse = ", "))
    })

    # Duplicate track
    shiny::observeEvent(input$btn_dup, {
      ids <- selected_track_ids()
      if (is_empty(ids)) {
        shiny::showNotification("Aucune track sélectionnée.", type = "warning")
        return()
      }
      tracks <- app_state$tracks
      dups <- lapply(ids, function(id) {
        dup <- get_track_by_id(tracks, id)
        if (is.null(dup)) return(NULL)
        dup$track_id <- new_track_id()
        dup$track_name <- paste0(dup$track_name, " (copie)")
        dup$order <- length(tracks) + which(ids == id)
        dup$created_at <- track_now()
        dup$updated_at <- track_now()
        dup
      })
      dups <- Filter(Negate(is.null), dups)
      app_state$tracks <- standardize_tracks(c(tracks, dups), app_state$project_config)
      app_state$selected_track_id <- if (length(dups) == 1L) dups[[1]]$track_id else NULL
      save_tracks_and_invalidate()
      message("[TRACK] Duplicated selected tracks: ", paste(ids, collapse = ", "))
    })

    # Delete track
    shiny::observeEvent(input$btn_delete, {
      ids <- selected_track_ids()
      if (is_empty(ids)) {
        shiny::showNotification("Aucune track sélectionnée.", type = "warning")
        return()
      }
      tracks <- app_state$tracks
      tracks <- Filter(function(t) !t$track_id %in% ids, tracks)
      app_state$tracks <- standardize_tracks(tracks, app_state$project_config)
      app_state$selected_track_id <- NULL
      save_tracks_and_invalidate()
      message("[TRACK] Deleted selected tracks: ", paste(ids, collapse = ", "))
    })

    # Move up
    shiny::observeEvent(input$btn_up, {
      ids <- selected_track_ids()
      if (!has_one_selection(ids)) {
        shiny::showNotification("Sélectionnez une seule track pour la déplacer.", type = "warning")
        return()
      }
      id <- ids[[1]]
      tracks <- app_state$tracks
      sel <- which(vapply(tracks, function(t) identical(t$track_id, id), logical(1)))
      if (length(sel) != 1L || sel <= 1) return()
      tmp    <- tracks[[sel - 1]]; tracks[[sel - 1]] <- tracks[[sel]]; tracks[[sel]] <- tmp
      for (i in seq_along(tracks)) tracks[[i]]$order <- i
      app_state$tracks <- standardize_tracks(tracks, app_state$project_config)
      save_tracks_and_invalidate()
      message("[TRACK] Reordered tracks.")
    })

    # Move down
    shiny::observeEvent(input$btn_down, {
      tracks <- app_state$tracks
      ids <- selected_track_ids()
      if (!has_one_selection(ids)) {
        shiny::showNotification("Sélectionnez une seule track pour la déplacer.", type = "warning")
        return()
      }
      id <- ids[[1]]
      sel <- which(vapply(tracks, function(t) identical(t$track_id, id), logical(1)))
      if (length(sel) != 1L || sel >= length(tracks)) return()
      tmp    <- tracks[[sel + 1]]; tracks[[sel + 1]] <- tracks[[sel]]; tracks[[sel]] <- tmp
      for (i in seq_along(tracks)) tracks[[i]]$order <- i
      app_state$tracks <- standardize_tracks(tracks, app_state$project_config)
      save_tracks_and_invalidate()
      message("[TRACK] Reordered tracks.")
    })

    # Edit params UI
    output$edit_params_ui <- shiny::renderUI({
      ids <- selected_track_ids()
      if (is_empty(ids)) {
        shinyjs::disable("btn_save_params")
        shinyjs::disable("btn_save_as_template")
        return(shiny::p(shiny::em("Aucune track sélectionnée.")))
      }
      if (has_multi_selection(ids)) {
        shinyjs::disable("btn_save_params")
        shinyjs::disable("btn_save_as_template")
        selected_tracks <- Filter(function(track) track$track_id %in% ids,
                                  app_state$tracks %||% list())
        color_inputs <- lapply(selected_tracks, function(track) {
          colourpicker::colourInput(
            ns(batch_track_color_input_id(track$track_id)),
            track$track_name %||% track$track_id,
            value = as.character((track$params %||% list())$color %||% "#333333"),
            showColour = "background"
          )
        })
        return(shiny::tagList(
          shiny::div(class = "alert alert-info p-2",
            sprintf("%d tracks sélectionnées. Les couleurs peuvent être modifiées ensemble ci-dessous.", length(ids))
          ),
          shiny::tags$div(class = "d-flex flex-wrap gap-2",
            shiny::actionButton(ns("btn_enable_selected"), "Activer les tracks sélectionnées", class = "btn btn-secondary btn-sm"),
            shiny::actionButton(ns("btn_disable_selected"), "Désactiver les tracks sélectionnées", class = "btn btn-secondary btn-sm"),
            shiny::actionButton(ns("btn_duplicate_selected"), "Dupliquer la sélection", class = "btn btn-secondary btn-sm"),
            shiny::actionButton(ns("btn_delete_selected"), "Supprimer la sélection", class = "btn btn-danger btn-sm"),
            shiny::actionButton(ns("btn_save_track_set"), "Sauvegarder sélection comme ensemble", class = "btn btn-primary btn-sm")
          ),
          shiny::tags$hr(class = "divider"),
          shiny::tags$h6("Couleurs de la sélection"),
          shiny::div(class = "rt-batch-colors", color_inputs),
          shiny::actionButton(ns("btn_apply_selected_colors"),
            shiny::tagList(shiny::icon("palette"), " Appliquer les couleurs"),
            class = "btn btn-primary btn-sm w-100 mt-2")
        ))
      }
      shinyjs::enable("btn_save_params")
      shinyjs::enable("btn_save_as_template")
      # Resolve directly from the current DataTable selection. This avoids a
      # reactive race with app_state$selected_track_id when changing rows.
      id <- ids[[1]]
      t <- get_track_by_id(app_state$tracks %||% list(), id)
      if (is.null(t)) return(shiny::p(shiny::em("Track sélectionnée introuvable.")))
      track_type <- as.character(t$track_type %||% "")
      if (length(track_type) == 0L || is.na(track_type[[1]]) || !nzchar(track_type[[1]])) {
        return(shiny::div(class = "alert alert-danger", "Type de track manquant ou invalide."))
      }
      track_type <- track_type[[1]]
      params <- tryCatch(get_track_params(schema, track_type), error = function(e) NULL)
      if (length(params) == 0)
        return(shiny::p("Aucun paramètre pour ce type de track."))

      inputs <- lapply(names(params), function(pname) {
        pdef  <- params[[pname]]
        ptype <- pdef$type %||% "text"
        val   <- t$params[[pname]] %||% pdef$default
        label <- paste0(pname, if (!is.null(pdef$description)) paste0(" — ", pdef$description) else "")
        input_id <- ns(paste0("param_", pname))
        switch(ptype,
          "numeric" = shiny::numericInput(input_id, label, value = as.numeric(val %||% 0),
                                          min = pdef$min %||% NA_real_,
                                          max = pdef$max %||% NA_real_,
                                          step = pdef$step %||% NA_real_),
          "boolean" = shiny::checkboxInput(input_id, label, value = isTRUE(val)),
          "color"   = colourpicker::colourInput(input_id, label, value = as.character(val %||% "#333333")),
          "select"  = {
            choices <- unlist(pdef$choices %||% character(0), use.names = TRUE)
            if (length(choices) == 0L) choices <- ""
            shiny::selectInput(input_id, label, choices = choices,
                               selected = as.character(val %||% choices[[1]]))
          },
          shiny::textInput(input_id, label, value = as.character(val %||% ""))
        )
      })
      shiny::tagList(
        shiny::p(shiny::strong("Track : "), t$track_name,
                 shiny::span(class = "badge bg-secondary ms-2", t$track_type)),
        if (track_type %in% c("bigwig", "bedgraph"))
          shiny::div(
            class = "alert alert-info p-2 small",
            "Pour comparer plusieurs samples du même assay, utilisez Shared scale by group ou Robust shared scale."
          )
        else NULL,
        inputs
      )
    })

    # Save params
    shiny::observeEvent(input$btn_save_params, {
      ids <- selected_track_ids()
      tracks <- app_state$tracks
      if (!has_one_selection(ids)) {
        shiny::showNotification("Erreur : aucune track sélectionnée.", type = "error")
        return()
      }
      id <- ids[[1]]
      t <- get_track_by_id(tracks, id)
      if (is.null(t)) return()
      params <- get_track_params(schema, t$track_type)
      for (pname in names(params)) {
        input_id <- paste0("param_", pname)
        val      <- input[[input_id]]
        if (!is.null(val)) t$params[[pname]] <- val
      }
      t$updated_at <- track_now()
      validate_track(standardize_track(t, app_state$project_config))
      app_state$tracks <- update_track_by_id(tracks, id, t)
      save_tracks_and_invalidate()
      message("[TRACK] Saved track: ", id)
      shiny::showNotification("Track enregistrée dans le projet.", type = "message")
    })

    shiny::observeEvent(input$btn_apply_selected_colors, {
      ids <- selected_track_ids()
      if (is_empty(ids)) return()
      tracks <- app_state$tracks %||% list()
      for (i in seq_along(tracks)) {
        if (!tracks[[i]]$track_id %in% ids) next
        input_id <- batch_track_color_input_id(tracks[[i]]$track_id)
        color <- input[[input_id]] %||% NULL
        if (!is.null(color) && nzchar(color)) tracks[[i]]$params$color <- color
        tracks[[i]]$updated_at <- track_now()
      }
      app_state$tracks <- standardize_tracks(tracks, app_state$project_config)
      save_tracks_and_invalidate()
      shiny::showNotification(
        sprintf("Couleurs enregistrées pour %d tracks.", length(ids)),
        type = "message", duration = 5
      )
    }, ignoreInit = TRUE)

    set_selected_enabled <- function(value) {
      ids <- selected_track_ids()
      if (is_empty(ids)) {
        shiny::showNotification("Aucune track sélectionnée.", type = "warning")
        return()
      }
      tracks <- app_state$tracks
      for (i in seq_along(tracks)) {
        if (tracks[[i]]$track_id %in% ids) {
          tracks[[i]]$enabled <- isTRUE(value)
          tracks[[i]]$updated_at <- track_now()
        }
      }
      app_state$tracks <- standardize_tracks(tracks, app_state$project_config)
      save_tracks_and_invalidate()
      shiny::showNotification("Sélection mise à jour.", type = "message")
    }

    shiny::observeEvent(input$btn_enable_selected, {
      set_selected_enabled(TRUE)
    })

    shiny::observeEvent(input$btn_disable_selected, {
      set_selected_enabled(FALSE)
    })

    shiny::observeEvent(input$btn_duplicate_selected, {
      ids <- selected_track_ids()
      if (is_empty(ids)) {
        shiny::showNotification("Aucune track sélectionnée.", type = "warning")
        return()
      }
      tracks <- app_state$tracks
      dups <- lapply(ids, function(id) {
        dup <- get_track_by_id(tracks, id)
        if (is.null(dup)) return(NULL)
        dup$track_id <- new_track_id()
        dup$track_name <- paste0(dup$track_name, " (copie)")
        dup$order <- length(tracks) + which(ids == id)
        dup$created_at <- track_now()
        dup$updated_at <- track_now()
        dup
      })
      dups <- Filter(Negate(is.null), dups)
      app_state$tracks <- standardize_tracks(c(tracks, dups), app_state$project_config)
      app_state$selected_track_id <- NULL
      save_tracks_and_invalidate()
      message("[TRACK] Duplicated selected tracks: ", paste(ids, collapse = ", "))
    })

    shiny::observeEvent(input$btn_delete_selected, {
      ids <- selected_track_ids()
      if (is_empty(ids)) {
        shiny::showNotification("Aucune track sélectionnée.", type = "warning")
        return()
      }
      app_state$tracks <- standardize_tracks(
        Filter(function(t) !t$track_id %in% ids, app_state$tracks %||% list()),
        app_state$project_config
      )
      app_state$selected_track_id <- NULL
      save_tracks_and_invalidate()
      message("[TRACK] Deleted selected tracks: ", paste(ids, collapse = ", "))
    })

    shiny::observeEvent(input$btn_save_track_set, {
      ids <- selected_track_ids()
      if (is_empty(ids)) {
        shiny::showNotification("Aucune track sélectionnée.", type = "warning")
        return()
      }
      shiny::showModal(shiny::modalDialog(
        title = "Sauvegarder un ensemble de tracks",
        shiny::textInput(ns("track_set_name"), "Nom de l'ensemble"),
        shiny::textAreaInput(ns("track_set_description"), "Description", rows = 3),
        shiny::checkboxInput(ns("track_set_keep_file_paths"), "Conserver les chemins de fichiers", TRUE),
        shiny::selectInput(ns("track_set_category"), "Catégorie",
                           choices = c("ATAC-seq" = "ATAC-seq", "RNA-seq" = "RNA-seq",
                                       "WGBS" = "WGBS", "ChIP-seq" = "ChIP-seq", "custom" = "custom"),
                           selected = "custom"),
        footer = shiny::tagList(
          shiny::modalButton("Annuler"),
          shiny::actionButton(ns("confirm_save_track_set"), "Sauvegarder", class = "btn btn-primary")
        ),
        easyClose = TRUE
      ))
    })

    shiny::observeEvent(input$confirm_save_track_set, {
      ids <- selected_track_ids()
      if (is_empty(ids)) {
        shiny::removeModal()
        shiny::showNotification("Aucune track sélectionnée.", type = "warning")
        return()
      }
      tryCatch({
        set <- save_track_set_template(
          app_state$tracks %||% list(),
          selected_track_ids = ids,
          name = input$track_set_name,
          description = input$track_set_description,
          keep_file_paths = isTRUE(input$track_set_keep_file_paths),
          category = input$track_set_category
        )
        shiny::removeModal()
        track_set_refresh(track_set_refresh() + 1L)
        message("[TRACK_SET] Saved track set: ", set$track_set_id)
        shiny::showNotification("Ensemble de tracks sauvegardé.", type = "message")
      }, error = function(e) {
        shiny::showNotification(sprintf("Erreur ensemble : %s", e$message), type = "error")
      })
    })

    find_template <- function(template_id) {
      templates <- load_track_templates()
      hits <- Filter(function(t) identical(t$template_id %||% "", template_id), templates)
      if (length(hits) == 0L) return(NULL)
      hits[[1]]
    }

    apply_template_to_selected <- function(template_id, strong_replace = FALSE) {
      tmpl <- find_template(template_id)
      if (is.null(tmpl)) stop("Template introuvable.")
      id <- selected_track_id()
      if (is.null(id)) stop("Aucune track sélectionnée.")
      current <- get_track_by_id(app_state$tracks, id)
      if (is.null(current)) stop("Track sélectionnée introuvable.")
      templ_track <- standardize_track(tmpl$track)
      if (!strong_replace) {
        keep <- c("track_id", "track_name", "track_type", "file_id", "file_path", "file_name",
                  "file_type", "enabled", "order", "created_at", "source_project")
        for (nm in keep) templ_track[[nm]] <- current[[nm]]
      } else {
        templ_track$track_id <- current$track_id
        templ_track$order <- current$order
        templ_track$created_at <- current$created_at
        if (!nzchar(templ_track$file_path %||% "")) {
          templ_track$file_id <- current$file_id
          templ_track$file_path <- current$file_path
          templ_track$file_name <- current$file_name
          templ_track$file_type <- current$file_type
        }
      }
      templ_track$is_template <- FALSE
      templ_track$updated_at <- track_now()
      app_state$tracks <- update_track_by_id(app_state$tracks, id, templ_track)
      save_tracks_and_invalidate()
      message("[TEMPLATE] Applied template ", tmpl$template_name %||% template_id, " to track ", id)
      shiny::showNotification("Template appliqué.", type = "message")
    }

    append_template_track <- function(template_id) {
      tmpl <- find_template(template_id)
      if (is.null(tmpl)) stop("Template introuvable.")
      new_track <- apply_track_template(tmpl)
      new_track$order <- length(app_state$tracks %||% list()) + 1L
      app_state$tracks <- standardize_tracks(c(app_state$tracks %||% list(), list(new_track)), app_state$project_config)
      app_state$selected_track_id <- new_track$track_id
      save_tracks_and_invalidate()
      message("[TEMPLATE] Added template ", tmpl$template_name %||% template_id, " as track ", new_track$track_id)
      shiny::showNotification("Template ajouté comme nouvelle track.", type = "message")
    }

    shiny::observeEvent(input$btn_apply_template, {
      template_id <- input$template_select
      if (is.null(template_id) || nchar(template_id) == 0) {
        shiny::showNotification("Choisissez un template.", type = "warning")
        return()
      }
      tryCatch(apply_template_to_selected(template_id, strong_replace = FALSE),
               error = function(e) shiny::showNotification(sprintf("Erreur template : %s", e$message), type = "error"))
    })

    shiny::observeEvent(input$btn_tmpl_replace, {
      template_id <- input$template_select
      if (is.null(template_id) || nchar(template_id) == 0) {
        shiny::showNotification("Choisissez un template.", type = "warning")
        return()
      }
      tryCatch(apply_template_to_selected(template_id, strong_replace = TRUE),
               error = function(e) shiny::showNotification(sprintf("Erreur template : %s", e$message), type = "error"))
    })

    shiny::observeEvent(input$btn_tmpl_append, {
      template_id <- input$template_select
      if (is.null(template_id) || nchar(template_id) == 0) {
        shiny::showNotification("Choisissez un template.", type = "warning")
        return()
      }
      tryCatch(append_template_track(template_id),
               error = function(e) shiny::showNotification(sprintf("Erreur template : %s", e$message), type = "error"))
    })

    shiny::observeEvent(input$btn_save_as_template, {
      t <- selected_track()
      if (is.null(t)) {
        shiny::showNotification("Erreur : aucune track sélectionnée.", type = "error")
        return()
      }
      shiny::showModal(shiny::modalDialog(
        title = "Sauvegarder comme template",
        shiny::textInput(ns("template_name"), "Nom du template", value = t$track_name %||% ""),
        shiny::textAreaInput(ns("template_description"), "Description", rows = 3),
        shiny::selectInput(ns("template_category"), "Catégorie",
                           choices = c("bigWig" = "bigwig", "genes" = "genes",
                                       "peaks" = "peaks", "spacer" = "spacer", "custom" = "custom"),
                           selected = t$track_type %||% "custom"),
        shiny::checkboxInput(ns("template_keep_file_path"), "Conserver le chemin du fichier", FALSE),
        footer = shiny::tagList(
          shiny::modalButton("Annuler"),
          shiny::actionButton(ns("confirm_save_template"), "Sauvegarder", class = "btn btn-primary")
        ),
        easyClose = TRUE
      ))
    })

    shiny::observeEvent(input$confirm_save_template, {
      t <- selected_track()
      if (is.null(t)) {
        shiny::removeModal()
        shiny::showNotification("Erreur : aucune track sélectionnée.", type = "error")
        return()
      }
      tryCatch({
        tmpl <- save_track_template(
          t,
          template_name = input$template_name,
          template_description = input$template_description,
          keep_file_path = isTRUE(input$template_keep_file_path),
          category = input$template_category
        )
        shiny::removeModal()
        template_refresh(template_refresh() + 1L)
        shiny::showNotification("Template sauvegardé.", type = "message")
        message("[TEMPLATE] Saved template: ", tmpl$template_id)
      }, error = function(e) {
        shiny::showNotification(sprintf("Erreur template : %s", e$message), type = "error")
      })
    })

    find_track_set <- function(track_set_id) {
      sets <- load_track_set_templates()
      hits <- Filter(function(s) identical(s$track_set_id %||% "", track_set_id), sets)
      if (length(hits) == 0L) return(NULL)
      hits[[1]]
    }

    apply_selected_track_set <- function(replace = FALSE) {
      set_id <- input$track_set_select %||% ""
      if (!nzchar(set_id)) {
        shiny::showNotification("Choisissez un ensemble de tracks.", type = "warning")
        return()
      }
      set <- find_track_set(set_id)
      if (is.null(set)) {
        shiny::showNotification("Ensemble introuvable.", type = "error")
        return()
      }
      base <- if (isTRUE(replace)) list() else app_state$tracks %||% list()
      app_state$tracks <- apply_track_set_template(set, base, keep_template_file_paths = TRUE)
      app_state$selected_track_id <- NULL
      save_tracks_and_invalidate()
      if (isTRUE(replace)) {
        message("[TRACK_SET] Replaced project tracks with set: ", set$name %||% set_id)
        shiny::showNotification("Tracks remplacées par l'ensemble.", type = "message")
      } else {
        message("[TRACK_SET] Applied track set: ", set$name %||% set_id)
        shiny::showNotification("Ensemble ajouté au projet.", type = "message")
      }
    }

    shiny::observeEvent(input$btn_trackset_append, {
      apply_selected_track_set(replace = FALSE)
    })

    shiny::observeEvent(input$btn_trackset_replace, {
      set_id <- input$track_set_select %||% ""
      if (!nzchar(set_id)) {
        shiny::showNotification("Choisissez un ensemble de tracks.", type = "warning")
        return()
      }
      shiny::showModal(shiny::modalDialog(
        title = "Remplacer les tracks actuelles",
        shiny::p("Cette action remplacera toutes les tracks du projet par l'ensemble sélectionné."),
        footer = shiny::tagList(
          shiny::modalButton("Annuler"),
          shiny::actionButton(ns("confirm_trackset_replace"), "Remplacer", class = "btn btn-danger")
        ),
        easyClose = TRUE
      ))
    })

    shiny::observeEvent(input$confirm_trackset_replace, {
      shiny::removeModal()
      apply_selected_track_set(replace = TRUE)
    })

    shiny::observeEvent(input$btn_trackset_delete, {
      set_id <- input$track_set_select %||% ""
      if (!nzchar(set_id)) {
        shiny::showNotification("Choisissez un ensemble de tracks.", type = "warning")
        return()
      }
      delete_track_set_template(set_id)
      track_set_refresh(track_set_refresh() + 1L)
      message("[TRACK_SET] Deleted track set: ", set_id)
      shiny::showNotification("Ensemble supprimé.", type = "message")
    })
  })
}

# Helper: convert tracks list to a display data.frame
tracks_to_df <- function(tracks) {
  if (length(tracks) == 0) return(data.frame())
  do.call(rbind, lapply(seq_along(tracks), function(i) {
    t <- tracks[[i]]
    data.frame(
      track_id     = t$track_id %||% "",
      `#`          = i,
      Nom          = t$track_name %||% "?",
      Type         = t$track_type %||% "?",
      Fichier      = if (!is.null(t$file_path) && nchar(t$file_path) > 0) basename(t$file_path) else "—",
      Couleur      = as.character((t$params %||% list())$color %||% "—"),
      Activée      = if (isTRUE(t$enabled)) "✓" else "✗",
      check.names  = FALSE,
      stringsAsFactors = FALSE
    )
  }))
}
