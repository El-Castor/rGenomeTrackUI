# =============================================================================
# mod_track_builder.R — Track configuration module
# =============================================================================

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
          DT::DTOutput(ns("tracks_table"))
        ),
        shiny::tags$div(
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

    selected_track_id <- function() app_state$selected_track_id %||% NULL

    selected_track <- function() {
      id <- selected_track_id()
      if (is.null(id)) return(NULL)
      get_track_by_id(app_state$tracks %||% list(), id)
    }

    selected_track_ids <- function() {
      get_selected_track_ids(input, app_state$tracks %||% list())
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
       options = list(pageLength = 20, dom = "tip",
                      columnDefs = list(list(visible = FALSE, targets = 0)),
                      language = rt_dt_language()))

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
      ttype <- "x-axis"
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
      shiny::showNotification("Track 'x-axis' ajoutée.", type = "message")
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
        return(shiny::tagList(
          shiny::div(class = "alert alert-info p-2",
            sprintf("%d tracks sélectionnées.", length(ids))
          ),
          shiny::tags$div(class = "d-flex flex-wrap gap-2",
            shiny::actionButton(ns("btn_enable_selected"), "Activer les tracks sélectionnées", class = "btn btn-secondary btn-sm"),
            shiny::actionButton(ns("btn_disable_selected"), "Désactiver les tracks sélectionnées", class = "btn btn-secondary btn-sm"),
            shiny::actionButton(ns("btn_duplicate_selected"), "Dupliquer la sélection", class = "btn btn-secondary btn-sm"),
            shiny::actionButton(ns("btn_delete_selected"), "Supprimer la sélection", class = "btn btn-danger btn-sm"),
            shiny::actionButton(ns("btn_save_track_set"), "Sauvegarder sélection comme ensemble", class = "btn btn-primary btn-sm")
          )
        ))
      }
      shinyjs::enable("btn_save_params")
      shinyjs::enable("btn_save_as_template")
      t <- selected_track()
      if (is.null(t)) return(shiny::p(shiny::em("Track sélectionnée introuvable.")))
      params <- get_track_params(schema, t$track_type)
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
                                          min = pdef$min, max = pdef$max),
          "boolean" = shiny::checkboxInput(input_id, label, value = isTRUE(val)),
          "color"   = colourpicker::colourInput(input_id, label, value = as.character(val %||% "#333333")),
          "select"  = shiny::selectInput(input_id, label,
                                          choices = unlist(pdef$choices),
                                          selected = as.character(val %||% pdef$choices[[1]])),
          shiny::textInput(input_id, label, value = as.character(val %||% ""))
        )
      })
      shiny::tagList(
        shiny::p(shiny::strong("Track : "), t$track_name,
                 shiny::span(class = "badge bg-secondary ms-2", t$track_type)),
        if (t$track_type %in% c("bigwig", "bedgraph"))
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
      Activée      = if (isTRUE(t$enabled)) "✓" else "✗",
      check.names  = FALSE,
      stringsAsFactors = FALSE
    )
  }))
}
