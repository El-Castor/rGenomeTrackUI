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
    shiny::h3("Track Builder"),
    shiny::uiOutput(ns("project_check")),
    shiny::fluidRow(
      shiny::column(4,
        bslib::card(
          bslib::card_header(shiny::icon("plus-circle"), " Ajouter une track"),
          shiny::selectInput(ns("new_track_type"), "Type de track",
                             choices = c("—" = "")),
          shiny::uiOutput(ns("track_type_help")),
          shiny::textInput(ns("new_track_name"), "Nom de la track",
                           placeholder = "ex: H3K27ac signal"),
          shiny::uiOutput(ns("file_selector_ui")),
          shiny::uiOutput(ns("file_compat_warning")),
          shiny::div(class = "d-flex gap-2 mb-2",
            shiny::actionButton(ns("btn_add_track"),
              shiny::icon("plus"), " Ajouter",
              class = "btn btn-primary flex-fill"),
            shiny::actionButton(ns("btn_add_xaxis"),
              "＋ x-axis", class = "btn btn-outline-secondary btn-sm"),
            shiny::actionButton(ns("btn_add_spacer"),
              "＋ espace", class = "btn btn-outline-secondary btn-sm")
          ),
          shiny::hr(),
          shiny::h6("Templates"),
          shiny::selectInput(ns("template_select"), NULL,
                             choices = c("—" = "")),
          shiny::actionButton(ns("btn_apply_template"), "Appliquer le template",
                              class = "btn btn-outline-info btn-sm w-100")
        )
      ),
      shiny::column(8,
        bslib::card(
          bslib::card_header(shiny::icon("list"), " Tracks configurées"),
          DT::DTOutput(ns("tracks_table")),
          shiny::br(),
          shiny::div(class = "d-flex gap-2 flex-wrap",
            shiny::actionButton(ns("btn_toggle"), shiny::icon("eye"), " Activer/Désactiver",
                                class = "btn btn-sm btn-outline-warning"),
            shiny::actionButton(ns("btn_dup"),    shiny::icon("copy"), " Dupliquer",
                                class = "btn btn-sm btn-outline-secondary"),
            shiny::actionButton(ns("btn_delete"), shiny::icon("trash"), " Supprimer",
                                class = "btn btn-sm btn-outline-danger"),
            shiny::actionButton(ns("btn_up"),   shiny::icon("arrow-up"),   " Monter",
                                class = "btn btn-sm btn-outline-dark"),
            shiny::actionButton(ns("btn_down"), shiny::icon("arrow-down"), " Descendre",
                                class = "btn btn-sm btn-outline-dark")
          )
        ),
        bslib::card(
          bslib::card_header(shiny::icon("sliders-h"), " Paramètres de la track sélectionnée"),
          shiny::uiOutput(ns("edit_params_ui"))
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

    # Track type choices
    shiny::observe({
      choices <- c("—" = "", get_track_type_choices(schema))
      shiny::updateSelectInput(session, "new_track_type", choices = choices)
    })

    # Template choices
    shiny::observe({
      tmpls <- tryCatch(list_templates(), error = function(e) character(0))
      choices <- c("—" = "")
      if (length(tmpls) > 0) {
        base_names <- tools::file_path_sans_ext(basename(tmpls))
        choices <- c("—" = "", setNames(tmpls, base_names))
      }
      shiny::updateSelectInput(session, "template_select", choices = choices)
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
    }, selection = "single",
       options = list(pageLength = 20, dom = "tip",
                      language = list(
                        url = "//cdn.datatables.net/plug-ins/1.13.1/i18n/fr-FR.json"
                      )))

    shiny::observeEvent(input$tracks_table_rows_selected, {
      for (btn in c("btn_toggle", "btn_dup", "btn_delete", "btn_up", "btn_down"))
        shinyjs::enable(btn)
    })

    # Ajouter un track x-axis
    shiny::observeEvent(input$btn_add_xaxis, {
      ttype <- "x-axis"
      tname <- "Axe X"
      current <- app_state$tracks
      new_t <- list(
        track_id   = paste0("t_", format(Sys.time(), "%Y%m%d%H%M%S"), "_xaxis"),
        track_name = tname, track_type = ttype,
        file_id = NULL, file_path = "",
        enabled = TRUE, order = length(current) + 1,
        params = get_track_default_params(schema, ttype),
        created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
        updated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
      )
      app_state$tracks <- c(current, list(new_t))
      shiny::showNotification("Track 'x-axis' ajoutée.", type = "message")
    })

    # Ajouter un spacer
    shiny::observeEvent(input$btn_add_spacer, {
      ttype <- "spacer"
      tname <- "Espace"
      current <- app_state$tracks
      new_t <- list(
        track_id   = paste0("t_", format(Sys.time(), "%Y%m%d%H%M%S"), "_spacer"),
        track_name = tname, track_type = ttype,
        file_id = NULL, file_path = "",
        enabled = TRUE, order = length(current) + 1,
        params = get_track_default_params(schema, ttype),
        created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
        updated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
      )
      app_state$tracks <- c(current, list(new_t))
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
        track_id   = paste0("t_", format(Sys.time(), "%Y%m%d%H%M%S"), "_", sample.int(999, 1)),
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
      app_state$tracks <- c(current_tracks, list(new_track))
      shiny::showNotification(sprintf("Track '%s' ajoutée.", tname), type = "message")
    })

    # Toggle enable/disable
    shiny::observeEvent(input$btn_toggle, {
      sel    <- input$tracks_table_rows_selected
      if (is.null(sel)) return()
      tracks <- app_state$tracks
      tracks[[sel]]$enabled <- !isTRUE(tracks[[sel]]$enabled)
      app_state$tracks <- tracks
    })

    # Duplicate track
    shiny::observeEvent(input$btn_dup, {
      sel    <- input$tracks_table_rows_selected
      if (is.null(sel)) return()
      tracks <- app_state$tracks
      dup    <- tracks[[sel]]
      dup$track_id   <- paste0("t_", format(Sys.time(), "%Y%m%d%H%M%S"), "_dup")
      dup$track_name <- paste0(dup$track_name, " (copie)")
      dup$order      <- length(tracks) + 1
      app_state$tracks <- c(tracks, list(dup))
    })

    # Delete track
    shiny::observeEvent(input$btn_delete, {
      sel    <- input$tracks_table_rows_selected
      if (is.null(sel)) return()
      tracks <- app_state$tracks
      tracks <- tracks[-sel]
      for (i in seq_along(tracks)) tracks[[i]]$order <- i
      app_state$tracks <- tracks
    })

    # Move up
    shiny::observeEvent(input$btn_up, {
      sel    <- input$tracks_table_rows_selected
      if (is.null(sel) || sel <= 1) return()
      tracks <- app_state$tracks
      tmp    <- tracks[[sel - 1]]; tracks[[sel - 1]] <- tracks[[sel]]; tracks[[sel]] <- tmp
      for (i in seq_along(tracks)) tracks[[i]]$order <- i
      app_state$tracks <- tracks
    })

    # Move down
    shiny::observeEvent(input$btn_down, {
      sel    <- input$tracks_table_rows_selected
      tracks <- app_state$tracks
      if (is.null(sel) || sel >= length(tracks)) return()
      tmp    <- tracks[[sel + 1]]; tracks[[sel + 1]] <- tracks[[sel]]; tracks[[sel]] <- tmp
      for (i in seq_along(tracks)) tracks[[i]]$order <- i
      app_state$tracks <- tracks
    })

    # Edit params UI
    output$edit_params_ui <- shiny::renderUI({
      sel    <- input$tracks_table_rows_selected
      tracks <- app_state$tracks
      if (is.null(sel) || sel > length(tracks))
        return(shiny::p(shiny::em("Sélectionnez une track dans le tableau.")))
      t      <- tracks[[sel]]
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
          "color"   = shinyjs::colourInput(input_id, label, value = as.character(val %||% "#333333")),
          "select"  = shiny::selectInput(input_id, label,
                                          choices = unlist(pdef$choices),
                                          selected = as.character(val %||% pdef$choices[[1]])),
          shiny::textInput(input_id, label, value = as.character(val %||% ""))
        )
      })
      shiny::tagList(
        shiny::p(shiny::strong("Track : "), t$track_name,
                 shiny::span(class = "badge bg-secondary ms-2", t$track_type)),
        inputs,
        shiny::actionButton(ns("btn_save_params"),
          shiny::icon("save"), " Sauvegarder les paramètres",
          class = "btn btn-success mt-2")
      )
    })

    # Save params
    shiny::observeEvent(input$btn_save_params, {
      sel    <- input$tracks_table_rows_selected
      tracks <- app_state$tracks
      if (is.null(sel) || sel > length(tracks)) return()
      t      <- tracks[[sel]]
      params <- get_track_params(schema, t$track_type)
      for (pname in names(params)) {
        input_id <- paste0("param_", pname)
        val      <- input[[input_id]]
        if (!is.null(val)) t$params[[pname]] <- val
      }
      t$updated_at      <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
      tracks[[sel]]     <- t
      app_state$tracks  <- tracks
      shiny::showNotification("Paramètres sauvegardés.", type = "message")
    })

    # Apply template
    pending_tmpl_path <- shiny::reactiveVal(NULL)

    do_apply_template <- function(tmpl_path, replace) {
      tryCatch({
        tmpl   <- load_template(tmpl_path)
        result <- apply_template(tmpl, app_state$project_config, app_state$registry)
        if (replace) {
          app_state$tracks <- result$tracks
        } else {
          app_state$tracks <- c(app_state$tracks, result$tracks)
        }
        n_added  <- length(result$tracks)
        action   <- if (replace) "appliqué (tracks remplacées)" else "ajouté à la suite"
        shiny::showNotification(
          sprintf("Template '%s' %s : %d track(s).", basename(tmpl_path), action, n_added),
          type = "message")
      }, error = function(e) {
        shiny::showNotification(sprintf("Erreur template : %s", e$message), type = "error")
      })
    }

    shiny::observeEvent(input$btn_apply_template, {
      tmpl_path <- input$template_select
      if (is.null(tmpl_path) || nchar(tmpl_path) == 0) {
        shiny::showNotification("Choisissez un template.", type = "warning")
        return()
      }
      pending_tmpl_path(tmpl_path)
      if (length(app_state$tracks) > 0) {
        shiny::showModal(shiny::modalDialog(
          title     = shiny::tagList(shiny::icon("layer-group"), " Appliquer le template"),
          shiny::p(sprintf("Vous avez déjà %d track(s) configurée(s).", length(app_state$tracks))),
          shiny::p("Choisissez l'action à effectuer :"),
          footer = shiny::tagList(
            shiny::actionButton(ns("btn_tmpl_replace"), "Remplacer tout",
                                class = "btn btn-danger btn-sm"),
            shiny::actionButton(ns("btn_tmpl_append"),  "Ajouter à la suite",
                                class = "btn btn-primary btn-sm"),
            shiny::modalButton("Annuler")
          ),
          easyClose = TRUE
        ))
      } else {
        do_apply_template(tmpl_path, replace = TRUE)
      }
    })

    shiny::observeEvent(input$btn_tmpl_replace, {
      shiny::removeModal()
      do_apply_template(pending_tmpl_path(), replace = TRUE)
    })

    shiny::observeEvent(input$btn_tmpl_append, {
      shiny::removeModal()
      do_apply_template(pending_tmpl_path(), replace = FALSE)
    })
  })
}

# Helper: convert tracks list to a display data.frame
tracks_to_df <- function(tracks) {
  if (length(tracks) == 0) return(data.frame())
  do.call(rbind, lapply(seq_along(tracks), function(i) {
    t <- tracks[[i]]
    data.frame(
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
