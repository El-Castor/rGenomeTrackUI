# =============================================================================
# mod_inputs.R — File input and registry module
# =============================================================================

#' Inputs module UI
#'
#' @param id module namespace ID
#' @export
mod_inputs_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(

    omics_banner(
      "Fichiers d'entrée",
      "Importez et gérez les fichiers de données génomiques pour vos analyses.",
      small = TRUE
    ),

    shiny::uiOutput(ns("project_check")),

    bslib::navset_tab(
      id = ns("inputs_tabs"),

      # ---- Onglet 1 : Ajouter un fichier ----
      bslib::nav_panel(
        shiny::tagList(shiny::icon("plus-circle"), " Ajouter"),
        shiny::div(class = "mt-3",
          shiny::fluidRow(
            shiny::column(5,
              shiny::tags$div(
                class = "rt-card",
                shiny::tags$div(
                  class = "rt-card-header",
                  shiny::tags$span(class = "rt-card-icon", shiny::icon("file-import")),
                  shiny::tags$h5("Source du fichier")
                ),
                bslib::navset_tab(
                  bslib::nav_panel(
                    shiny::tagList(shiny::icon("upload"), " Upload"),
                    shiny::div(class = "mt-2",
                      shiny::fileInput(ns("file_upload"), "Choisir un fichier",
                                       accept = c(".bw", ".bigwig", ".bed", ".bedgraph", ".bg",
                                                  ".gtf", ".gff", ".gff3", ".narrowPeak",
                                                  ".bedpe", ".links")),
                      shiny::selectInput(ns("upload_track_type"), "Type de track",
                                         choices = c("Auto-détecté" = "")),
                      shiny::uiOutput(ns("upload_format_hint")),
                      shiny::selectInput(ns("upload_mode"), "Mode d'import",
                                         choices = c("Copier dans le projet" = "copy",
                                                     "Lien symbolique" = "link")),
                      shiny::textInput(ns("upload_notes"), "Notes (optionnel)"),
                      shiny::actionButton(ns("btn_add_upload"),
                        shiny::tagList(shiny::icon("plus"), " Ajouter au registre"),
                        class = "btn btn-primary w-100")
                    )
                  ),
                  bslib::nav_panel(
                    shiny::tagList(shiny::icon("hdd"), " Chemin local"),
                    shiny::div(class = "mt-2",
                      shiny::tags$div(
                        class = "alert alert-info",
                        shiny::icon("lightbulb"),
                        " Pour des fichiers volumineux (> 30 Mo) déjà présents sur le serveur."
                      ),
                      shiny::textInput(ns("local_path"), "Chemin absolu",
                                       placeholder = "/data/sample/file.bw"),
                      shiny::selectInput(ns("local_track_type"), "Type de track",
                                         choices = c("Auto-détecté" = "")),
                      shiny::uiOutput(ns("local_format_hint")),
                      shiny::selectInput(ns("local_mode"), "Mode d'import",
                                         choices = c("Copier" = "copy", "Lien symbolique" = "link")),
                      shiny::textInput(ns("local_notes"), "Notes (optionnel)"),
                      shiny::actionButton(ns("btn_add_local"),
                        shiny::tagList(shiny::icon("plus"), " Ajouter au registre"),
                        class = "btn btn-primary w-100")
                    )
                  )
                ),
                shiny::uiOutput(ns("add_feedback"))
              )
            ),
            shiny::column(7,
              shiny::tags$div(
                class = "rt-card",
                shiny::tags$div(
                  class = "rt-card-header",
                  shiny::tags$span(class = "rt-card-icon", shiny::icon("check-circle")),
                  shiny::tags$h5("Validation")
                ),
                shiny::uiOutput(ns("file_validation_panel"))
              )
            )
          )
        )
      ),

      # ---- Onglet 2 : Formats & templates ----
      bslib::nav_panel(
        shiny::tagList(shiny::icon("table"), " Formats & templates"),
        shiny::div(class = "mt-3",
          shiny::fluidRow(
            shiny::column(4,
              shiny::tags$div(
                class = "rt-card",
                shiny::tags$div(
                  class = "rt-card-header",
                  shiny::tags$span(class = "rt-card-icon", shiny::icon("file-code")),
                  shiny::tags$h5("Format")
                ),
                shiny::selectInput(ns("fmt_selector"), NULL,
                  choices = c(
                    "BED (annotations/features)" = "bed",
                    "BedGraph (signal texte)"     = "bedgraph",
                    "BigWig (signal binaire)"     = "bigwig",
                    "GTF/GFF (gènes)"             = "gtf",
                    "narrowPeak (pics ChIP)"      = "narrowpeak",
                    "BEDPE / Links"               = "bedpe",
                    "Domains (TADs)"              = "domains",
                    "Regions BED"                 = "regions",
                    "Lignes verticales"           = "vlines",
                    "Lignes horizontales"         = "hlines"
                  )
                ),
                shiny::uiOutput(ns("fmt_dl_buttons")),
                shiny::tags$hr(class = "divider"),
                shiny::downloadButton(ns("dl_templates_zip"),
                  shiny::tagList(shiny::icon("file-archive"), " Tous les templates (ZIP)"),
                  class = "btn btn-secondary btn-sm w-100")
              )
            ),
            shiny::column(8,
              shiny::tags$div(
                class = "rt-card",
                shiny::uiOutput(ns("fmt_help_panel"))
              )
            )
          )
        )
      ),

      # ---- Onglet 3 : Registre ----
      bslib::nav_panel(
        shiny::tagList(shiny::icon("list-alt"), " Registre"),
        shiny::div(class = "mt-3",
          shiny::tags$div(
            class = "rt-card",
            shiny::tags$div(
              class = "rt-card-header flex-between",
              shiny::tags$div(
                class = "flex-row gap-8",
                shiny::tags$span(class = "rt-card-icon", shiny::icon("database")),
                shiny::tags$h5("Fichiers enregistrés")
              ),
              shiny::tags$div(
                class = "d-flex gap-2",
                shiny::actionButton(ns("btn_refresh_registry"),
                  shiny::tagList(shiny::icon("sync"), " Actualiser"),
                  class = "btn btn-secondary btn-sm"),
                shiny::actionButton(ns("btn_remove"),
                  shiny::tagList(shiny::icon("trash"), " Supprimer"),
                  class = "btn btn-danger btn-sm")
              )
            ),
            DT::DTOutput(ns("registry_table"))
          )
        )
      ),

      # ---- Onglet 4 : Preview fichier ----
      bslib::nav_panel(
        shiny::tagList(shiny::icon("eye"), " Preview"),
        shiny::div(class = "mt-3",
          shiny::tags$div(
            class = "rt-card",
            shiny::tags$div(
              class = "rt-card-header",
              shiny::tags$span(class = "rt-card-icon", shiny::icon("file-alt")),
              shiny::tags$h5("Aperçu du fichier")
            ),
            shiny::uiOutput(ns("preview_selector_ui")),
            shiny::tags$hr(class = "divider"),
            shiny::uiOutput(ns("file_preview_panel"))
          )
        )
      )
    )
  )
}

#' Inputs module server
#'
#' @param id module namespace ID
#' @param app_state reactive values
#' @param schema schema list from load_track_schema
#' @export
mod_inputs_server <- function(id, app_state, schema) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    specs <- load_input_specs()

    # ---- Choix track type depuis schéma ----
    shiny::observe({
      choices <- c("Auto-détecté" = "", get_track_type_choices(schema))
      shiny::updateSelectInput(session, "upload_track_type", choices = choices)
      shiny::updateSelectInput(session, "local_track_type",  choices = choices)
    })

    output$project_check <- shiny::renderUI({
      if (is.null(app_state$project_config)) {
        shiny::div(class = "alert alert-warning",
          shiny::icon("exclamation-triangle"),
          " Aucun projet actif. Créez ou ouvrez un projet dans l'onglet ",
          shiny::strong("Projets"), " avant d'ajouter des fichiers.")
      }
    })

    # Désactiver le bouton d'ajout si pas de projet
    shiny::observe({
      has_project <- !is.null(app_state$project_config)
      shinyjs::toggleState("btn_add_upload", condition = has_project)
      shinyjs::toggleState("btn_add_local",  condition = has_project)
    })

    # ---- Hint format pour upload ----
    output$upload_format_hint <- shiny::renderUI({
      tt <- input$upload_track_type
      if (is.null(tt) || tt == "") return(NULL)
      fmts <- get_formats_for_track_type(tt, specs)
      exts <- get_extensions_for_track_type(tt, specs)
      if (length(fmts) == 0) return(NULL)
      shiny::div(class = "alert alert-info p-2 mt-1",
        shiny::icon("info-circle"),
        sprintf(" Formats compatibles : %s (%s)",
                paste(toupper(fmts), collapse = ", "),
                paste(exts, collapse = ", "))
      )
    })

    # ---- Hint format pour chemin local ----
    output$local_format_hint <- shiny::renderUI({
      tt <- input$local_track_type
      if (is.null(tt) || tt == "") return(NULL)
      fmts <- get_formats_for_track_type(tt, specs)
      exts <- get_extensions_for_track_type(tt, specs)
      if (length(fmts) == 0) return(NULL)
      shiny::div(class = "alert alert-info p-2 mt-1",
        shiny::icon("info-circle"),
        sprintf(" Formats compatibles : %s (%s)",
                paste(toupper(fmts), collapse = ", "),
                paste(exts, collapse = ", "))
      )
    })

    # ---- Panel de validation de fichier ----
    pending_file <- shiny::reactiveVal(NULL)  # list(path, name, track_type)

    shiny::observe({
      f <- input$file_upload
      if (!is.null(f)) {
        pending_file(list(path = f$datapath, name = f$name,
                          track_type = input$upload_track_type))
      }
    })
    shiny::observe({
      p <- trimws(input$local_path %||% "")
      tt <- input$local_track_type %||% ""
      if (nchar(p) > 0 && file.exists(p)) {
        pending_file(list(path = p, name = basename(p), track_type = tt))
      }
    })

    output$file_validation_panel <- shiny::renderUI({
      pf <- pending_file()
      if (is.null(pf)) {
        return(shiny::div(class = "text-muted mt-3",
          shiny::icon("file"), " Sélectionnez un fichier pour afficher sa validation."))
      }
      # Détecter le format depuis le type de track
      tt <- pf$track_type
      fmts <- if (!is.null(tt) && nchar(tt) > 0) get_formats_for_track_type(tt, specs) else c()
      fmt_id <- if (length(fmts) > 0) fmts[1] else NULL

      val <- if (!is.null(fmt_id)) {
        validate_against_format_spec(pf$path, fmt_id, specs)
      } else {
        list(status = "warning",
             messages = "Type de track non sélectionné — validation de format ignorée.",
             preview = tryCatch(head(readLines(pf$path, n = 5, warn = FALSE), 5),
                                error = function(e) NULL))
      }

      status_class <- switch(val$status,
        "ok"      = "success",
        "warning" = "warning",
        "error"   = "danger",
        "info"
      )
      status_icon <- switch(val$status,
        "ok"      = "check-circle",
        "warning" = "exclamation-triangle",
        "error"   = "times-circle",
        "info-circle"
      )

      preview_block <- if (!is.null(val$preview) && length(val$preview) > 0) {
        shiny::tagList(
          shiny::h6("Aperçu (5 premières lignes de données) :"),
          shiny::pre(style = "font-size:0.78em; max-height:150px; overflow:auto;",
                     paste(val$preview, collapse = "\n"))
        )
      } else NULL

      msg_block <- if (length(val$messages) > 0) {
        shiny::div(class = paste0("alert alert-", status_class, " p-2 mt-2"),
          shiny::icon(status_icon), " ",
          paste(val$messages, collapse = " | ")
        )
      } else {
        shiny::div(class = "alert alert-success p-2 mt-2",
          shiny::icon("check-circle"), " Fichier valide.")
      }

      shiny::tagList(
        shiny::h6(shiny::icon("file"), " ", pf$name),
        msg_block,
        preview_block
      )
    })

    # ---- Registre ----
    refresh_registry <- function() {
      if (!is.null(app_state$project_config)) {
        app_state$registry <- load_file_registry(app_state$project_config)
      }
    }

    shiny::observeEvent(input$btn_refresh_registry, { refresh_registry() })

    output$registry_table <- DT::renderDT({
      reg <- app_state$registry
      if (is.null(reg) || nrow(reg) == 0)
        return(data.frame(Message = "Registre vide."))
      cols <- intersect(c("file_id", "original_name", "track_type_selected",
                           "linked_or_copied", "size_bytes", "date_added", "status", "notes"),
                        names(reg))
      reg[, cols, drop = FALSE]
    }, selection = "single",
       options = list(pageLength = 15, scrollX = TRUE, dom = "tip",
                      language = list(
                        url = "//cdn.datatables.net/plug-ins/1.13.1/i18n/fr-FR.json"
                      )))

    shiny::observeEvent(input$registry_table_rows_selected, {
      shinyjs::enable("btn_remove")
    })

    # ---- Preview d'un fichier du registre ----
    output$preview_selector_ui <- shiny::renderUI({
      reg <- app_state$registry
      if (is.null(reg) || nrow(reg) == 0)
        return(shiny::p(shiny::em("Registre vide.")))
      choices <- stats::setNames(reg$file_path %||% reg$file_id, reg$original_name)
      shiny::selectInput(ns("preview_file_sel"), "Fichier à prévisualiser :", choices = choices)
    })

    output$file_preview_panel <- shiny::renderUI({
      sel <- input$preview_file_sel
      if (is.null(sel) || sel == "") return(NULL)
      if (!file.exists(sel)) return(shiny::p(shiny::em("Fichier introuvable.")))
      # Binaire ?
      ext <- tolower(tools::file_ext(sel))
      binary_exts <- c("bw", "bigwig", "tbi", "bai")
      if (ext %in% binary_exts) {
        return(shiny::div(class = "alert alert-info",
          shiny::icon("binary"), " Fichier binaire — aperçu non disponible."))
      }
      lines <- tryCatch(readLines(sel, n = 50, warn = FALSE), error = function(e) NULL)
      if (is.null(lines)) return(shiny::p("Impossible de lire le fichier."))
      shiny::pre(style = "font-size:0.78em; max-height:400px; overflow:auto;",
                 paste(lines, collapse = "\n"))
    })

    # ---- Ajouter un fichier ----
    do_add_file <- function(path, mode, track_type, notes, original_name = NULL) {
      if (is.null(app_state$project_config)) {
        output$add_feedback <- shiny::renderUI({
          shiny::div(class = "alert alert-danger mt-2 p-2",
            shiny::icon("times-circle"),
            " Aucun projet actif — ouvrez un projet dans l'onglet ",
            shiny::strong("Projets"), " d'abord.")
        })
        return()
      }
      tryCatch({
        app_state$registry <- add_file_to_registry(
          project_config = app_state$project_config,
          source_path    = path,
          mode           = mode,
          track_type     = if (!is.null(track_type) && nchar(track_type) > 0) track_type else NULL,
          notes          = notes,
          original_name  = original_name
        )
        fname <- basename(path)
        shiny::showNotification(sprintf("Fichier ajouté : %s", fname), type = "message")
        output$add_feedback <- shiny::renderUI({
          shiny::div(class = "alert alert-success mt-2 p-2",
            shiny::icon("check"), sprintf(" Ajouté : %s", fname))
        })
        pending_file(NULL)
      }, error = function(e) {
        shiny::showNotification(sprintf("Erreur : %s", e$message), type = "error")
        output$add_feedback <- shiny::renderUI({
          shiny::div(class = "alert alert-danger mt-2 p-2",
            shiny::icon("times"), sprintf(" Erreur : %s", e$message))
        })
      })
    }

    shiny::observeEvent(input$btn_add_upload, {
      req(input$file_upload)
      do_add_file(
        path          = input$file_upload$datapath,
        mode          = input$upload_mode,
        track_type    = input$upload_track_type,
        notes         = input$upload_notes,
        original_name = input$file_upload$name   # nom original, pas le chemin temp
      )
    })

    shiny::observeEvent(input$btn_add_local, {
      path <- trimws(input$local_path)
      if (nchar(path) == 0) {
        shiny::showNotification("Chemin vide.", type = "warning")
        return()
      }
      do_add_file(path, input$local_mode, input$local_track_type, input$local_notes)
    })

    shiny::observeEvent(input$btn_remove, {
      sel <- input$registry_table_rows_selected
      if (is.null(sel) || is.null(app_state$registry)) return()
      reg <- app_state$registry
      reg <- reg[-sel, , drop = FALSE]
      save_file_registry(reg, app_state$project_config)
      app_state$registry <- reg
      shiny::showNotification("Entrée supprimée du registre.", type = "message")
    })

    # ---- Téléchargements formats ----
    output$fmt_help_panel <- shiny::renderUI({
      req(input$fmt_selector)
      shiny::HTML(render_format_help(input$fmt_selector, specs))
    })

    output$fmt_dl_buttons <- shiny::renderUI({
      req(input$fmt_selector)
      fid <- input$fmt_selector
      tpl <- get_template_path(fid, "templates/input_files", specs)
      ex  <- get_example_path(fid, "examples/input_files", specs)
      btns <- list()
      if (!is.null(tpl)) {
        btns[[1]] <- shiny::downloadButton(
          ns(paste0("dl_tpl_", fid)), "Template",
          class = "btn btn-outline-secondary btn-sm mt-1 w-100"
        )
      }
      if (!is.null(ex)) {
        btns[[length(btns)+1]] <- shiny::downloadButton(
          ns(paste0("dl_ex_", fid)), "Exemple",
          class = "btn btn-outline-info btn-sm mt-1 w-100"
        )
      }
      if (length(btns) == 0) shiny::em("Aucun fichier disponible.")
      else shiny::div(btns)
    })

    for (fmt_id in names(specs)) {
      local({
        fid <- fmt_id
        output[[paste0("dl_tpl_", fid)]] <- shiny::downloadHandler(
          filename = function() specs[[fid]]$template_file %||% paste0("template_", fid, ".txt"),
          content  = function(file) {
            p <- get_template_path(fid, "templates/input_files", specs)
            if (!is.null(p)) file.copy(p, file)
          }
        )
        output[[paste0("dl_ex_", fid)]] <- shiny::downloadHandler(
          filename = function() specs[[fid]]$example_file %||% paste0("example_", fid, ".txt"),
          content  = function(file) {
            p <- get_example_path(fid, "examples/input_files", specs)
            if (!is.null(p)) file.copy(p, file)
          }
        )
      })
    }

    output$dl_templates_zip <- shiny::downloadHandler(
      filename = function() "rGenomeTrackUI_input_templates.zip",
      content  = function(file) {
        zip_path <- "templates/input_files/rGenomeTrackUI_input_templates.zip"
        if (file.exists(zip_path)) file.copy(zip_path, file)
      }
    )
  })
}
