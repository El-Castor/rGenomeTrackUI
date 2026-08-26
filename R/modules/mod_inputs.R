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
                      shiny::div(
                        class = "alert alert-info p-2 mb-2",
                        shiny::icon("info-circle"),
                        shiny::HTML(" Les fichiers uploadés sont toujours <strong>copiés</strong> dans le projet.
                          Pour les très gros fichiers (&gt; 100 Mo), privilégiez
                          <strong>Chemin local + Lien symbolique</strong>.")
                      ),
                      shiny::fileInput(ns("file_upload"), "Choisir un ou plusieurs fichiers",
                                       multiple = TRUE,
                                       accept = c(".bw", ".bigwig", ".bed", ".bedgraph", ".bg",
                                                  ".gtf", ".gff", ".gff3", ".narrowPeak",
                                                  ".bedpe", ".links")),
                      shiny::div(
                        id = ns("upload_status"),
                        class = "rt-upload-status",
                        role = "status",
                        `aria-live` = "polite"
                      ),
                      shiny::selectInput(ns("upload_track_type"), "Type de track",
                                         choices = c("Auto-détecté" = "")),
                      shiny::uiOutput(ns("upload_format_hint")),
                      shiny::textInput(ns("upload_notes"), "Notes (optionnel)"),
                      shiny::actionButton(ns("btn_add_upload"),
                        shiny::tagList(shiny::icon("plus"), " Ajouter les fichiers au registre"),
                        class = "btn btn-primary w-100")
                    )
                  ),
                  bslib::nav_panel(
                    shiny::tagList(shiny::icon("hdd"), " Chemin local"),
                    shiny::div(class = "mt-2",
                      shiny::tags$div(
                        class = "alert alert-info p-2 mb-2",
                        shiny::icon("lightbulb"),
                        shiny::HTML(" <strong>Recommand&#233; pour les gros fichiers (&gt; 100 Mo)</strong> d&#233;j&#224; pr&#233;sents sur la machine (BigWig, HiC&#8230;). Permet <strong>Copier</strong> ou <strong>Lien symbolique</strong> pour &#233;viter la duplication.")
                      ),
                      shiny::tags$label("Chemin absolu du fichier", class = "form-label"),
                      shiny::div(
                        class = "d-flex gap-2 align-items-end",
                        shiny::div(
                          class = "flex-grow-1",
                          shiny::textInput(ns("local_path"), NULL,
                                           placeholder = "/data/sample/file.bw",
                                           width = "100%")
                        ),
                        shiny::div(
                          class = "mb-3",
                          shinyFiles::shinyFilesButton(
                            id         = ns("browse_local_file"),
                            label      = shiny::tagList(shiny::icon("folder-open"), " Parcourir\u2026"),
                            title      = "S\u00e9lectionner un fichier local",
                            multiple   = FALSE,
                            buttonType = "outline-secondary"
                          )
                        )
                      ),
                      shiny::selectInput(ns("local_track_type"), "Type de track",
                                         choices = c("Auto-d\u00e9tect\u00e9" = "")),
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

    # ---- File browser shinyFiles pour Chemin local ----
    # Racines de navigation (macOS/Linux, dirs non-existants filtrés)
    browse_roots <- local({
      candidates <- c(
        Home      = normalizePath("~",           mustWork = FALSE),
        Documents = normalizePath("~/Documents", mustWork = FALSE),
        Desktop   = normalizePath("~/Desktop",   mustWork = FALSE),
        Downloads = normalizePath("~/Downloads", mustWork = FALSE)
      )
      if (dir.exists("/Volumes")) candidates["Volumes"] <- "/Volumes"
      candidates[vapply(candidates, dir.exists, logical(1))]
    })

    shinyFiles::shinyFileChoose(
      input     = input,
      id        = "browse_local_file",
      roots     = browse_roots,
      session   = session,
      filetypes = c("bw", "bigwig", "bigWig", "bed", "bedgraph", "bg",
                    "gtf", "gff", "gff3", "narrowPeak", "narrowpeak",
                    "bedpe", "links", "cool", "mcool", "hic", "h5")
    )

    shiny::observeEvent(input$browse_local_file, {
      if (is.integer(input$browse_local_file)) return()
      parsed <- shinyFiles::parseFilePaths(browse_roots, input$browse_local_file)
      if (nrow(parsed) > 0) {
        selected_path <- as.character(parsed$datapath[[1]])
        shiny::updateTextInput(session, "local_path", value = selected_path)

        # Auto-detect track type from extension
        detected <- detect_file_type(selected_path)
        if (!is.null(detected) && detected != "unknown") {
          shiny::updateSelectInput(session, "local_track_type",
                                   selected = file_type_to_track_type(detected))
        }

        # Notification avec taille du fichier
        fsize     <- file.info(selected_path)$size
        fsize_str <- if (!is.null(fsize) && !is.na(fsize)) {
          if      (fsize >= 1073741824L) sprintf("%.1f Go", fsize / 1073741824)
          else if (fsize >= 1048576L)    sprintf("%.1f Mo", fsize / 1048576)
          else if (fsize >= 1024L)       sprintf("%.1f Ko", fsize / 1024)
          else                           sprintf("%d o",    as.integer(fsize))
        } else "taille inconnue"

        shiny::showNotification(
          sprintf("Fichier s\u00e9lectionn\u00e9 : %s (%s)", basename(selected_path), fsize_str),
          type     = "message",
          duration = 4
        )
      }
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    output$project_check <- shiny::renderUI({
      if (is.null(app_state$project_config)) {
        shiny::div(class = "alert alert-warning",
          shiny::icon("exclamation-triangle"),
          " Aucun projet actif. Créez ou ouvrez un projet dans l'onglet ",
          shiny::strong("Projets"), " avant d'ajouter des fichiers.")
      }
    })

    # Le bouton upload ne devient actif qu'après confirmation par le serveur
    # que le transfert Shiny est terminé et que le fichier temporaire existe.
    shiny::observe({
      has_project <- !is.null(app_state$project_config)
      f <- input$file_upload
      upload_ready <- has_project && !is.null(f) && nrow(f) > 0 &&
        all(file.exists(f$datapath))
      shinyjs::toggleState("btn_add_upload", condition = upload_ready)
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

    # ---- Feedback état ajout (pattern correct : reactiveVal lu par renderUI) ----
    add_result <- shiny::reactiveVal(NULL)  # list(type, msg)

    output$add_feedback <- shiny::renderUI({
      res <- add_result()
      if (is.null(res)) return(NULL)
      alert_class <- switch(res$type,
        "success" = "alert-success",
        "error"   = "alert-danger",
        "warning" = "alert-warning",
        "alert-info"
      )
      icon_name <- switch(res$type,
        "success" = "check-circle",
        "error"   = "times-circle",
        "warning" = "exclamation-triangle",
        "info-circle"
      )
      shiny::div(
        class = paste("alert mt-2 p-2", alert_class),
        shiny::icon(icon_name), " ", shiny::HTML(res$msg)
      )
    })

    # ---- Panel de validation de fichier ----
    pending_file <- shiny::reactiveVal(NULL)  # list(path, name, track_type)

    shiny::observeEvent(input$file_upload, {
      f <- input$file_upload
      shiny::req(!is.null(f), nrow(f) > 0)
      selected_type <- input$upload_track_type %||% ""
      if (nrow(f) > 1L) {
        selected_type <- ""
        shiny::updateSelectInput(session, "upload_track_type", selected = "")
      } else if (!nzchar(selected_type)) {
        detected_type <- file_type_to_track_type(detect_file_type(f$name[[1]]))
        if (!is.null(detected_type) && !identical(detected_type, "unknown")) {
          selected_type <- detected_type
          shiny::updateSelectInput(session, "upload_track_type", selected = detected_type)
        }
      }
      message(sprintf("[Inputs] Upload received: %d file(s): %s",
                      nrow(f), paste(f$name, collapse = ", ")))
      pending_file(list(path = f$datapath, name = f$name, size = f$size,
                        track_type = selected_type))
      add_result(NULL)
      session$sendCustomMessage("rt_upload_received", list(
        id = ns("upload_status"),
        name = if (nrow(f) == 1L) f$name[[1]] else paste(f$name, collapse = ", "),
        count = nrow(f),
        size = sum(f$size, na.rm = TRUE)
      ))
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
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
      if (length(pf$path) > 1L) {
        rows <- lapply(seq_along(pf$path), function(i) {
          detected <- detect_file_type(pf$name[[i]])
          track_type <- file_type_to_track_type(detected)
          exists <- file.exists(pf$path[[i]])
          size <- suppressWarnings(as.numeric(pf$size[[i]] %||% file.info(pf$path[[i]])$size))
          size_label <- if (!is.na(size) && size >= 1048576) {
            sprintf("%.1f Mo", size / 1048576)
          } else if (!is.na(size)) {
            sprintf("%.1f Ko", size / 1024)
          } else "taille inconnue"
          shiny::div(
            class = "d-flex justify-content-between align-items-center border-bottom py-2 gap-2",
            shiny::div(shiny::icon(if (exists) "check-circle" else "times-circle"), " ",
                       shiny::tags$strong(pf$name[[i]])),
            shiny::tags$small(
              class = if (exists) "text-muted text-nowrap" else "text-danger text-nowrap",
              sprintf("%s · %s", if (identical(track_type, "unknown")) "type inconnu" else track_type,
                      size_label)
            )
          )
        })
        return(shiny::tagList(
          shiny::div(class = "alert alert-info p-2 mb-2",
            shiny::icon("copy"), " ",
            shiny::tags$strong(sprintf("%d fichiers prêts à être ajoutés", length(pf$path))),
            shiny::tags$br(),
            shiny::tags$small("Le type sera détecté séparément pour chaque fichier.")
          ),
          rows
        ))
      }
      # Détecter le format depuis le type de track
      tt <- pf$track_type
      fmts <- if (!is.null(tt) && nchar(tt) > 0) get_formats_for_track_type(tt, specs) else c()
      fmt_id <- if (length(fmts) > 0) fmts[1] else NULL

      # Le chemin temporaire Shiny n'est pas une source fiable pour le format.
      # En cas de retard du selectInput, utiliser le nom original et surtout ne
      # jamais envoyer un fichier binaire dans readLines().
      detected_format <- detect_file_type(pf$name)
      if (is.null(fmt_id) && detected_format %in% names(specs)) {
        fmt_id <- detected_format
      }
      original_ext <- tolower(tools::file_ext(pf$name %||% ""))
      is_known_binary <- original_ext %in% c("bw", "bigwig", "cool", "mcool", "hic", "h5") ||
        (!is.null(fmt_id) && isTRUE(specs[[fmt_id]]$binary))

      val <- if (!is.null(fmt_id)) {
        validate_against_format_spec(
          pf$path, fmt_id, specs,
          original_name = pf$name
        )
      } else if (is_known_binary) {
        list(
          status = "warning",
          messages = "Fichier binaire détecté — aperçu texte désactivé.",
          preview = NULL
        )
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
                      language = rt_dt_language()))

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
    do_add_file <- function(path, mode, track_type, notes, original_name = NULL, source_type = "local") {
      message(sprintf("[Inputs] Adding file to registry"))
      message(sprintf("[Inputs] source=%s", source_type))
      message(sprintf("[Inputs] mode=%s", mode))
      message(sprintf("[Inputs] file_type=%s", track_type %||% "(auto)"))
      message(sprintf("[Inputs] src=%s", path))

      if (is.null(app_state$project_config)) {
        msg <- "Aucun projet actif \u2014 ouvrez un projet dans l'onglet <strong>Projets</strong> d'abord."
        message("[Inputs] ERROR: no active project")
        shiny::showNotification("Aucun projet actif.", type = "error", duration = 6)
        add_result(list(type = "error", msg = msg))
        return(invisible(NULL))
      }

      # Upload : interdire le symlink (le fichier temp sera supprimé à la fin de session)
      warn_symlink <- FALSE
      if (source_type == "upload" && mode == "link") {
        message("[Inputs] WARNING: upload+symlink requested, converting to copy")
        mode <- "copy"
        warn_symlink <- TRUE
      }

      tryCatch({
        shinyjs::disable("btn_add_upload")
        shinyjs::disable("btn_add_local")
        on.exit({
          shinyjs::enable("btn_add_upload")
          shinyjs::enable("btn_add_local")
        })

        proj_path <- app_state$project_config$project_path
        message(sprintf("[Inputs] project=%s", proj_path))

        updated_reg <- add_file_to_registry(
          project_config = app_state$project_config,
          source_path    = path,
          mode           = mode,
          track_type     = if (!is.null(track_type) && nchar(track_type) > 0) track_type else NULL,
          notes          = notes,
          original_name  = original_name
        )
        app_state$registry <- updated_reg

        last_entry <- updated_reg[nrow(updated_reg), ]
        dst        <- last_entry$stored_path %||% ""
        fname      <- last_entry$original_name %||% basename(path)
        message(sprintf("[Inputs] dst=%s", dst))
        message(sprintf("[Inputs] validation=%s", last_entry$status %||% "ok"))
        message(sprintf("[Inputs] registry updated — %d total entries", nrow(updated_reg)))

        notif_msg <- if (warn_symlink) {
          sprintf("Fichier ajouté (copié) : %s", fname)
        } else {
          sprintf("Fichier ajouté : %s", fname)
        }
        shiny::showNotification(notif_msg, type = "message", duration = 5)

        feedback_msg <- if (warn_symlink) {
          sprintf(
            "<strong>Ajouté :</strong> %s<br/><small class='text-warning'>🟡 Mode Lien symbolique ignoré pour un fichier uploadé (fichier temporaire) — fichier copié dans le projet.</small>",
            htmltools::htmlEscape(fname)
          )
        } else {
          sprintf("<strong>Ajouté :</strong> %s &mdash; <small>%s</small>",
                  htmltools::htmlEscape(fname),
                  htmltools::htmlEscape(dst))
        }
        add_result(list(type = if (warn_symlink) "warning" else "success", msg = feedback_msg))
        pending_file(NULL)

        # Naviguer vers le registre pour que l'utilisateur voie la mise à jour
        bslib::nav_select(ns("inputs_tabs"), selected = "Registre", session = session)

      }, error = function(e) {
        msg_err <- conditionMessage(e)
        message(sprintf("[Inputs] ERROR: %s", msg_err))
        shiny::showNotification(sprintf("Erreur : %s", msg_err), type = "error", duration = 10)
        add_result(list(type = "error", msg = htmltools::htmlEscape(msg_err)))
      })
    }

    shiny::observeEvent(input$btn_add_upload, {
      f <- input$file_upload
      if (is.null(f) || nrow(f) == 0L) {
        shiny::showNotification("Aucun fichier sélectionné.", type = "warning", duration = 5)
        add_result(list(type = "warning", msg = "Veuillez sélectionner un fichier avant de cliquer sur Ajouter."))
        return()
      }
      shinyjs::disable("btn_add_upload")
      on.exit(shinyjs::enable("btn_add_upload"), add = TRUE)
      successes <- character(0)
      failures <- character(0)
      common_type <- input$upload_track_type %||% ""
      shiny::withProgress(
        message = "Ajout des fichiers au registre",
        value = 0,
        {
          for (i in seq_len(nrow(f))) {
            shiny::incProgress(1 / nrow(f), detail = sprintf("%d/%d — %s", i, nrow(f), f$name[[i]]))
            tryCatch({
              app_state$registry <- add_file_to_registry(
                project_config = app_state$project_config,
                source_path = f$datapath[[i]],
                mode = "copy",
                track_type = if (nzchar(common_type)) common_type else NULL,
                notes = input$upload_notes %||% "",
                original_name = f$name[[i]]
              )
              successes <- c(successes, f$name[[i]])
            }, error = function(e) {
              failures <<- c(failures, sprintf("%s : %s", f$name[[i]], conditionMessage(e)))
            })
          }
        }
      )
      if (length(successes) > 0L) {
        pending_file(NULL)
        shiny::showNotification(
          sprintf("%d fichier(s) ajouté(s) au registre.", length(successes)),
          type = "message", duration = 6
        )
        add_result(list(
          type = if (length(failures) > 0L) "warning" else "success",
          msg = sprintf("<strong>%d fichier(s) ajouté(s).</strong>%s",
            length(successes),
            if (length(failures) > 0L) sprintf("<br>%d échec(s).", length(failures)) else "")
        ))
        bslib::nav_select(ns("inputs_tabs"), selected = "Registre", session = session)
      }
      if (length(failures) > 0L) {
        message("[Inputs] Batch upload failures: ", paste(failures, collapse = " | "))
        shiny::showNotification(paste(failures, collapse = "\n"), type = "error", duration = 12)
        if (length(successes) == 0L) {
          add_result(list(
            type = "error",
            msg = sprintf("<strong>Aucun fichier ajouté.</strong><br>%s",
                          htmltools::htmlEscape(paste(failures, collapse = " | ")))
          ))
        }
      }
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    shiny::observeEvent(input$btn_add_local, {
      path <- trimws(input$local_path %||% "")
      if (nchar(path) == 0) {
        shiny::showNotification("Chemin vide.", type = "warning", duration = 5)
        add_result(list(type = "warning", msg = "Veuillez saisir un chemin de fichier."))
        return()
      }
      if (!file.exists(path)) {
        shiny::showNotification(sprintf("Fichier introuvable : %s", path), type = "error", duration = 8)
        add_result(list(type = "error", msg = sprintf("Fichier introuvable : <code>%s</code>",
                                                       htmltools::htmlEscape(path))))
        return()
      }
      do_add_file(
        path        = path,
        mode        = input$local_mode,
        track_type  = input$local_track_type,
        notes       = input$local_notes,
        source_type = "local"
      )
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
