# =============================================================================
# mod_preview.R — Configuration preview module
# =============================================================================

#' Preview module UI
#'
#' @param id module namespace ID
#' @export
mod_preview_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(

    omics_banner(
      "Aperçu de la configuration",
      "Vérifiez les fichiers générés avant de lancer l'analyse.",
      small = TRUE
    ),

    shiny::fluidRow(
      # Left: validation + summary
      shiny::column(4,
        shiny::tags$div(
          class = "rt-card",
          shiny::tags$div(
            class = "rt-card-header",
            shiny::tags$span(class = "rt-card-icon", shiny::icon("check-double")),
            shiny::tags$h5("Checklist")
          ),
          shiny::uiOutput(ns("checklist_ui"))
        ),
        shiny::tags$div(
          class = "rt-card",
          shiny::tags$div(
            class = "rt-card-header",
            shiny::tags$span(class = "rt-card-icon", shiny::icon("info-circle")),
            shiny::tags$h5("Résumé")
          ),
          shiny::uiOutput(ns("summary_ui"))
        )
      ),
      # Right: config files
      shiny::column(8,
        bslib::navset_tab(
          bslib::nav_panel(
            shiny::tagList(shiny::icon("file-code"), " tracks.ini"),
            shiny::div(
              class = "mt-2",
              shiny::div(
                class = "flex-between mb-2",
                shiny::tags$span(style = "font-size:12px;color:var(--rt-muted);",
                                 "Configuration des tracks générée"),
                shiny::div(
                  class = "d-flex gap-2",
                  shiny::actionButton(ns("copy_ini"),
                    shiny::tagList(shiny::icon("copy"), " Copier"),
                    class = "btn btn-secondary btn-sm",
                    `data-copy-target` = paste0("#", ns("ini_preview"))),
                  shiny::downloadButton(ns("dl_ini"),
                    shiny::tagList(shiny::icon("download"), " .ini"),
                    class = "btn btn-secondary btn-sm")
                )
              ),
              code_box(ns("ini_preview"), lang = "ini")
            )
          ),
          bslib::nav_panel(
            shiny::tagList(shiny::icon("r-project"), " Script R"),
            shiny::div(
              class = "mt-2",
              shiny::div(
                class = "flex-between mb-2",
                shiny::tags$span(style = "font-size:12px;color:var(--rt-muted);",
                                 "Script rGenomeTracks"),
                shiny::downloadButton(ns("dl_r_script"),
                  shiny::tagList(shiny::icon("download"), " .R"),
                  class = "btn btn-secondary btn-sm")
              ),
              code_box(ns("r_script_preview"), lang = "R")
            )
          ),
          bslib::nav_panel(
            shiny::tagList(shiny::icon("terminal"), " Script Shell"),
            shiny::div(
              class = "mt-2",
              shiny::div(
                class = "flex-between mb-2",
                shiny::tags$span(style = "font-size:12px;color:var(--rt-muted);",
                                 "Script pyGenomeTracks"),
                shiny::downloadButton(ns("dl_sh_script"),
                  shiny::tagList(shiny::icon("download"), " .sh"),
                  class = "btn btn-secondary btn-sm")
              ),
              code_box(ns("sh_script_preview"), lang = "bash")
            )
          )
        )
      )
    )
  )
}

#' Preview module server
#'
#' @param id module namespace ID
#' @param app_state reactive values
#' @param schema schema list
#' @export
mod_preview_server <- function(id, app_state, schema) {
  shiny::moduleServer(id, function(input, output, session) {

    # ---- Checklist de validation ----
    output$checklist_ui <- shiny::renderUI({
      proj    <- app_state$project_config
      regions <- app_state$regions %||% character(0)
      fs      <- app_state$figure_settings %||% list()
      tracks  <- app_state$tracks
      enabled <- Filter(function(t) isTRUE(t$enabled), tracks)
      reg     <- app_state$registry

      checks <- list(
        list(
          ok    = !is.null(proj),
          label = "Projet actif",
          detail = if (is.null(proj)) "Créez ou ouvrez un projet" else proj$project_name
        ),
        list(
          ok    = length(regions) > 0,
          label = "Région(s) définie(s)",
          detail = if (length(regions) == 0) "Ajoutez une région dans 'Régions & Figure'"
                   else sprintf("%d région(s)", length(regions))
        ),
        list(
          ok    = length(enabled) > 0,
          label = "Track(s) activée(s)",
          detail = if (length(enabled) == 0) "Ajoutez et activez des tracks dans 'Track Builder'"
                   else sprintf("%d track(s)", length(enabled))
        ),
        list(
          ok    = {
            files_ok <- TRUE
            if (length(enabled) > 0 && !is.null(reg)) {
              for (t in enabled) {
                if (!is.null(t$file_id) && nchar(t$file_id %||% "") > 0) {
                  entry <- tryCatch(get_file_by_id(reg, t$file_id), error = function(e) NULL)
                  sp <- entry$stored_path %||% ""
                  if (nchar(sp) == 0 || !file.exists(sp)) { files_ok <- FALSE; break }
                }
              }
            }
            files_ok
          },
          label = "Fichiers de données accessibles",
          detail = "Vérifiez les chemins dans le registre"
        ),
        list(
          ok    = !is.null(fs$renderer) && nchar(fs$renderer %||% "") > 0,
          label = "Renderer sélectionné",
          detail = fs$renderer %||% "Définissez le renderer dans 'Régions & Figure'"
        )
      )

      items <- lapply(checks, function(chk) {
        st <- if (chk$ok) "ok" else "error"
        checklist_item(
          shiny::tagList(
            shiny::tags$strong(chk$label),
            shiny::tags$div(style = "font-size:11px;color:var(--rt-muted);", chk$detail)
          ),
          status = st
        )
      })

      n_ok <- sum(vapply(checks, function(c) isTRUE(c$ok), logical(1)))
      n    <- length(checks)
      overall_st <- if (n_ok == n) "ok" else if (n_ok >= n - 1) "warning" else "error"

      shiny::tagList(
        shiny::div(
          class = "mb-2",
          status_badge(overall_st, label = sprintf("%d / %d validés", n_ok, n))
        ),
        checklist_ui(items)
      )
    })

    # ---- Résumé ----
    output$summary_ui <- shiny::renderUI({
      proj    <- app_state$project_config
      regions <- app_state$regions %||% character(0)
      fs      <- app_state$figure_settings %||% list()
      tracks  <- app_state$tracks
      enabled <- Filter(function(t) isTRUE(t$enabled), tracks)
      shiny::tags$dl(class = "row",
        shiny::tags$dt(class = "col-5", "Projet"),
        shiny::tags$dd(class = "col-7", if (!is.null(proj)) proj$project_name else shiny::em("—")),
        shiny::tags$dt(class = "col-5", "Région(s)"),
        shiny::tags$dd(class = "col-7",
          if (length(regions) > 0)
            shiny::HTML(paste(vapply(regions, function(r) paste0("<code>", r, "</code>"), character(1)), collapse = "<br>"))
          else shiny::em("—")
        ),
        shiny::tags$dt(class = "col-5", "Tracks actives"),
        shiny::tags$dd(class = "col-7", sprintf("%d / %d", length(enabled), length(tracks))),
        shiny::tags$dt(class = "col-5", "Renderer"),
        shiny::tags$dd(class = "col-7", fs$renderer %||% "—"),
        shiny::tags$dt(class = "col-5", "Format"),
        shiny::tags$dd(class = "col-7", toupper(fs$output_format %||% "—")),
        shiny::tags$dt(class = "col-5", "Dimensions"),
        shiny::tags$dd(class = "col-7", sprintf("%s cm @ %s dpi", fs$width %||% "?", fs$dpi %||% "?"))
      )
    })

    # ---- Résoudre les chemins de fichiers ----
    resolve_tracks <- function() {
      tracks  <- app_state$tracks
      enabled <- Filter(function(t) isTRUE(t$enabled), tracks)
      reg     <- app_state$registry
      lapply(enabled, function(t) {
        if (!is.null(t$file_id) && nchar(t$file_id %||% "") > 0 &&
            !is.null(reg) && nrow(reg) > 0) {
          entry <- tryCatch(get_file_by_id(reg, t$file_id), error = function(e) NULL)
          if (!is.null(entry)) t$file_path <- entry$stored_path %||% t$file_path
        }
        t
      })
    }

    # ---- Previews ----
    output$ini_preview <- shiny::renderText({
      enabled <- resolve_tracks()
      regions <- app_state$regions %||% character(0)
      fs <- app_state$figure_settings %||% list()
      if (length(enabled) == 0) return("# Aucune track activée.")
      tryCatch(generate_tracks_ini(enabled, schema, regions = regions, figure_settings = fs), error = function(e) paste("Erreur:", e$message))
    })

    output$r_script_preview <- shiny::renderText({
      enabled <- resolve_tracks()
      regions <- app_state$regions %||% character(0)
      fs      <- app_state$figure_settings %||% list()
      if (length(enabled) == 0)
        return("# Aucune track activée.")
      if (length(regions) == 0)
        return("# Impossible de générer le script R : aucune région définie.")
      tryCatch({
        preview_dir <- get_preview_dir(app_state$project_config)
        generate_rgenometracks_script(preview_dir, enabled, schema, regions, fs)
      }, error = function(e) paste("Erreur:", e$message))
    })

    output$sh_script_preview <- shiny::renderText({
      regions <- app_state$regions %||% character(0)
      fs      <- app_state$figure_settings %||% list()
      if (length(regions) == 0)
        return("# Impossible de générer le script shell : aucune région définie.")
      renderer <- (app_state$figure_settings %||% list())$renderer %||% ""
      if (!nzchar(renderer))
        return("# Impossible de générer le script shell : aucun renderer configuré.")
      tryCatch({
        preview_dir <- get_preview_dir(app_state$project_config)
        generate_pygenometracks_shell_script(preview_dir, regions, fs)
      }, error = function(e) paste("Erreur:", e$message))
    })

    # ---- Téléchargements ----
    output$dl_ini <- shiny::downloadHandler(
      filename = function() "tracks.ini",
      content  = function(file) {
        enabled <- resolve_tracks()
        regions <- app_state$regions %||% character(0)
        fs <- app_state$figure_settings %||% list()
        txt <- if (length(enabled) == 0) "# Aucune track activée."
               else tryCatch(generate_tracks_ini(enabled, schema, regions = regions, figure_settings = fs), error = function(e) paste("Erreur:", e$message))
        writeLines(txt, file)
      }
    )

    output$dl_r_script <- shiny::downloadHandler(
      filename = function() "rgenometracks_script.R",
      content  = function(file) {
        enabled <- resolve_tracks()
        regions <- app_state$regions %||% character(0)
        fs      <- app_state$figure_settings %||% list()
        txt <- tryCatch({
          preview_dir <- get_preview_dir(app_state$project_config)
          generate_rgenometracks_script(preview_dir, enabled, schema, regions, fs)
        }, error = function(e) paste("# Erreur:", e$message))
        writeLines(txt, file)
      }
    )

    output$dl_sh_script <- shiny::downloadHandler(
      filename = function() "pygenometracks_run.sh",
      content  = function(file) {
        regions <- app_state$regions %||% character(0)
        fs      <- app_state$figure_settings %||% list()
        txt <- tryCatch({
          preview_dir <- get_preview_dir(app_state$project_config)
          generate_pygenometracks_shell_script(preview_dir, regions, fs)
        }, error = function(e) paste("# Erreur:", e$message))
        writeLines(txt, file)
      }
    )
  })
}
