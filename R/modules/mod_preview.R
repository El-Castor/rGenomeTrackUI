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
    shiny::h3("Aperçu de la configuration"),
    shiny::fluidRow(
      shiny::column(4,
        bslib::card(
          bslib::card_header(shiny::icon("check-double"), " Checklist de validation"),
          shiny::uiOutput(ns("checklist_ui"))
        ),
        bslib::card(
          bslib::card_header(shiny::icon("info-circle"), " Résumé"),
          shiny::uiOutput(ns("summary_ui"))
        )
      ),
      shiny::column(8,
        bslib::navset_tab(
          bslib::nav_panel("tracks.ini",
            shiny::div(class = "d-flex justify-content-end gap-2 mt-2 mb-1",
              shiny::downloadButton(ns("dl_ini"), "Télécharger .ini",
                class = "btn btn-outline-secondary btn-sm"),
              shiny::actionButton(ns("copy_ini"), shiny::icon("copy"), " Copier",
                class = "btn btn-outline-secondary btn-sm",
                onclick = sprintf(
                  "navigator.clipboard.writeText(document.getElementById('%s').textContent)",
                  ns("ini_preview")
                ))
            ),
            shiny::verbatimTextOutput(ns("ini_preview"))
          ),
          bslib::nav_panel("Script R",
            shiny::div(class = "d-flex justify-content-end gap-2 mt-2 mb-1",
              shiny::downloadButton(ns("dl_r_script"), "Télécharger .R",
                class = "btn btn-outline-secondary btn-sm")
            ),
            shiny::verbatimTextOutput(ns("r_script_preview"))
          ),
          bslib::nav_panel("Script Shell",
            shiny::div(class = "d-flex justify-content-end gap-2 mt-2 mb-1",
              shiny::downloadButton(ns("dl_sh_script"), "Télécharger .sh",
                class = "btn btn-outline-secondary btn-sm")
            ),
            shiny::verbatimTextOutput(ns("sh_script_preview"))
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
        icon_name  <- if (chk$ok) "check-circle" else "times-circle"
        icon_class <- if (chk$ok) "text-success" else "text-danger"
        shiny::div(class = "d-flex align-items-start gap-2 mb-2",
          shiny::span(shiny::icon(icon_name), class = icon_class),
          shiny::div(
            shiny::strong(chk$label),
            shiny::div(class = "text-muted small", chk$detail)
          )
        )
      })

      n_ok <- sum(vapply(checks, function(c) isTRUE(c$ok), logical(1)))
      n    <- length(checks)
      overall_class <- if (n_ok == n) "success" else if (n_ok >= n - 1) "warning" else "danger"
      overall_icon  <- if (n_ok == n) "check-circle" else "exclamation-triangle"

      shiny::tagList(
        shiny::div(class = paste0("alert alert-", overall_class, " p-2 mb-2"),
          shiny::icon(overall_icon),
          sprintf(" %d/%d éléments validés", n_ok, n)
        ),
        items
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
      if (length(enabled) == 0) return("# Aucune track activée.")
      tryCatch(generate_tracks_ini(enabled, schema), error = function(e) paste("Erreur:", e$message))
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
        txt <- if (length(enabled) == 0) "# Aucune track activée."
               else tryCatch(generate_tracks_ini(enabled, schema), error = function(e) paste("Erreur:", e$message))
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
