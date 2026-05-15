# =============================================================================
# mod_region_settings.R — Region & figure settings module
# =============================================================================

#' Region & Figure Settings UI
#'
#' @param id module namespace ID
#' @export
mod_region_settings_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(

    omics_banner(
      "Régions & Paramètres de figure",
      "Définissez les loci génomiques à visualiser et les options de rendu.",
      small = TRUE
    ),

    shiny::fluidRow(
      shiny::column(6,
        shiny::tags$div(
          class = "rt-card",
          shiny::tags$div(
            class = "rt-card-header",
            shiny::tags$span(class = "rt-card-icon", shiny::icon("map-marker-alt")),
            shiny::tags$h5("Régions génomiques")
          ),
          bslib::navset_tab(
            bslib::nav_panel(
              shiny::tagList(shiny::icon("crosshairs"), " Région unique"),
              shiny::div(class = "mt-2",
                shiny::div(class = "alert alert-info p-2 mb-2",
                  shiny::icon("info-circle"),
                  shiny::HTML(" Format : <code>chr:start-end</code> &nbsp; Ex : <code>chr1:1000000-1250000</code>")
                ),
                shiny::textInput(ns("region_single"), "Région", placeholder = "chr1:1000000-1250000"),
                shiny::div(class = "d-flex gap-2 flex-wrap mb-1",
                  shiny::tags$small(class = "text-muted", "Exemples rapides :"),
                  shiny::actionLink(ns("ex_reg1"), "chr1:1000-20000"),
                  shiny::actionLink(ns("ex_reg2"), "chrX:100000-500000"),
                  shiny::actionLink(ns("ex_reg3"), "chr21:34600000-34700000")
                ),
                shiny::uiOutput(ns("region_validate_ui")),
                shiny::actionButton(ns("btn_add_region"),
                  shiny::tagList(shiny::icon("plus"), " Ajouter la région"),
                  class = "btn btn-primary btn-sm mt-1")
              )
            ),
            bslib::nav_panel(
              shiny::tagList(shiny::icon("file-alt"), " Fichier BED"),
              shiny::div(class = "mt-2",
                shiny::div(class = "alert alert-info p-2 mb-2",
                  shiny::icon("info-circle"),
                  shiny::HTML(" Un fichier BED 3+ colonnes. Chaque ligne génère une figure séparée.")
                ),
                shiny::fileInput(ns("regions_bed"), "Fichier BED de régions (.bed)"),
                shiny::div(class = "d-flex gap-2",
                  shiny::actionButton(ns("btn_load_bed"),
                    shiny::tagList(shiny::icon("upload"), " Charger"),
                    class = "btn btn-primary btn-sm"),
                  shiny::downloadButton(ns("dl_regions_template"),
                    shiny::tagList(shiny::icon("download"), " Template BED"),
                    class = "btn btn-secondary btn-sm")
                )
              )
            )
          ),
          shiny::tags$hr(class = "divider"),
          shiny::tags$div(
            class = "flex-between mb-2",
            shiny::tags$h6(class = "mb-0",
              shiny::tagList(shiny::icon("list"), " Régions sélectionnées")
            ),
            shiny::actionButton(ns("btn_clear_regions"),
              shiny::tagList(shiny::icon("times"), " Effacer"),
              class = "btn btn-danger btn-sm")
          ),
          shiny::uiOutput(ns("regions_list_ui"))
        )
      ),
      shiny::column(6,
        shiny::tags$div(
          class = "rt-card",
          shiny::tags$div(
            class = "rt-card-header",
            shiny::tags$span(class = "rt-card-icon", shiny::icon("image")),
            shiny::tags$h5("Paramètres de figure")
          ),
          shiny::selectInput(ns("output_format"), "Format de sortie",
                             choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg"),
                             selected = "png"),
          shiny::fluidRow(
            shiny::column(6,
              shiny::numericInput(ns("width"), "Largeur (cm)", value = 38, min = 5, max = 200)
            ),
            shiny::column(6,
              shiny::numericInput(ns("dpi"), "Résolution (DPI)", value = 150, min = 72, max = 600)
            )
          ),
          shiny::textInput(ns("title"), "Titre de la figure", placeholder = "ex: Locus GENE1"),
          shiny::fluidRow(
            shiny::column(6,
              shiny::numericInput(ns("fontsize"), "Taille de police", value = 14, min = 6, max = 40)
            ),
            shiny::column(6,
              shiny::numericInput(ns("track_label_fraction"), "Fraction étiquette",
                                  value = 0.1, min = 0.01, max = 0.5, step = 0.01)
            )
          ),
          shiny::selectInput(ns("track_label_h_align"), "Alignement étiquettes",
                             choices = c("Gauche" = "left", "Centre" = "center", "Droite" = "right")),
          shiny::checkboxInput(ns("decreasing_x_axis"), "Axe X décroissant (brin –)", value = FALSE),
          shiny::selectInput(ns("renderer"), "Renderer",
                             choices = c("pyGenomeTracks" = "pyGenomeTracks",
                                         "rGenomeTracks"  = "rGenomeTracks",
                                         "Les deux"       = "both"),
                             selected = "pyGenomeTracks"),
          shiny::div(class = "alert alert-warning p-2",
            shiny::icon("exclamation-triangle"),
            " pyGenomeTracks doit être installé dans l'environnement conda actif."
          ),
          shiny::textInput(ns("output_basename"), "Préfixe de sortie", value = "figure"),
          shiny::actionButton(ns("btn_save_settings"),
            shiny::tagList(shiny::icon("save"), " Enregistrer les paramètres"),
            class = "btn btn-primary w-100")
        )
      )
    )
  )
}

#' Region & Figure Settings server
#'
#' @param id module namespace ID
#' @param app_state reactive values
#' @export
mod_region_settings_server <- function(id, app_state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Initialize with existing app state values
    shiny::observe({
      fs <- app_state$figure_settings
      if (!is.null(fs)) {
        shiny::updateSelectInput(session, "output_format", selected = fs$output_format %||% "png")
        shiny::updateNumericInput(session, "width",    value = fs$width    %||% 38)
        shiny::updateNumericInput(session, "dpi",      value = fs$dpi      %||% 150)
        shiny::updateTextInput(session, "title",       value = fs$title    %||% "")
        shiny::updateNumericInput(session, "fontsize", value = fs$fontsize %||% 14)
        shiny::updateSelectInput(session, "renderer",  selected = fs$renderer %||% "pyGenomeTracks")
        shiny::updateTextInput(session, "output_basename", value = fs$output_basename %||% "figure")
      }
    })

    # Exemples rapides
    shiny::observeEvent(input$ex_reg1, {
      shiny::updateTextInput(session, "region_single", value = "chr1:1000-20000")
    })
    shiny::observeEvent(input$ex_reg2, {
      shiny::updateTextInput(session, "region_single", value = "chrX:100000-500000")
    })
    shiny::observeEvent(input$ex_reg3, {
      shiny::updateTextInput(session, "region_single", value = "chr21:34600000-34700000")
    })

    output$region_validate_ui <- shiny::renderUI({
      reg <- trimws(input$region_single %||% "")
      if (nchar(reg) == 0) return(NULL)
      err <- if (!validate_region(reg)) sprintf("Format invalide : '%s'. Attendu : chr:debut-fin", reg) else NULL
      if (is.null(err)) {
        shiny::div(class = "text-success small mt-1",
          shiny::icon("check-circle"), sprintf(" Format valide : %s", reg))
      } else {
        shiny::div(class = "text-danger small mt-1",
          shiny::icon("times-circle"), sprintf(" %s", err))
      }
    })

    shiny::observeEvent(input$btn_add_region, {
      reg <- trimws(input$region_single %||% "")
      if (nchar(reg) == 0) return()
      err <- if (!validate_region(reg)) sprintf("Région invalide : '%s'", reg) else NULL
      if (!is.null(err)) {
        shiny::showNotification(sprintf("Région invalide : %s", reg), type = "warning")
        return()
      }
      existing <- app_state$regions %||% character(0)
      if (reg %in% existing) {
        shiny::showNotification("Cette région est déjà dans la liste.", type = "warning")
        return()
      }
      app_state$regions <- c(existing, reg)
      shiny::updateTextInput(session, "region_single", value = "")
    })

    shiny::observeEvent(input$btn_load_bed, {
      req(input$regions_bed)
      tryCatch({
        regions <- read_regions_bed(input$regions_bed$datapath)
        app_state$regions <- regions
        shiny::showNotification(sprintf("%d région(s) chargée(s).", length(regions)), type = "message")
      }, error = function(e) {
        shiny::showNotification(sprintf("Erreur BED : %s", e$message), type = "error")
      })
    })

    output$dl_regions_template <- shiny::downloadHandler(
      filename = function() "template_regions.bed",
      content  = function(file) {
        tpl <- "templates/input_files/template_regions.bed"
        if (file.exists(tpl)) file.copy(tpl, file)
        else writeLines("chr1\t1000\t5000\tregion_1\nchr1\t6000\t10000\tregion_2", file)
      }
    )

    output$regions_list_ui <- shiny::renderUI({
      regions <- app_state$regions %||% character(0)
      if (length(regions) == 0)
        return(shiny::p(class = "text-muted", shiny::em("Aucune région sélectionnée.")))
      items <- lapply(seq_along(regions), function(i) {
        shiny::tags$li(
          shiny::code(regions[i]),
          shiny::actionLink(ns(paste0("del_region_", i)),
            shiny::icon("times"),
            class = "ms-2 text-danger",
            style = "font-size:0.9em;")
        )
      })
      shiny::tags$ul(class = "list-unstyled", items)
    })

    # Suppression individuelle d'une région
    shiny::observe({
      regions <- app_state$regions %||% character(0)
      for (i in seq_along(regions)) {
        local({
          idx <- i
          shiny::observeEvent(input[[paste0("del_region_", idx)]], {
            regs <- app_state$regions %||% character(0)
            if (idx <= length(regs)) app_state$regions <- regs[-idx]
          }, ignoreNULL = TRUE, once = TRUE)
        })
      }
    })

    shiny::observeEvent(input$btn_clear_regions, {
      app_state$regions <- character(0)
    })

    shiny::observeEvent(input$btn_save_settings, {
      app_state$figure_settings <- list(
        output_format        = input$output_format,
        width                = input$width,
        dpi                  = input$dpi,
        title                = input$title %||% "",
        fontsize             = input$fontsize,
        track_label_fraction = input$track_label_fraction,
        track_label_h_align  = input$track_label_h_align,
        decreasing_x_axis    = isTRUE(input$decreasing_x_axis),
        renderer             = input$renderer,
        output_basename      = input$output_basename %||% "figure"
      )
      shiny::showNotification("Paramètres enregistrés.", type = "message")
    })
  })
}
