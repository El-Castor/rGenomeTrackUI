# =============================================================================
# mod_project.R — Project creation and selection module
# =============================================================================

#' Project module UI
#'
#' @param id module namespace ID
#' @export
mod_project_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Gestion des projets"),
    shiny::fluidRow(
      # --- Create project panel
      shiny::column(5,
        shiny::wellPanel(
          shiny::h4("Créer un nouveau projet"),
          shiny::textInput(ns("project_name"), "Nom du projet *", placeholder = "ex: ATAC-seq hg38 K562"),
          shiny::textInput(ns("genome_label"), "Génome *", placeholder = "ex: hg38, mm10, dm6"),
          shiny::textAreaInput(ns("description"), "Description (optionnel)", rows = 2),
          shiny::actionButton(ns("btn_create"), "Créer le projet", class = "btn btn-primary"),
          shiny::uiOutput(ns("create_feedback"))
        )
      ),
      # --- Existing projects panel
      shiny::column(7,
        shiny::h4("Projets existants"),
        shiny::actionButton(ns("btn_refresh"), "Actualiser", class = "btn btn-sm btn-outline-secondary"),
        shiny::br(), shiny::br(),
        DT::DTOutput(ns("projects_table")),
        shiny::br(),
        shiny::actionButton(ns("btn_open"), "Ouvrir le projet sélectionné",
                            class = "btn btn-success", disabled = NA)
      )
    ),
    shiny::hr(),
    shiny::h4("Projet actif"),
    shiny::uiOutput(ns("active_project_card"))
  )
}

#' Project module server
#'
#' @param id module namespace ID
#' @param app_state reactive values
#' @export
mod_project_server <- function(id, app_state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    projects_data <- shiny::reactiveVal(data.frame())

    refresh_projects <- function() {
      root <- app_state$projects_root %||% "projects"
      df   <- tryCatch(list_projects(root), error = function(e) data.frame())
      projects_data(df)
    }

    shiny::observe({ refresh_projects() })
    shiny::observeEvent(input$btn_refresh, { refresh_projects() })

    output$projects_table <- DT::renderDT({
      df <- projects_data()
      if (nrow(df) == 0) return(data.frame(message = "Aucun projet trouvé."))
      df[, c("project_name", "genome_label", "description", "created_at"), drop = FALSE]
    }, selection = "single", options = list(pageLength = 10, dom = "tip"))

    shiny::observeEvent(input$projects_table_rows_selected, {
      shinyjs::enable("btn_open")
    })

    shiny::observeEvent(input$btn_open, {
      sel <- input$projects_table_rows_selected
      if (is.null(sel) || length(sel) == 0) {
        shiny::showNotification("Sélectionnez un projet dans la liste.", type = "warning")
        return()
      }
      df  <- projects_data()
      row <- df[sel, ]
      tryCatch({
        config              <- load_project(row$project_path)
        app_state$project_config <- config
        app_state$registry  <- load_file_registry(config)
        app_state$tracks    <- list()
        shiny::showNotification(sprintf("Projet '%s' ouvert.", config$project_name), type = "message")
        app_state$nav_to <- "inputs"
      }, error = function(e) {
        shiny::showNotification(sprintf("Erreur : %s", e$message), type = "error")
      })
    })

    shiny::observeEvent(input$btn_create, {
      name   <- trimws(input$project_name)
      genome <- trimws(input$genome_label)
      desc   <- trimws(input$description)
      root   <- app_state$projects_root %||% "projects"

      if (nchar(name) == 0 || nchar(genome) == 0) {
        output$create_feedback <- shiny::renderUI({
          shiny::div(class = "alert alert-warning mt-2", "Nom et génome requis.")
        })
        return()
      }

      tryCatch({
        config <- create_project(name, genome, desc, root_dir = root)
        app_state$project_config <- config
        app_state$registry       <- load_file_registry(config)
        app_state$tracks         <- list()
        output$create_feedback <- shiny::renderUI({
          shiny::div(class = "alert alert-success mt-2",
                     sprintf("Projet '%s' créé.", config$project_name))
        })
        refresh_projects()
        shiny::updateTextInput(session, "project_name", value = "")
        shiny::updateTextInput(session, "genome_label", value = "")
        shiny::updateTextAreaInput(session, "description", value = "")
        app_state$nav_to <- "inputs"
      }, error = function(e) {
        output$create_feedback <- shiny::renderUI({
          shiny::div(class = "alert alert-danger mt-2", sprintf("Erreur : %s", e$message))
        })
      })
    })

    output$active_project_card <- shiny::renderUI({
      proj <- app_state$project_config
      if (is.null(proj)) return(shiny::div(class = "text-muted", "Aucun projet actif."))
      shiny::wellPanel(
        shiny::tags$dl(
          shiny::tags$dt("Nom"), shiny::tags$dd(proj$project_name %||% "?"),
          shiny::tags$dt("Génome"), shiny::tags$dd(proj$genome_label %||% "?"),
          shiny::tags$dt("Créé le"), shiny::tags$dd(proj$created_at %||% "?"),
          shiny::tags$dt("Chemin"), shiny::tags$dd(shiny::code(proj$project_path %||% "?"))
        )
      )
    })
  })
}
