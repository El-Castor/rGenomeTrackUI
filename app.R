#!/usr/bin/env Rscript
# =============================================================================
# app.R — rGenomeTrackUI entry point
# =============================================================================
# Launch (recommended — uses the conda R, not the system R):
#   /path/to/envs/rgenometrackui/bin/Rscript app.R
#
# Or with the helper script:
#   bash scripts/run_app.sh
#
# NOTE: do NOT use plain 'Rscript app.R' or 'conda run -n rgenometrackui Rscript app.R'
# on macOS if /usr/local/bin precedes the conda env in PATH — this picks up
# the system R instead of the conda R, and rGenomeTracks won't be found.
# =============================================================================

# Set working directory to the script's location so all relative paths work
if (!interactive()) {
  script_path <- tryCatch(
    normalizePath(sys.frame(1)$ofile, mustWork = FALSE),
    error = function(e) ""
  )
  if (nchar(script_path) > 0) {
    setwd(dirname(script_path))
  }
}

# Set MPLBACKEND for headless pyGenomeTracks calls
Sys.setenv(MPLBACKEND = "Agg")

# Increase upload limit to 2 GB (BigWig and large files)
options(shiny.maxRequestSize = 2 * 1024^3)

# ---------------------------------------------------------------------------
# Detect R / conda PATH conflict and warn early
# ---------------------------------------------------------------------------
.r_bin  <- normalizePath(file.path(R.home("bin"), "R"), mustWork = FALSE)
.conda_prefix <- Sys.getenv("CONDA_PREFIX", unset = "")
if (nchar(.conda_prefix) > 0) {
  .conda_r <- file.path(.conda_prefix, "bin", "R")
  if (file.exists(.conda_r) && !grepl(.conda_prefix, .r_bin, fixed = TRUE)) {
    message(
      "\n[WARNING] PATH conflict detected:\n",
      "  Running R : ", .r_bin, "\n",
      "  Conda R   : ", .conda_r, "\n",
      "  The system R is taking priority over the conda env R.\n",
      "  Packages installed via 'install.R' in the conda env may not be found.\n",
      "  Recommended launch:\n",
      "    conda run -n rgenometrackui Rscript app.R\n"
    )
  }
}
rm(.r_bin, .conda_prefix)

# =============================================================================
# Load packages
# =============================================================================
suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(shinyjs)
  library(DT)
  library(jsonlite)
  library(yaml)
})

# =============================================================================
# Source all core modules
# =============================================================================
core_files <- list.files("R/core", pattern = "\\.R$", full.names = TRUE)
for (f in core_files) source(f, local = FALSE)

# Source all Shiny modules
module_files <- list.files("R/modules", pattern = "\\.R$", full.names = TRUE)
for (f in module_files) source(f, local = FALSE)

# =============================================================================
# Load configuration
# =============================================================================
app_config   <- tryCatch(read_yaml_safe("config/app_config.yaml"), error = function(e) list())
track_schema <- tryCatch(load_track_schema("config/track_schema.yaml"),
                         error = function(e) { warning(e$message); list(tracks = list()) })

APP_PORT     <- as.integer(app_config$app$port    %||% 3838L)
PROJECTS_ROOT <- app_config$app$projects_root %||% "projects"
ensure_dir(PROJECTS_ROOT)
ensure_dir("logs")

# =============================================================================
# UI
# =============================================================================
ui <- bslib::page_navbar(
  title    = "rGenomeTrackUI",
  id       = "main_nav",
  theme    = bslib::bs_theme(bootswatch = "flatly", version = 5),
  collapsible = TRUE,
  header   = shinyjs::useShinyjs(),

  # ------ Dashboard ---------------------------------------------------------
  bslib::nav_panel(
    "Dashboard",
    value = "dashboard",
    shiny::div(class = "container-fluid mt-3",
      mod_dashboard_ui("dashboard")
    )
  ),

  # ------ Projects ----------------------------------------------------------
  bslib::nav_panel(
    "Projets",
    value = "project",
    shiny::div(class = "container-fluid mt-3",
      mod_project_ui("project")
    )
  ),

  # ------ Inputs ------------------------------------------------------------
  bslib::nav_panel(
    "Inputs",
    value = "inputs",
    shiny::div(class = "container-fluid mt-3",
      mod_inputs_ui("inputs")
    )
  ),

  # ------ Track Builder -----------------------------------------------------
  bslib::nav_panel(
    "Track Builder",
    value = "tracks",
    shiny::div(class = "container-fluid mt-3",
      mod_track_builder_ui("tracks")
    )
  ),

  # ------ Region & Figure ---------------------------------------------------
  bslib::nav_panel(
    "Régions & Figure",
    value = "regions",
    shiny::div(class = "container-fluid mt-3",
      mod_region_settings_ui("regions")
    )
  ),

  # ------ Preview -----------------------------------------------------------
  bslib::nav_panel(
    "Aperçu config",
    value = "preview",
    shiny::div(class = "container-fluid mt-3",
      mod_preview_ui("preview")
    )
  ),

  # ------ Run ---------------------------------------------------------------
  bslib::nav_panel(
    "Run",
    value = "run",
    shiny::div(class = "container-fluid mt-3",
      mod_run_ui("run")
    )
  ),

  # ------ Results -----------------------------------------------------------
  bslib::nav_panel(
    "Résultats",
    value = "results",
    shiny::div(class = "container-fluid mt-3",
      mod_results_ui("results")
    )
  ),

  # ------ History -----------------------------------------------------------
  bslib::nav_panel(
    "Historique",
    value = "history",
    shiny::div(class = "container-fluid mt-3",
      mod_history_ui("history")
    )
  ),

  # ------ Settings ----------------------------------------------------------
  bslib::nav_panel(
    "Paramètres",
    value = "settings",
    shiny::div(class = "container-fluid mt-3",
      mod_settings_ui("settings")
    )
  ),
  bslib::nav_panel(
    title = shiny::tagList(shiny::icon("book"), "Documentation"),
    value = "documentation",
    shiny::div(class = "container-fluid mt-3",
      mod_documentation_ui("docs")
    )
  )
)

# =============================================================================
# Server
# =============================================================================
server <- function(input, output, session) {

  # Shared reactive state across all modules
  app_state <- shiny::reactiveValues(
    project_config  = NULL,
    registry        = NULL,
    tracks          = list(),
    regions         = character(0),
    figure_settings = list(
      output_format        = "png",
      width                = 38,
      dpi                  = 150,
      title                = "",
      fontsize             = 14,
      track_label_fraction = 0.1,
      track_label_h_align  = "left",
      decreasing_x_axis    = FALSE,
      renderer             = "pyGenomeTracks",
      output_basename      = "figure"
    ),
    current_run     = NULL,
    last_run_path   = NULL,
    projects_root   = PROJECTS_ROOT,
    nav_to          = NULL
  )

  # Navigation handler: allow modules to request tab navigation
  shiny::observe({
    dest <- app_state$nav_to
    if (!is.null(dest) && nchar(dest) > 0) {
      bslib::nav_select("main_nav", dest)
      app_state$nav_to <- NULL
    }
  })

  # ------ Module servers ----------------------------------------------------
  mod_dashboard_server("dashboard", app_state)
  mod_project_server("project",     app_state)
  mod_inputs_server("inputs",       app_state, track_schema)
  mod_track_builder_server("tracks",app_state, track_schema)
  mod_region_settings_server("regions", app_state)
  mod_preview_server("preview",     app_state, track_schema)
  mod_run_server("run",             app_state, track_schema)
  mod_results_server("results",     app_state)
  mod_history_server("history",     app_state, track_schema)
  mod_settings_server("settings",   app_state)
  mod_documentation_server("docs")
}

# =============================================================================
# Launch
# =============================================================================
cat(sprintf("\n[rGenomeTrackUI] Starting on http://%s:%d\n",
            app_config$app$host %||% "127.0.0.1", APP_PORT))

shiny::shinyApp(
  ui      = ui,
  server  = server,
  options = list(
    host = app_config$app$host %||% "127.0.0.1",
    port = APP_PORT,
    launch.browser = FALSE
  )
)
