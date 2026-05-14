library(testthat)
library(withr)

# =============================================================================
# test_preview_generation.R — Preview script generation tests
# =============================================================================

for (f in c("utils_slug.R", "utils_paths.R", "utils_json.R",
            "track_schema.R", "validators.R", "ini_generator.R",
            "r_script_generator.R", "shell_script_generator.R")) {
  source(file.path("..", "..", "R", "core", f))
}

SCHEMA  <- load_track_schema(file.path("..", "..", "config", "track_schema.yaml"))

make_tracks <- function() {
  list(
    list(track_id = "t1", track_type = "bedgraph", track_name = "Signal",
         enabled = TRUE, file_path = "/data/signal.bedgraph",
         params = list(color = "blue", height = 2)),
    list(track_id = "t4", track_type = "x_axis", track_name = "X axis",
         enabled = TRUE, file_id = NULL, params = list())
  )
}

make_figure <- function() {
  list(output_format = "png", width = 20, dpi = 100,
       title = "Test Preview", fontsize = 12, renderer = "pyGenomeTracks")
}

make_regions <- function() list("chr1:1000-5000")

# ---- get_preview_dir --------------------------------------------------------

test_that("get_preview_dir with valid project_config returns valid dir", {
  tmp <- withr::local_tempdir()
  cfg <- list(project_path = tmp)
  preview_dir <- get_preview_dir(cfg)
  expect_true(dir.exists(preview_dir))
  expect_true(grepl("preview", preview_dir))
  expect_false(grepl("^/preview$", preview_dir))
})

test_that("get_preview_dir with NULL config falls back to tempdir", {
  preview_dir <- get_preview_dir(NULL)
  expect_true(dir.exists(preview_dir))
  expect_true(grepl(tempdir(), preview_dir, fixed = TRUE) ||
              grepl("rGenomeTrackUI_preview", preview_dir))
})

test_that("get_preview_dir never returns /preview", {
  preview_dir <- get_preview_dir(NULL)
  expect_false(identical(preview_dir, "/preview"))
  expect_false(grepl("^/preview/?$", preview_dir))
})

# ---- tracks.ini generation --------------------------------------------------

test_that("generate_tracks_ini returns non-empty string for valid tracks", {
  ini <- generate_tracks_ini(make_tracks(), SCHEMA)
  expect_type(ini, "character")
  expect_true(nchar(ini) > 0)
  expect_true(grepl("\\[", ini))  # At least one section header
})

test_that("generate_tracks_ini raises error for empty tracks (as documented)", {
  expect_error(generate_tracks_ini(list(), SCHEMA), regexp = "No tracks")
})

# ---- R script generation ----------------------------------------------------

test_that("generate_rgenometracks_script writes to given run_path, not /preview", {
  tmp <- withr::local_tempdir()
  script <- generate_rgenometracks_script(
    run_path        = tmp,
    tracks          = make_tracks(),
    schema          = SCHEMA,
    regions         = make_regions(),
    figure_settings = make_figure()
  )
  expect_type(script, "character")
  expect_true(nchar(script) > 0)
  expect_true(file.exists(file.path(tmp, "scripts", "run_rGenomeTracks.R")))
  expect_false(file.exists("/preview/scripts/run_rGenomeTracks.R"))
})

test_that("Preview dir route: script written in project preview subdir", {
  tmp <- withr::local_tempdir()
  cfg <- list(project_path = tmp)
  preview_dir <- get_preview_dir(cfg)
  script <- generate_rgenometracks_script(
    run_path        = preview_dir,
    tracks          = make_tracks(),
    schema          = SCHEMA,
    regions         = make_regions(),
    figure_settings = make_figure()
  )
  expect_true(file.exists(file.path(preview_dir, "scripts", "run_rGenomeTracks.R")))
})

# ---- Shell script generation ------------------------------------------------

test_that("generate_pygenometracks_shell_script writes to given run_path, not /preview", {
  tmp <- withr::local_tempdir()
  script <- generate_pygenometracks_shell_script(
    run_path        = tmp,
    regions         = make_regions(),
    figure_settings = make_figure()
  )
  expect_type(script, "character")
  expect_true(nchar(script) > 0)
  expect_true(file.exists(file.path(tmp, "scripts", "run_pyGenomeTracks.sh")))
  expect_false(file.exists("/preview/scripts/run_pyGenomeTracks.sh"))
})

test_that("Shell script contains pyGenomeTracks command", {
  tmp <- withr::local_tempdir()
  script <- generate_pygenometracks_shell_script(
    run_path        = tmp,
    regions         = make_regions(),
    figure_settings = make_figure()
  )
  expect_match(script, "pyGenomeTracks")
})
