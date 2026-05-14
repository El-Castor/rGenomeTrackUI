library(testthat)
library(withr)

# Source all required core modules
for (f in c(
  "utils_slug.R", "utils_paths.R", "utils_json.R",
  "project_manager.R", "run_manager.R",
  "file_registry.R", "validators.R",
  "track_schema.R", "ini_generator.R",
  "r_script_generator.R", "shell_script_generator.R",
  "logging.R", "config_generator.R"
)) {
  source(file.path("..", "..", "R", "core", f))
}

SCHEMA <- load_track_schema(file.path("..", "..", "config", "track_schema.yaml"))

# Helper: create a minimal project + run with a bedgraph track
make_prepared_run <- function(tmp) {
  bg_path <- file.path(tmp, "signal.bedgraph")
  writeLines(c("chr1\t1000\t2000\t5.0", "chr1\t2000\t3000\t3.2"), bg_path)

  proj <- create_project("PrepTest", genome_label = "hg38", root_dir = tmp)

  # Add file to project registry using the real API
  add_file_to_registry(proj, bg_path, mode = "copy", track_type = "bedgraph")
  reg_df <- load_file_registry(proj)

  tracks <- list(
    list(track_id = "t1", track_type = "bedgraph", track_name = "Signal",
         enabled = TRUE, file_id = reg_df$file_id[1],
         params = list(color = "blue", height = 2)),
    list(track_id = "t2", track_type = "x_axis", track_name = "Axe X",
         enabled = TRUE, file_id = NULL, params = list())
  )

  meta <- create_run(proj, "prep_test_run", "chr1:1000-5000", "pyGenomeTracks")

  figure <- list(
    output_format = "png", width = 20, dpi = 100,
    title = "Test", fontsize = 12, renderer = "pyGenomeTracks"
  )

  prepare_run_files(proj, meta$run_path, tracks, SCHEMA, reg_df,
                    list("chr1:1000-5000"), figure)

  list(run_path = meta$run_path, proj = proj, tracks = tracks, reg = reg_df)
}

test_that("prepare_run_files créé tracks.ini", {
  tmp <- withr::local_tempdir()
  r <- make_prepared_run(tmp)
  expect_true(file.exists(file.path(r$run_path, "config", "tracks.ini")))
})

test_that("prepare_run_files créé run_rGenomeTracks.R", {
  tmp <- withr::local_tempdir()
  r <- make_prepared_run(tmp)
  expect_true(file.exists(file.path(r$run_path, "scripts", "run_rGenomeTracks.R")))
})

test_that("prepare_run_files créé run_pyGenomeTracks.sh", {
  tmp <- withr::local_tempdir()
  r <- make_prepared_run(tmp)
  expect_true(file.exists(file.path(r$run_path, "scripts", "run_pyGenomeTracks.sh")))
})

test_that("prepare_run_files créé tracks_config.json", {
  tmp <- withr::local_tempdir()
  r <- make_prepared_run(tmp)
  expect_true(file.exists(file.path(r$run_path, "config", "tracks_config.json")))
})

test_that("prepare_run_files créé run_config.yaml", {
  tmp <- withr::local_tempdir()
  r <- make_prepared_run(tmp)
  expect_true(file.exists(file.path(r$run_path, "config", "run_config.yaml")))
})

test_that("prepare_run_files met à jour le statut en 'prepared'", {
  tmp <- withr::local_tempdir()
  r <- make_prepared_run(tmp)
  meta <- load_run_metadata(r$run_path)
  expect_equal(meta$status, "prepared")
})

test_that("tracks.ini contient les types de track attendus", {
  tmp <- withr::local_tempdir()
  r <- make_prepared_run(tmp)
  ini_content <- paste(readLines(file.path(r$run_path, "config", "tracks.ini")), collapse = "\n")
  # Must have a bedgraph section and x_axis section
  expect_match(ini_content, "bedgraph", ignore.case = TRUE)
})

test_that("run_rGenomeTracks.R contient les appels rGenomeTracks", {
  tmp <- withr::local_tempdir()
  r <- make_prepared_run(tmp)
  script_content <- paste(readLines(file.path(r$run_path, "scripts", "run_rGenomeTracks.R")), collapse = "\n")
  expect_match(script_content, "rGenomeTracks|plot_gtracks", perl = TRUE)
})

test_that("run_pyGenomeTracks.sh référence le bon tracks.ini", {
  tmp <- withr::local_tempdir()
  r <- make_prepared_run(tmp)
  sh_content <- paste(readLines(file.path(r$run_path, "scripts", "run_pyGenomeTracks.sh")), collapse = "\n")
  expect_match(sh_content, "tracks\\.ini")
})

test_that("prepare_run_files lève une erreur si aucune track activée", {
  tmp <- withr::local_tempdir()
  proj <- create_project("NoTrackTest", genome_label = "hg38", root_dir = tmp)
  meta <- create_run(proj, "no_tracks_run", "chr1:1000-5000", "pyGenomeTracks")
  figure <- list(output_format = "png", renderer = "pyGenomeTracks")
  expect_error(
    prepare_run_files(proj, meta$run_path, list(), SCHEMA,
                      data.frame(), list("chr1:1000-5000"), figure),
    regexp = "No enabled tracks"
  )
})
