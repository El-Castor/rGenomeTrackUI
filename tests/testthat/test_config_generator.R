library(testthat)
library(withr)

for (f in c("utils_slug.R", "utils_paths.R", "utils_json.R",
            "project_manager.R", "run_manager.R",
            "file_registry.R", "validators.R",
            "track_schema.R", "ini_generator.R",
            "r_script_generator.R", "shell_script_generator.R",
            "logging.R", "config_generator.R")) {
  source(file.path("..", "..", "R", "core", f))
}

make_registry_and_tracks <- function(proj) {
  # Create a minimal bedgraph file in the project's raw dir
  raw_dir <- file.path(proj$project_path, "inputs", "raw")
  dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
  bg_path <- file.path(raw_dir, "signal.bedgraph")
  writeLines(c("chr1\t1000\t2000\t5.0", "chr1\t2000\t3000\t3.2"), bg_path)

  add_file_to_registry(proj, bg_path, mode = "copy", track_type = "bedgraph")
  reg  <- load_file_registry(proj)

  tracks <- list(
    list(track_id = "t1", track_type = "bedgraph", track_name = "Signal",
         enabled = TRUE, file_id = reg$file_id[1],
         params = list(color = "blue", height = 2)),
    list(track_id = "t2", track_type = "x_axis", track_name = "X axis",
         enabled = TRUE, file_id = NULL, params = list())
  )
  list(proj = proj, registry = reg, tracks = tracks)
}

test_that("prepare_run_files creates all expected output files", {
  tmp <- withr::local_tempdir()
  schema <- load_track_schema(testthat::test_path("..", "..", "config", "track_schema.yaml"))
  cfg <- create_project("ConfigTest", genome_label = "hg38", root_dir = tmp)

  rt  <- make_registry_and_tracks(cfg)
  run <- create_run(rt$proj, "test_run", region = "chr1:1000-5000", renderer = "pyGenomeTracks")

  # Resolve file paths
  tracks <- lapply(rt$tracks, function(t) {
    if (!is.null(t$file_id)) {
      entry <- get_file_by_id(rt$registry, t$file_id)
      if (!is.null(entry)) t$file_path <- entry$stored_path
    }
    t
  })

  figure <- list(output_format = "png", width = 20, dpi = 100,
                 title = "Test", fontsize = 12, renderer = "pyGenomeTracks")

  prepare_run_files(
    project_config  = rt$proj,
    run_path        = run$run_path,
    tracks          = tracks,
    schema          = schema,
    registry        = rt$registry,
    regions         = list("chr1:1000-5000"),
    figure_settings = figure
  )

  expect_true(file.exists(file.path(run$run_path, "config", "run_config.yaml")))
  expect_true(file.exists(file.path(run$run_path, "config", "tracks.ini")))
  expect_true(file.exists(file.path(run$run_path, "scripts", "run_pyGenomeTracks.sh")))
})
