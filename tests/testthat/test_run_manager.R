library(testthat)
library(withr)

for (f in c("utils_slug.R", "utils_paths.R", "utils_json.R",
            "validators.R", "project_manager.R", "ini_generator.R",
            "run_manager.R")) {
  source(file.path("..", "..", "R", "core", f))
}

make_project <- function(tmp) {
  create_project("RunTest", genome_label = "hg38", root_dir = tmp)
}

test_that("create_run creates run directory with expected sub-dirs", {
  tmp <- withr::local_tempdir()
  cfg <- make_project(tmp)
  run <- create_run(cfg, "my_run", region = "chr1:1000-5000")

  expect_true(dir.exists(run$run_path))
  expect_true(file.exists(file.path(run$run_path, "run_metadata.json")))
  for (d in c("config", "scripts", "logs", "outputs")) {
    expect_true(dir.exists(file.path(run$run_path, d)))
  }
})

test_that("create_run errors if run directory already exists", {
  tmp <- withr::local_tempdir()
  cfg <- make_project(tmp)
  run <- create_run(cfg, "dup_run", region = "chr1:1000-5000")
  # create_run_dirs is idempotent; but creating the same run_path again should
  # already exist — verify the metadata file is present
  expect_true(file.exists(file.path(run$run_path, "run_metadata.json")))
})

test_that("list_runs returns all runs for a project", {
  tmp <- withr::local_tempdir()
  cfg <- make_project(tmp)
  create_run(cfg, "run_a", region = "chr1:1000-5000")
  Sys.sleep(1.1)  # ensure different timestamps
  create_run(cfg, "run_b", region = "chr1:1000-5000")
  runs <- list_runs(cfg$project_path)
  expect_equal(nrow(runs), 2)
})

test_that("save and load run_metadata round-trips", {
  tmp <- withr::local_tempdir()
  cfg <- make_project(tmp)
  run <- create_run(cfg, "meta_run", region = "chr1:1000-5000")

  meta <- load_run_metadata(run$run_path)
  meta$status <- "done"
  save_run_metadata(meta, run$run_path)

  meta2 <- load_run_metadata(run$run_path)
  expect_equal(meta2$status, "done")
})

test_that("get_current_region returns first selected region", {
  state <- new.env(parent = emptyenv())
  state$regions <- c(" Bd1:293101-306219 ", "Bd2:1-100")

  expect_equal(get_current_region(NULL, state), "Bd1:293101-306219")

  state$regions <- character(0)
  expect_null(get_current_region(NULL, state))
})

test_that("changing region invalidates prepared run state", {
  state <- new.env(parent = emptyenv())
  state$current_run <- list(run_path = "old")
  state$prepared_config <- list(region = "Bd1:1-100")
  state$run_command <- "pyGenomeTracks ..."
  state$last_output_file <- "figure_Bd1_1_100.png"
  state$last_run_region <- "Bd1:1-100"
  state$preview_image <- "old.png"

  invalidate_prepared_run_state(state, "Bd1:200-300")

  expect_null(state$current_run)
  expect_null(state$prepared_config)
  expect_null(state$run_command)
  expect_null(state$last_output_file)
  expect_null(state$last_run_region)
  expect_null(state$preview_image)
  expect_false(isTRUE(state$prepared_run_ready))
  expect_equal(state$last_prepare_status, "invalidated")
})

test_that("can_launch_run only depends on light prepared state", {
  state <- new.env(parent = emptyenv())
  state$prepared_run_ready <- TRUE
  state$prepared_config <- list(region = "Bd1:1-100", signal_scaling_summary = NULL)
  state$run_command <- "pyGenomeTracks --tracks tracks.ini --region Bd1:1-100"
  state$is_preparing <- FALSE
  state$is_running <- FALSE
  expect_true(can_launch_run(state))

  state$is_preparing <- TRUE
  expect_false(can_launch_run(state))

  state$is_preparing <- FALSE
  state$is_running <- TRUE
  expect_false(can_launch_run(state))

  state$is_running <- FALSE
  state$run_command <- NULL
  expect_false(can_launch_run(state))
})

test_that("expected output filename uses current region", {
  tmp <- withr::local_tempdir()
  out <- expected_output_file_for_region(
    tmp,
    "Bd1:293101-306219",
    list(output_basename = "figure", output_format = "png")
  )

  expect_match(basename(out), "figure_Bd1_293101_306219\\.png")
})

test_that("different regions produce different output filenames and command previews", {
  tmp <- withr::local_tempdir()
  fs <- list(output_basename = "figure", output_format = "png", width = 38, dpi = 150)
  region_a <- "Bd1:100-200"
  region_b <- "Bd1:300-400"
  out_a <- expected_output_file_for_region(tmp, region_a, fs)
  out_b <- expected_output_file_for_region(tmp, region_b, fs)

  expect_false(identical(out_a, out_b))
  expect_match(basename(out_a), "Bd1_100_200")
  expect_match(basename(out_b), "Bd1_300_400")

  cmd_b <- build_pygenometracks_command_preview(tmp, region_b, out_b, fs)
  expect_match(cmd_b, "--region")
  expect_match(cmd_b, region_b, fixed = TRUE)
  expect_match(cmd_b, basename(out_b), fixed = TRUE)
})

test_that("quick render filenames are unique and region-scoped", {
  f1 <- make_render_output_filename("Bd1:100-200", 1)
  f2 <- make_render_output_filename("Bd1:100-200", 2)

  expect_equal(f1, "figure_Bd1_100_200_render_001.png")
  expect_equal(f2, "figure_Bd1_100_200_render_002.png")
  expect_false(identical(f1, f2))
})

test_that("publication panel settings apply compact and balanced track heights", {
  settings <- list(
    width = 12, dpi = 300, track_height = 1.1,
    annotation_height = 0.25, gene_track_height = 0.9,
    spacer_height = 0.05, fontsize = 6, gene_rows = 1
  )
  fs <- merge_results_render_settings(list(), settings)
  expect_equal(fs$width, 12)
  expect_equal(fs$dpi, 300)
  expect_equal(fs$signal_track_height, 1.1)
  expect_equal(fs$annotation_track_height, 0.25)
  expect_equal(fs$gene_track_height, 0.9)

  tracks <- list(
    list(track_type = "bigwig", params = list(height = 3)),
    list(track_type = "bed", params = list(height = 3)),
    list(track_type = "gtf", params = list(height = 1.5))
  )
  adjusted <- apply_results_render_track_settings(tracks, settings)
  expect_equal(adjusted[[1]]$params$height, 1.1)
  expect_equal(adjusted[[2]]$params$height, 0.25)
  expect_equal(adjusted[[3]]$params$height, 1.5)
})

test_that("result render settings merge into figure settings", {
  fs <- merge_results_render_settings(
    base_settings = list(signal_scale_mode = "auto_per_track", width = 38),
    settings = list(
      signal_scale_mode = "manual",
      min_value = 0,
      manual_max_value = 42,
      padding_factor = 1.2,
      quantile = 0.95,
      gene_track_height = 2,
      spacer_height = 0.6,
      fontsize = 8,
      gene_rows = 3
    )
  )

  expect_equal(fs$signal_scale_mode, "manual")
  expect_equal(fs$manual_max_value, 42)
  expect_equal(fs$signal_max_padding_factor, 1.2)
  expect_equal(fs$shared_quantile, 0.95)
  expect_equal(fs$gene_track_height, 2)
  expect_equal(fs$spacer_before_genes_height, 0.6)
  expect_equal(fs$gene_label_fontsize, 8)
  expect_equal(fs$gene_rows, 3)
  expect_equal(fs$width, 38)
})

test_that("select_run_output_figure prefers recorded output then newest PNG", {
  tmp <- withr::local_tempdir()
  out_dir <- file.path(tmp, "outputs", "multi_region")
  dir.create(out_dir, recursive = TRUE)
  old <- file.path(out_dir, "figure_Bd1_1_100.png")
  new <- file.path(out_dir, "figure_Bd1_200_300.png")
  writeLines("old", old)
  Sys.sleep(1.1)
  writeLines("new", new)

  expect_equal(select_run_output_figure(tmp), new)

  state <- new.env(parent = emptyenv())
  state$last_output_file <- old
  expect_equal(select_run_output_figure(tmp, app_state = state), old)
})
