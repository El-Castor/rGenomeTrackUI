library(testthat)
library(withr)

for (f in c("utils_slug.R", "utils_paths.R", "utils_json.R",
            "track_schema.R", "validators.R", "r_script_generator.R")) {
  source(file.path("..", "..", "R", "core", f))
}

SCHEMA <- load_track_schema(file.path("..", "..", "config", "track_schema.yaml"))

make_tracks <- function() {
  list(
    list(track_id = "t1", track_type = "bedgraph", track_name = "Signal",
         enabled = TRUE, file_path = "/data/signal.bedgraph",
         params = list(color = "blue", height = 2)),
    list(track_id = "t2", track_type = "gtf", track_name = "Genes",
         enabled = TRUE, file_path = "/data/genes.gtf",
         params = list(height = 4))
  )
}

make_figure <- function() {
  list(output_format = "png", width = 20, dpi = 150,
       title = "Test Figure", fontsize = 12)
}

make_regions <- function() {
  list("chr1:1000-5000", "chr1:8000-10000")
}

test_that("generate_rgenometracks_script returns character string", {
  tmp <- withr::local_tempdir()
  script <- generate_rgenometracks_script(run_path = tmp,
    tracks = make_tracks(), schema = SCHEMA,
    regions = make_regions(), figure_settings = make_figure())
  expect_type(script, "character")
  expect_true(nchar(script) > 0)
})

test_that("R script contains library() calls", {
  tmp <- withr::local_tempdir()
  script <- generate_rgenometracks_script(run_path = tmp,
    tracks = make_tracks(), schema = SCHEMA,
    regions = make_regions(), figure_settings = make_figure())
  expect_match(script, "library\\(rGenomeTracks\\)")
})

test_that("R script contains region loop", {
  tmp <- withr::local_tempdir()
  script <- generate_rgenometracks_script(run_path = tmp,
    tracks = make_tracks(), schema = SCHEMA,
    regions = make_regions(), figure_settings = make_figure())
  expect_match(script, "for.*region")
})

test_that("write_rgenometracks_script creates a file", {
  tmp_dir <- withr::local_tempdir()
  script_text <- "#!/usr/bin/env Rscript\n# test"
  write_rgenometracks_script(run_path = tmp_dir, script_text = script_text)
  expect_true(file.exists(file.path(tmp_dir, "scripts", "run_rGenomeTracks.R")))
})

test_that("generate_rgenometracks_script never writes to /preview", {
  tmp <- withr::local_tempdir()
  generate_rgenometracks_script(run_path = tmp,
    tracks = make_tracks(), schema = SCHEMA,
    regions = make_regions(), figure_settings = make_figure())
  # File must be in tmp, not in /preview
  expect_false(file.exists("/preview/scripts/run_rGenomeTracks.R"))
  expect_true(file.exists(file.path(tmp, "scripts", "run_rGenomeTracks.R")))
})
