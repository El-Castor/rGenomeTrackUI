library(testthat)
library(withr)

# =============================================================================
# test_demo_project_e2e.R — End-to-end demo project creation tests
# (Runs entirely in tempdir, no side effects on the real projects/ folder)
# =============================================================================

for (f in c("utils_slug.R", "utils_paths.R", "utils_json.R",
            "validators.R", "file_registry.R",
            "project_manager.R", "run_manager.R",
            "track_schema.R", "templates.R")) {
  source(file.path("..", "..", "R", "core", f))
}

# ---- Helpers ----------------------------------------------------------------

example_data_dir <- function()
  file.path("..", "..", "example_data")

example_files_exist <- function() {
  d <- example_data_dir()
  all(file.exists(file.path(d, c(
    "mini_signal.bedgraph", "mini_genes.gtf",
    "mini_peaks.narrowPeak", "mini_links.bedpe", "mini_regions.bed"
  ))))
}

# ---- Structure tests --------------------------------------------------------

test_that("create_demo_project returns required list keys", {
  tmp  <- withr::local_tempdir()
  demo <- create_demo_project(projects_root = tmp)

  expect_true(is.list(demo))
  for (key in c("project_config", "registry", "demo_tracks",
                "demo_regions", "demo_figure")) {
    expect_true(key %in% names(demo),
                info = sprintf("Missing key: %s", key))
  }
})

test_that("create_demo_project project_config has a valid project_path", {
  tmp  <- withr::local_tempdir()
  demo <- create_demo_project(projects_root = tmp)
  expect_true(!is.null(demo$project_config$project_path))
  expect_true(dir.exists(demo$project_config$project_path))
})

test_that("create_demo_project demo_tracks has at least 1 track", {
  tmp  <- withr::local_tempdir()
  demo <- create_demo_project(projects_root = tmp)
  expect_true(length(demo$demo_tracks) >= 1)
})

test_that("create_demo_project demo_regions has at least 1 region", {
  tmp  <- withr::local_tempdir()
  demo <- create_demo_project(projects_root = tmp)
  expect_true(length(demo$demo_regions) >= 1)
})

test_that("create_demo_project demo_figure has renderer = pyGenomeTracks", {
  tmp  <- withr::local_tempdir()
  demo <- create_demo_project(projects_root = tmp)
  expect_equal(demo$demo_figure$renderer, "pyGenomeTracks")
})

test_that("create_demo_project registry is a data.frame", {
  tmp  <- withr::local_tempdir()
  demo <- create_demo_project(projects_root = tmp)
  expect_true(is.data.frame(demo$registry))
})

test_that("create_demo_project stops with NULL projects_root", {
  expect_error(create_demo_project(projects_root = NULL), "projects_root")
})

test_that("create_demo_project is idempotent (overwrite = TRUE)", {
  tmp   <- withr::local_tempdir()
  demo1 <- create_demo_project(projects_root = tmp)
  demo2 <- create_demo_project(projects_root = tmp, overwrite = TRUE)
  expect_equal(demo1$project_config$project_path,
               demo2$project_config$project_path)
})

# ---- Registry files (only when example_data exists) -------------------------

test_that("demo registry files exist on disk when example_data is present", {
  skip_if_not(example_files_exist(),
    "example_data files not found (run from app root context)")
  tmp  <- withr::local_tempdir()
  demo <- create_demo_project(projects_root = tmp)
  if (nrow(demo$registry) > 0) {
    for (i in seq_len(nrow(demo$registry))) {
      sp <- demo$registry$stored_path[i]
      expect_true(file.exists(sp),
                  info = sprintf("stored_path not found: %s", sp))
    }
  }
})

test_that("demo tracks for file-based types have a known file_id", {
  tmp  <- withr::local_tempdir()
  demo <- create_demo_project(projects_root = tmp)
  file_based_types <- c("bedgraph", "gtf", "narrowPeak", "bigwig", "bed")
  for (t in demo$demo_tracks) {
    if (t$track_type %in% file_based_types) {
      expect_false(is.null(t$file_id),
                   info = sprintf("Track '%s' (type=%s) has no file_id",
                                  t$track_name, t$track_type))
    }
  }
})

test_that("demo regions use chr1", {
  tmp  <- withr::local_tempdir()
  demo <- create_demo_project(projects_root = tmp)
  regions_str <- vapply(demo$demo_regions, function(r) r$region, character(1))
  expect_true(any(grepl("^chr1:", regions_str)))
})
