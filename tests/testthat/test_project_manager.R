library(testthat)
library(withr)

# Load all core utilities in dependency order
for (f in c("utils_slug.R", "utils_paths.R", "utils_json.R", "project_manager.R")) {
  source(file.path("..", "..", "R", "core", f))
}

test_that("validate_project_name accepts valid names", {
  expect_silent(validate_project_name("Mon Projet"))
  expect_silent(validate_project_name("ATAC-RNAseq 2024"))
})

test_that("validate_project_name rejects empty or too-long names", {
  expect_error(validate_project_name(""))
  expect_error(validate_project_name(strrep("a", 129)))  # limit is 128 chars
})

test_that("create_project creates expected directory structure", {
  tmp <- withr::local_tempdir()
  cfg <- create_project("Test Project", genome_label = "hg38", root_dir = tmp)

  expect_equal(cfg$project_name, "Test Project")
  project_dir <- cfg$project_path
  expect_true(dir.exists(project_dir))
  expect_true(file.exists(file.path(project_dir, "project_config.json")))
  for (subdir in c("inputs/raw", "inputs/linked", "runs", "exports")) {
    expect_true(dir.exists(file.path(project_dir, subdir)))
  }
})

test_that("create_project errors if project already exists", {
  tmp <- withr::local_tempdir()
  create_project("Duplicate", genome_label = "hg38", root_dir = tmp)
  expect_error(create_project("Duplicate", genome_label = "hg38", root_dir = tmp))
})

test_that("load_project returns config for existing project", {
  tmp <- withr::local_tempdir()
  created <- create_project("LoadMe", genome_label = "hg38", root_dir = tmp)
  loaded  <- load_project(created$project_path)
  expect_equal(loaded$project_name, "LoadMe")
  expect_equal(loaded$project_slug, created$project_slug)
})

test_that("list_projects returns all project slugs", {
  tmp <- withr::local_tempdir()
  create_project("Alpha", genome_label = "hg38", root_dir = tmp)
  create_project("Beta",  genome_label = "hg38", root_dir = tmp)
  projects <- list_projects(tmp)
  expect_true(any(grepl("alpha", projects$project_slug)))
  expect_true(any(grepl("beta",  projects$project_slug)))
})
