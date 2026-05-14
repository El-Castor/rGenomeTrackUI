library(testthat)
library(withr)

for (f in c("utils_slug.R", "utils_paths.R", "utils_json.R",
            "validators.R", "file_registry.R",
            "project_manager.R", "run_manager.R",
            "track_schema.R", "templates.R")) {
  source(file.path("..", "..", "R", "core", f))
}

get_template_dir <- function(tmp) file.path(tmp, "templates")

make_tracks <- function() {
  list(
    list(track_id = "t1", track_type = "bedgraph", track_name = "Signal",
         enabled = TRUE, file_id = NA, params = list(color = "blue"))
  )
}

test_that("list_templates returns yaml files", {
  tmp <- withr::local_tempdir()
  tdir <- get_template_dir(tmp)
  dir.create(tdir)
  writeLines("name: test\ntracks: []", file.path(tdir, "test.yaml"))
  tpls <- list_templates(tdir)
  expect_true(length(tpls) >= 1)
})

test_that("load_template reads template fields", {
  tmp <- withr::local_tempdir()
  tdir <- get_template_dir(tmp)
  dir.create(tdir)
  content <- "name: TestTpl\ndescription: A test\ntracks:\n  - track_type: spacer\n    track_name: Spacer\n    params: {}\n"
  writeLines(content, file.path(tdir, "test-tpl.yaml"))
  tpl <- load_template(file.path(tdir, "test-tpl.yaml"))
  expect_equal(tpl$name, "TestTpl")
  expect_equal(length(tpl$tracks), 1)
})

test_that("validate_template passes for valid template", {
  tpl <- list(
    name   = "My template",
    tracks = list(
      list(track_type = "spacer", track_name = "S", params = list())
    )
  )
  expect_true(validate_template(tpl))
})

test_that("validate_template fails when tracks missing", {
  expect_error(validate_template(list(name = "Bad")))
})

test_that("apply_template populates tracks list from template", {
  tmp <- withr::local_tempdir()
  tdir <- get_template_dir(tmp)
  dir.create(tdir)
  content <- "name: Simple\ntracks:\n  - track_type: spacer\n    track_name: Space\n    params: {}\n"
  tpl_path <- file.path(tdir, "simple.yaml")
  writeLines(content, tpl_path)

  tpl <- load_template(tpl_path)
  result <- apply_template(tpl)
  expect_equal(length(result$tracks), 1)
  expect_equal(result$tracks[[1]]$track_type, "spacer")
})

test_that("create_demo_project accepts projects_root argument", {
  tmp <- withr::local_tempdir()

  demo <- create_demo_project(projects_root = tmp)

  expect_true(is.list(demo))
  expect_true("project_config" %in% names(demo))
  expect_true("registry"       %in% names(demo))
  expect_true("demo_tracks"    %in% names(demo))
  expect_true("demo_regions"   %in% names(demo))
  expect_true("demo_figure"    %in% names(demo))

  expect_true(dir.exists(demo$project_config$project_path))
  expect_true(is.data.frame(demo$registry))
  expect_true(length(demo$demo_tracks) > 0)
  expect_equal(demo$demo_figure$renderer, "pyGenomeTracks")
})

test_that("create_demo_project stops on NULL projects_root", {
  expect_error(create_demo_project(projects_root = NULL), regexp = "projects_root")
})

test_that("create_demo_project is idempotent (overwrite = TRUE)", {
  tmp <- withr::local_tempdir()
  demo1 <- create_demo_project(projects_root = tmp)
  demo2 <- create_demo_project(projects_root = tmp, overwrite = TRUE)
  expect_equal(
    demo1$project_config$project_path,
    demo2$project_config$project_path
  )
})
