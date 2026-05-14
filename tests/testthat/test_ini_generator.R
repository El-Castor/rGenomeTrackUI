library(testthat)

for (f in c("utils_slug.R", "utils_paths.R", "utils_json.R",
            "track_schema.R", "ini_generator.R")) {
  source(file.path("..", "..", "R", "core", f))
}

SCHEMA <- load_track_schema(testthat::test_path("..", "..", "config", "track_schema.yaml"))

make_tracks <- function() {
  list(
    list(track_id = "t1", track_type = "bedgraph", track_name = "Signal",
         enabled = TRUE, file_path = "/data/signal.bedgraph",
         params = list(color = "blue", height = 2)),
    list(track_id = "t2", track_type = "gtf", track_name = "Genes",
         enabled = TRUE, file_path = "/data/genes.gtf",
         params = list(height = 4)),
    list(track_id = "t3", track_type = "x_axis", track_name = "X axis",
         enabled = TRUE, file_path = NULL,
         params = list())
  )
}

test_that("generate_tracks_ini returns a non-empty character string", {
  ini <- generate_tracks_ini(make_tracks(), SCHEMA)
  expect_type(ini, "character")
  expect_true(nchar(ini) > 0)
})

test_that("INI output contains expected section headers", {
  ini <- generate_tracks_ini(make_tracks(), SCHEMA)
  expect_match(ini, "\\[Signal\\]")
  expect_match(ini, "\\[Genes\\]")
  expect_match(ini, "\\[X axis\\]")
})

test_that("INI output contains file = for file-backed tracks", {
  ini <- generate_tracks_ini(make_tracks(), SCHEMA)
  expect_match(ini, "file = /data/signal.bedgraph")
  expect_match(ini, "file = /data/genes.gtf")
})

test_that("INI output contains file_type for bedgraph", {
  ini <- generate_tracks_ini(make_tracks(), SCHEMA)
  expect_match(ini, "file_type = bedgraph")
})

test_that("disabled tracks are excluded from INI", {
  tracks <- make_tracks()
  tracks[[2]]$enabled <- FALSE
  ini <- generate_tracks_ini(tracks, SCHEMA)
  expect_false(grepl("\\[Genes\\]", ini))
})

test_that("write_tracks_ini writes to disk", {
  tmp <- tempfile(fileext = ".ini")
  on.exit(unlink(tmp))
  write_tracks_ini(make_tracks(), SCHEMA, tmp)
  expect_true(file.exists(tmp))
  content <- readLines(tmp)
  expect_true(any(grepl("\\[Signal\\]", content)))
})
