library(testthat)
library(withr)

for (f in c("utils_slug.R", "utils_paths.R", "utils_json.R",
            "validators.R", "file_registry.R", "project_manager.R")) {
  source(file.path("..", "..", "R", "core", f))
}

# ---- validate_file_exists ---------------------------------------------------
test_that("validate_file_exists passes for real files", {
  tmp <- tempfile(); writeLines("x", tmp); on.exit(unlink(tmp))
  expect_true(validate_file_exists(tmp))
})

test_that("validate_file_exists fails for missing file", {
  expect_error(validate_file_exists("/nonexistent/path/file.bed"))
})

# ---- validate_bed_light -----------------------------------------------------
test_that("validate_bed_light accepts minimal BED3 content", {
  tmp <- tempfile(fileext = ".bed")
  writeLines(c("chr1\t1000\t5000", "chr2\t0\t100"), tmp)
  on.exit(unlink(tmp))
  expect_true(validate_bed_light(tmp))
})

test_that("validate_bed_light rejects non-numeric coords", {
  tmp <- tempfile(fileext = ".bed")
  writeLines(c("chr1\tstart\tend"), tmp)
  on.exit(unlink(tmp))
  expect_error(validate_bed_light(tmp))
})

# ---- detect_file_type -------------------------------------------------------
test_that("detect_file_type maps extensions correctly", {
  expect_equal(detect_file_type("sample.bedgraph"), "bedgraph")
  expect_equal(detect_file_type("sample.gtf"),      "gtf")
  expect_equal(detect_file_type("sample.narrowPeak"), "narrowPeak")
  expect_equal(detect_file_type("sample.bw"),       "bigwig")
  expect_equal(detect_file_type("sample.xyz"),      "unknown")
})

# ---- file registry ----------------------------------------------------------
test_that("add_file_to_registry creates a registry entry", {
  tmp_file <- tempfile(fileext = ".bedgraph")
  writeLines(c("chr1\t1000\t2000\t5.0"), tmp_file)
  on.exit(unlink(tmp_file))

  # create_project needs withr for temp dir
  tmp_proj_dir <- withr::local_tempdir()
  proj <- create_project("RegTest", genome_label = "hg38", root_dir = tmp_proj_dir)
  add_file_to_registry(proj, tmp_file, mode = "copy", track_type = "bedgraph")
  reg  <- load_file_registry(proj)
  expect_equal(nrow(reg), 1)
  expect_equal(reg$file_type_detected[1], "bedgraph")
})
