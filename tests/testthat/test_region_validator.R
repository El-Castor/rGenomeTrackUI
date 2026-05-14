library(testthat)

for (f in c("utils_slug.R", "utils_paths.R", "utils_json.R", "validators.R")) {
  source(file.path("..", "..", "R", "core", f))
}

test_that("validate_region accepts valid UCSC regions", {
  expect_true(validate_region("chr1:1000-5000"))
  expect_true(validate_region("chrX:100-200"))
  expect_true(validate_region("chr10:1000000-2000000"))
})

test_that("validate_region rejects malformed input", {
  expect_false(validate_region(""))
  expect_false(validate_region("chr1:1000"))
  expect_false(validate_region("chr1 1000 5000"))
  expect_false(validate_region("chr1:5000-1000"))  # start > end
})

test_that("parse_region extracts chrom, start, end", {
  r <- parse_region("chr1:1000-5000")
  expect_equal(r$chrom, "chr1")
  expect_equal(r$start, 1000)
  expect_equal(r$end,   5000)
})

test_that("parse_region returns NULL for invalid region", {
  expect_null(parse_region("invalid"))
})

test_that("sanitize_region_for_filename replaces colons and hyphens", {
  fn <- sanitize_region_for_filename("chr1:1000-5000")
  expect_false(grepl(":", fn))
  expect_match(fn, "^[a-zA-Z0-9_.-]+$")
})

test_that("read_regions_bed reads a valid BED file", {
  tmp <- tempfile(fileext = ".bed")
  writeLines(c("chr1\t1000\t5000", "chr1\t8000\t9000"), tmp)
  on.exit(unlink(tmp))
  regions <- read_regions_bed(tmp)
  expect_equal(length(regions), 2)
  expect_equal(regions[[1]], "chr1:1000-5000")
})
