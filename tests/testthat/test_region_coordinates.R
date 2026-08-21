library(testthat)

source(file.path("..", "..", "R", "core", "utils_slug.R"))
source(file.path("..", "..", "R", "modules", "mod_region_settings.R"))

test_that("guided region coordinates convert between bp and kb", {
  expect_equal(region_coordinate_to_bp(43601.769, "kb"), 43601769L)
  expect_equal(region_coordinate_from_bp(43601769L, "kb"), 43601.769)
  expect_equal(region_coordinate_to_bp(43601769, "bp"), 43601769L)
})

test_that("bp to kb to bp conversion preserves the genomic coordinate", {
  coordinates <- c(1L, 1000L, 43601769L)
  converted <- vapply(coordinates, function(x) {
    region_coordinate_to_bp(region_coordinate_from_bp(x, "kb"), "kb")
  }, integer(1L))
  expect_equal(converted, coordinates)
})

test_that("region delete controls are stable and region-specific", {
  first <- region_delete_input_id("Bd1:2127-13092")
  replacement <- region_delete_input_id("Bd1:7127-8092")

  expect_identical(first, region_delete_input_id("Bd1:2127-13092"))
  expect_false(identical(first, replacement))
  expect_match(first, "^del_region_[[:xdigit:]]{8}$")
})

test_that("gene margins are explicit and allow an exact gene region", {
  expect_equal(gene_flank_bp("0", 3000L), 0L)
  expect_equal(gene_flank_bp("5000", 3000L), 5000L)
  expect_equal(gene_flank_bp("custom", 3000L), 3000L)
  expect_equal(gene_flank_bp("custom", -100L), 0L)
  expect_equal(format_gene_flank(0L), "aucune marge")
  expect_equal(format_gene_flank(5000L), "5 kb de chaque côté")
})
