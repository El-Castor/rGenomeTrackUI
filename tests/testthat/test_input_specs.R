library(testthat)

# Source only the module under test — %||% is defined inside input_specs.R
source(file.path("..", "..", "R", "core", "input_specs.R"))

# Paths resolved relative to the test file (works in both test_dir and test_file contexts)
config_dir    <- testthat::test_path("..", "..", "config")
templates_dir <- testthat::test_path("..", "..", "templates", "input_files")
examples_dir  <- testthat::test_path("..", "..", "examples", "input_files")

# ---- load_input_specs --------------------------------------------------------

test_that("load_input_specs returns a non-empty list", {
  reset_input_specs_cache()
  specs <- load_input_specs(config_dir)
  expect_type(specs, "list")
  expect_gt(length(specs), 0)
})

test_that("load_input_specs caches results", {
  reset_input_specs_cache()
  specs1 <- load_input_specs(config_dir)
  specs2 <- load_input_specs(config_dir)
  expect_identical(specs1, specs2)
})

# ---- get_supported_input_formats ---------------------------------------------

test_that("get_supported_input_formats returns character vector", {
  specs <- load_input_specs(config_dir)
  fmts <- get_supported_input_formats(specs)
  expect_type(fmts, "character")
  expect_true("bed" %in% fmts)
  expect_true("bigwig" %in% fmts)
})

# ---- get_format_spec ---------------------------------------------------------

test_that("get_format_spec returns spec for known format", {
  specs <- load_input_specs(config_dir)
  spec <- get_format_spec("bed", specs)
  expect_type(spec, "list")
  expect_true(!is.null(spec$extensions))
})

test_that("get_format_spec returns NULL for unknown format", {
  specs <- load_input_specs(config_dir)
  expect_null(get_format_spec("unknownformat_xyz", specs))
})

# ---- get_formats_for_track_type ---------------------------------------------

test_that("get_formats_for_track_type returns formats for bigwig", {
  specs <- load_input_specs(config_dir)
  fmts <- get_formats_for_track_type("bigwig", specs)
  expect_true("bigwig" %in% fmts)
})

test_that("get_formats_for_track_type returns character(0) for unknown type", {
  specs <- load_input_specs(config_dir)
  fmts <- get_formats_for_track_type("nonexistent_track_type", specs)
  expect_equal(length(fmts), 0)
})

# ---- render_format_help ------------------------------------------------------

test_that("render_format_help returns non-empty character", {
  specs <- load_input_specs(config_dir)
  html <- render_format_help("bed", specs)
  expect_type(html, "character")
  expect_gt(nchar(html), 10)
})

# ---- format_specs_as_dataframe -----------------------------------------------

test_that("format_specs_as_dataframe returns data.frame with expected columns", {
  specs <- load_input_specs(config_dir)
  df <- format_specs_as_dataframe(specs)
  expect_s3_class(df, "data.frame")
  expect_true("Format" %in% names(df))
  expect_true("Extensions" %in% names(df))
})

# ---- validate_against_format_spec --------------------------------------------

test_that("validate_against_format_spec returns list with status/messages/preview", {
  specs  <- load_input_specs(config_dir)
  bed_ex <- file.path(examples_dir, "example_features.bed")
  skip_if_not(file.exists(bed_ex), "Example BED file not found")
  result <- validate_against_format_spec(bed_ex, "bed", specs)
  expect_type(result, "list")
  expect_true("status" %in% names(result))
  expect_true("messages" %in% names(result))
  expect_true("preview" %in% names(result))
})

test_that("validate_against_format_spec handles missing file", {
  specs  <- load_input_specs(config_dir)
  result <- validate_against_format_spec("/nonexistent/path/file.bed", "bed", specs)
  expect_equal(result$status, "error")
})

test_that("validate_against_format_spec returns error for empty file", {
  specs    <- load_input_specs(config_dir)
  tmp_file <- tempfile(fileext = ".bed")
  writeLines(character(0), tmp_file)
  on.exit(unlink(tmp_file))
  result <- validate_against_format_spec(tmp_file, "bed", specs)
  expect_equal(result$status, "error")
})

test_that("validate_against_format_spec returns error for insufficient columns", {
  specs    <- load_input_specs(config_dir)
  tmp_file <- tempfile(fileext = ".bedgraph")
  # BedGraph requires 4 columns; give only 2
  writeLines(c("chr1\t1000"), tmp_file)
  on.exit(unlink(tmp_file))
  result <- validate_against_format_spec(tmp_file, "bedgraph", specs)
  expect_equal(result$status, "error")
})

test_that("validate_against_format_spec accepts valid BedGraph", {
  specs <- load_input_specs(config_dir)
  ex    <- file.path(examples_dir, "example_signal.bedgraph")
  skip_if_not(file.exists(ex), "example_signal.bedgraph not found")
  result <- validate_against_format_spec(ex, "bedgraph", specs)
  expect_equal(result$status, "ok")
})

test_that("validate_against_format_spec accepts valid GTF", {
  specs <- load_input_specs(config_dir)
  ex    <- file.path(examples_dir, "example_genes.gtf")
  skip_if_not(file.exists(ex), "example_genes.gtf not found")
  result <- validate_against_format_spec(ex, "gtf", specs)
  expect_equal(result$status, "ok")
})

test_that("validate_against_format_spec accepts valid narrowPeak", {
  specs <- load_input_specs(config_dir)
  ex    <- file.path(examples_dir, "example_peaks.narrowPeak")
  skip_if_not(file.exists(ex), "example_peaks.narrowPeak not found")
  result <- validate_against_format_spec(ex, "narrowpeak", specs)
  expect_equal(result$status, "ok")
})

test_that("validate_against_format_spec accepts valid BEDPE", {
  specs <- load_input_specs(config_dir)
  ex    <- file.path(examples_dir, "example_links.bedpe")
  skip_if_not(file.exists(ex), "example_links.bedpe not found")
  result <- validate_against_format_spec(ex, "bedpe", specs)
  expect_equal(result$status, "ok")
})

# ---- get_template_path / get_example_path ------------------------------------

test_that("get_template_path returns path for bed", {
  result <- get_template_path("bed", templates_dir)
  expect_type(result, "character")
  expect_true(file.exists(result))
})

test_that("get_template_path returns NULL for bigwig (no template)", {
  result <- get_template_path("bigwig", templates_dir)
  expect_null(result)
})

test_that("get_example_path returns path for bedgraph", {
  result <- get_example_path("bedgraph", examples_dir)
  expect_type(result, "character")
  expect_true(file.exists(result))
})

# ---- formats completeness ---------------------------------------------------

test_that("all required formats are present in specs", {
  specs <- load_input_specs(config_dir)
  fmts  <- get_supported_input_formats(specs)
  for (required in c("bed", "bedgraph", "bigwig", "gtf", "narrowpeak", "bedpe",
                     "domains", "regions", "vlines", "hlines")) {
    expect_true(required %in% fmts, info = paste("Missing format:", required))
  }
})

test_that("each format has required fields", {
  specs <- load_input_specs(config_dir)
  for (fmt in names(specs)) {
    s <- specs[[fmt]]
    expect_true(!is.null(s$label),       info = paste(fmt, "missing label"))
    expect_true(!is.null(s$extensions),  info = paste(fmt, "missing extensions"))
    expect_true(!is.null(s$description), info = paste(fmt, "missing description"))
  }
})
