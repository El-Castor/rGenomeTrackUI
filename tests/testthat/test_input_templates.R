library(testthat)

templates_dir <- testthat::test_path("..", "..", "templates", "input_files")

expected_templates <- c(
  "template_features.bed",
  "template_signal.bedgraph",
  "template_genes.gtf",
  "template_peaks.narrowPeak",
  "template_links.bedpe",
  "template_domains.bed",
  "template_regions.bed",
  "template_vlines.tsv",
  "template_hlines.tsv"
)

for (fname in expected_templates) {
  local({
    f <- fname
    test_that(paste("Template exists:", f), {
      path <- file.path(templates_dir, f)
      expect_true(file.exists(path), info = path)
    })

    test_that(paste("Template is non-empty:", f), {
      path <- file.path(templates_dir, f)
      skip_if_not(file.exists(path))
      content <- readLines(path, warn = FALSE)
      expect_gt(length(content), 0)
    })
  })
}

test_that("Templates ZIP exists", {
  zip_path <- file.path(templates_dir, "rGenomeTrackUI_input_templates.zip")
  expect_true(file.exists(zip_path))
})
