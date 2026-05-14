library(testthat)

docs_dir <- testthat::test_path("..", "..", "docs")

expected_docs <- c(
  "quickstart.md",
  "input_formats.md",
  "troubleshooting.md",
  "best_practices.md",
  "use_cases.md"
)

for (fname in expected_docs) {
  local({
    f <- fname
    test_that(paste("Doc file exists:", f), {
      path <- file.path(docs_dir, f)
      expect_true(file.exists(path), info = path)
    })

    test_that(paste("Doc file is non-empty:", f), {
      path <- file.path(docs_dir, f)
      skip_if_not(file.exists(path))
      content <- readLines(path, warn = FALSE)
      expect_gt(length(content), 5)
    })
  })
}
