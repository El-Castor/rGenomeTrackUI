library(testthat)

source(file.path("..", "..", "R", "core", "utils_slug.R"))

test_that("slugify produces lowercase underscore-separated strings", {
  expect_equal(slugify("Hello World"),  "hello_world")
  expect_equal(slugify("Mon Projet"),   "mon_projet")
  expect_equal(slugify("foo__bar"),     "foo_bar")
  expect_equal(slugify("  spaces  "),   "spaces")
})

test_that("slugify rejects empty or NA input", {
  expect_error(slugify(""))
  expect_error(slugify(NA_character_))
})

test_that("validate_slug passes valid slugs", {
  expect_true(validate_slug("hello_world"))
  expect_true(validate_slug("abc123"))
})

test_that("validate_slug rejects invalid slugs", {
  expect_false(validate_slug("Hello World"))
  expect_false(validate_slug("foo/bar"))
  expect_false(validate_slug("../evil"))
  expect_false(validate_slug("hello-world"))  # hyphens not allowed in slugs
})

test_that("sanitize_name strips dangerous characters", {
  expect_equal(sanitize_name("foo/bar"), "foo-bar")
  expect_equal(sanitize_name("a..b"),    "a--b")
})

test_that("is_safe_name returns TRUE for simple names", {
  expect_true(is_safe_name("my_file.bed"))
  expect_false(is_safe_name("../evil.bed"))
  expect_false(is_safe_name("/abs/path"))
})
