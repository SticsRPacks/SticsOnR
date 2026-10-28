# Tests on the helpers used by stics_wrapper() to chain successive USMs.

test_that("is_successive_usm detects USMs that follow another one", {
  successive <- list(c("usm1", "usm2", "usm3"), c("usmA", "usmB"))

  # First USM of a succession has no predecessor
  expect_false(is_successive_usm(successive, "usm1"))
  expect_false(is_successive_usm(successive, "usmA"))

  # Any other USM of a succession has one
  expect_true(is_successive_usm(successive, "usm2"))
  expect_true(is_successive_usm(successive, "usm3"))
  expect_true(is_successive_usm(successive, "usmB"))

  # USM not involved in any succession
  expect_false(is_successive_usm(successive, "unknown_usm"))
})

test_that("is_previous_usm detects USMs followed by another one", {
  successive <- list(c("usm1", "usm2", "usm3"), c("usmA", "usmB"))

  # Last USM of a succession is not a predecessor
  expect_false(is_previous_usm(successive, "usm3"))
  expect_false(is_previous_usm(successive, "usmB"))

  # Any other USM of a succession is one
  expect_true(is_previous_usm(successive, "usm1"))
  expect_true(is_previous_usm(successive, "usm2"))
  expect_true(is_previous_usm(successive, "usmA"))

  # USM not involved in any succession
  expect_false(is_previous_usm(successive, "unknown_usm"))
})

test_that("successive USMs helpers handle an empty succession list", {
  expect_false(is_successive_usm(NULL, "usm1"))
  expect_false(is_successive_usm(list(), "usm1"))
  expect_false(is_previous_usm(NULL, "usm1"))
  expect_false(is_previous_usm(list(), "usm1"))
})

test_that("a single USM succession has neither predecessor nor successor", {
  successive <- list("usm1")

  expect_false(is_successive_usm(successive, "usm1"))
  expect_false(is_previous_usm(successive, "usm1"))
})
