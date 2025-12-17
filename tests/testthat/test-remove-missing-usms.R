context("Test removing missing usms from successive USMs list")
library(SticsOnR)

test_that("remove_missing_usms keeps all successions when no USM is missing", {

  successive_usms <- list(
    c("A1", "A2"),
    c("B1", "B2")
  )

  avail_sit <- c("A1", "A2", "B1", "B2")

  result <- remove_missing_usms(
    data_dir = "/fake/path",
    successive_usms = successive_usms,
    avail_sit = avail_sit
  )

  expect_equal(result, successive_usms)
})


test_that("remove_missing_usms removes successions containing missing USMs", {

  successive_usms <- list(
    c("A1", "A2"),
    c("B1", "B2"),
    c("C1", "C2")
  )

  avail_sit <- c("A1", "A2", "C1", "C2")

  result <- suppressWarnings(
    remove_missing_usms(
      data_dir = "/fake/path",
      successive_usms = successive_usms,
      avail_sit = avail_sit
    )
  )

  expect_equal(
    result,
    list(
      c("A1", "A2"),
      c("C1", "C2")
    )
  )
})


test_that("remove_missing_usms emits a warning listing missing USMs", {

  successive_usms <- list(
    c("A1", "A2"),
    c("B1", "B2")
  )

  avail_sit <- c("A1", "A2")

  expect_warning(
    remove_missing_usms(
      data_dir = "/data",
      successive_usms = successive_usms,
      avail_sit = avail_sit
    ),
    regexp = "B1 B2"
  )
})


test_that("remove_missing_usms removes all successions if all USMs are missing", {

  successive_usms <- list(
    c("A1", "A2"),
    c("B1")
  )

  avail_sit <- character(0)

  result <- suppressWarnings(
    remove_missing_usms(
      data_dir = "/fake/path",
      successive_usms = successive_usms,
      avail_sit = avail_sit
    )
  )

  expect_equal(result, list())
})


test_that("remove_missing_usms handles empty successive_usms", {

  successive_usms <- list()

  avail_sit <- c("A1", "A2")

  result <- remove_missing_usms(
    data_dir = "/fake/path",
    successive_usms = successive_usms,
    avail_sit = avail_sit
  )

  expect_equal(result, list())
})


test_that("remove_missing_usms does not modify input by reference", {

  successive_usms <- list(
    c("A1", "A2"),
    c("B1")
  )

  avail_sit <- c("A1", "A2")

  successive_usms_copy <- successive_usms

  suppressWarnings(
    remove_missing_usms(
      data_dir = "/fake/path",
      successive_usms = successive_usms,
      avail_sit = avail_sit
    )
  )

  expect_equal(successive_usms, successive_usms_copy)
})
