context("Test removing missing usms from successive USMs list")

test_that("remove_missing_usms keeps all successions when no USM is missing", {
  successive <- list(
    c("A1", "A2"),
    c("B1", "B2")
  )

  avail_sit <- c("A1", "A2", "B1", "B2")

  result <- remove_missing_usms(
    workspace = "/fake/path",
    successive = successive,
    avail_sit = avail_sit
  )

  expect_equal(result, successive)
})


test_that("remove_missing_usms removes successions containing missing USMs", {
  successive <- list(
    c("A1", "A2"),
    c("B1", "B2"),
    c("C1", "C2")
  )

  avail_sit <- c("A1", "A2", "C1", "C2")

  result <- suppressWarnings(
    remove_missing_usms(
      workspace = "/fake/path",
      successive = successive,
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
  successive <- list(
    c("A1", "A2"),
    c("B1", "B2")
  )

  avail_sit <- c("A1", "A2")

  expect_warning(
    remove_missing_usms(
      workspace = "/data",
      successive = successive,
      avail_sit = avail_sit
    ),
    regexp = "B1 B2"
  )
})


test_that("remove_missing_usms removes all successions if all USMs are missing", {
  successive <- list(
    c("A1", "A2"),
    c("B1")
  )

  avail_sit <- character(0)

  result <- suppressWarnings(
    remove_missing_usms(
      workspace = "/fake/path",
      successive = successive,
      avail_sit = avail_sit
    )
  )

  expect_equal(result, list())
})


test_that("remove_missing_usms handles empty successive", {
  successive <- list()

  avail_sit <- c("A1", "A2")

  result <- remove_missing_usms(
    workspace = "/fake/path",
    successive = successive,
    avail_sit = avail_sit
  )

  expect_equal(result, list())
})


test_that("remove_missing_usms does not modify input by reference", {
  successive <- list(
    c("A1", "A2"),
    c("B1")
  )

  avail_sit <- c("A1", "A2")

  successive_copy <- successive

  suppressWarnings(
    remove_missing_usms(
      workspace = "/fake/path",
      successive = successive,
      avail_sit = avail_sit
    )
  )

  expect_equal(successive, successive_copy)
})
