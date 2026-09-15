# Tests on check_stics_exe(), for the cases that can be checked without a
# real Stics executable.

test_that("check_stics_exe stops on a missing executable", {
  missing_exe <- file.path(tempdir(), "no_such_stics_exe")

  expect_error(
    check_stics_exe(missing_exe),
    regexp = "doesn't exist"
  )
})

test_that("check_stics_exe returns FALSE on a missing executable", {
  missing_exe <- file.path(tempdir(), "no_such_stics_exe")

  expect_false(check_stics_exe(missing_exe, stop_on_error = FALSE))
})

test_that("check_stics_exe informs the user when verbose", {
  missing_exe <- file.path(tempdir(), "no_such_stics_exe")

  expect_message(
    check_stics_exe(missing_exe, stop_on_error = FALSE, verbose = TRUE),
    regexp = "does not exist"
  )
})

test_that("check_stics_exe does not change the working directory", {
  missing_exe <- file.path(tempdir(), "no_such_stics_exe")
  work_dir <- getwd()

  check_stics_exe(missing_exe, stop_on_error = FALSE)
  expect_equal(getwd(), work_dir)

  try(check_stics_exe(missing_exe), silent = TRUE)
  expect_equal(getwd(), work_dir)
})

test_that("check_stics_exe fails on a file that is not an executable", {
  not_an_exe <- file.path(tempdir(), "not_a_stics_exe.txt")
  writeLines("I am not an executable", not_an_exe)
  on.exit(unlink(not_an_exe), add = TRUE)

  expect_error(
    suppressWarnings(check_stics_exe(not_an_exe)),
    regexp = "not executable"
  )
  expect_false(
    suppressWarnings(check_stics_exe(not_an_exe, stop_on_error = FALSE))
  )
})
