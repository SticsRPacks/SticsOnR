# Tests on the directories resolution done by run_system() before running
# the model, and on the warnings display helper.

test_that("run_system stops when a usm directory does not exist", {
  workspace <- local_test_workspace()

  expect_error(
    suppressWarnings(run_system("stics_exe", workspace, usm = "unknown_usm")),
    regexp = "does/do not exist",
    fixed = TRUE
  )
})

test_that("run_system stops when the workspace does not exist", {
  missing_workspace <- file.path(tempdir(), "no_such_workspace")

  expect_error(
    suppressWarnings(run_system("stics_exe", missing_workspace)),
    regexp = "does/do not exist",
    fixed = TRUE
  )
})

test_that("run_system stops on a missing executable before running", {
  workspace <- local_test_workspace()
  dir.create(file.path(workspace, "wheat"))

  expect_error(
    run_system(
      file.path(tempdir(), "no_such_stics_exe"),
      workspace,
      usm = "wheat"
    ),
    regexp = "doesn't exist"
  )
})

test_that("run_system does not change the working directory", {
  workspace <- local_test_workspace()
  work_dir <- getwd()

  try(
    suppressWarnings(run_system("stics_exe", workspace, usm = "unknown_usm")),
    silent = TRUE
  )

  expect_equal(getwd(), work_dir)
})

test_that("stics_display_warnings only warns on a non-empty string", {
  expect_warning(stics_display_warnings("a model warning"), "a model warning")
  expect_no_warning(stics_display_warnings(""))
})
