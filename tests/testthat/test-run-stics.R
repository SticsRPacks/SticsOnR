# Tests on the arguments dispatching done by run_stics() before calling
# run_system(). run_system() is mocked so that no model run is needed.

test_that("run_stics runs all the sub-directories when no usm is given", {
  workspace <- local_test_workspace()
  captured <- new.env(parent = emptyenv())

  local_mocked_bindings(
    run_system = function(stics_exe, workspace, usm, check, verbose) {
      captured$usm <- usm
      captured$check <- check
      invisible(list())
    }
  )

  invisible(run_stics("stics_exe", workspace))

  expect_equal(captured$usm, "all")
  expect_true(captured$check)
})

test_that("run_stics runs the workspace itself if it holds the usm file", {
  workspace <- local_test_workspace()
  # A workspace containing new_travail.usm is a single usm directory
  file.create(file.path(workspace, "new_travail.usm"))
  captured <- new.env(parent = emptyenv())

  local_mocked_bindings(
    run_system = function(stics_exe, workspace, usm, check, verbose) {
      captured$usm <- usm
      invisible(list())
    }
  )

  invisible(run_stics("stics_exe", workspace))

  expect_null(captured$usm)
})

test_that("run_stics forwards the usm names and the other arguments", {
  workspace <- local_test_workspace()
  captured <- new.env(parent = emptyenv())

  local_mocked_bindings(
    run_system = function(stics_exe, workspace, usm, check, verbose) {
      captured$stics_exe <- stics_exe
      captured$workspace <- workspace
      captured$usm <- usm
      captured$check <- check
      captured$verbose <- verbose
      invisible(list())
    }
  )

  run_stics(
    "stics_exe",
    workspace,
    usm = c("wheat", "maize"),
    check = FALSE,
    verbose = TRUE
  )

  expect_equal(captured$stics_exe, "stics_exe")
  expect_equal(captured$workspace, workspace)
  expect_equal(captured$usm, c("wheat", "maize"))
  expect_false(captured$check)
  expect_true(captured$verbose)
})

test_that("run_stics returns the run_system output invisibly", {
  workspace <- local_test_workspace()
  usms_out <- list(list(name = "wheat", error = FALSE, message = "ok"))

  local_mocked_bindings(
    run_system = function(...) invisible(usms_out)
  )

  expect_invisible(run_stics("stics_exe", workspace))
  expect_equal(run_stics("stics_exe", workspace), usms_out)
})
