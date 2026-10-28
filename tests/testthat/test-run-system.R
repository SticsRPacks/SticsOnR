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

test_that("run_system reports the usm names and a successful run", {
  workspace <- local_test_workspace()
  dir.create(file.path(workspace, "wheat"))
  dir.create(file.path(workspace, "maize"))

  # The model run and the output files check are mocked: no Stics needed
  ran_in <- character(0)
  local_mocked_bindings(
    check_stics_exe = function(...) invisible(TRUE),
    run_system_cmd = function(command, output = FALSE, ...) {
      ran_in <<- c(ran_in, basename(getwd()))
      status <- TRUE
      attr(status, "output") <- "Stics ran"
      status
    }
  )
  local_mocked_bindings(
    get_usm_txt = function(...) list(nom = basename(getwd())),
    check_output_files = function(...) list(error = FALSE, missing = NULL),
    .package = "SticsRFiles"
  )

  out <- run_system("stics_exe", workspace, usm = "all")

  expect_length(out, 2)
  expect_setequal(
    vapply(out, function(x) x$name, character(1)),
    c("wheat", "maize")
  )
  expect_false(out[[1]]$error)
  expect_equal(out[[1]]$message, "Stics ran")
  # The model is run from within each usm directory
  expect_setequal(ran_in, c("wheat", "maize"))
})

test_that("run_system reports a model execution error", {
  workspace <- local_test_workspace()
  dir.create(file.path(workspace, "wheat"))

  local_mocked_bindings(
    check_stics_exe = function(...) invisible(TRUE),
    run_system_cmd = function(command, output = FALSE, ...) {
      status <- FALSE
      attr(status, "message") <- "Model execution error"
      status
    }
  )
  local_mocked_bindings(
    get_usm_txt = function(...) list(nom = "wheat"),
    check_output_files = function(...) list(error = FALSE, missing = NULL),
    .package = "SticsRFiles"
  )

  out <- run_system("stics_exe", workspace, usm = "wheat")

  expect_true(out[[1]]$error)
  expect_equal(out[[1]]$message, "Model execution error")
})

test_that("run_system reports missing output files", {
  workspace <- local_test_workspace()
  dir.create(file.path(workspace, "wheat"))

  local_mocked_bindings(
    check_stics_exe = function(...) invisible(TRUE),
    run_system_cmd = function(command, output = FALSE, ...) {
      status <- TRUE
      attr(status, "output") <- "Stics ran"
      status
    }
  )
  local_mocked_bindings(
    get_usm_txt = function(...) list(nom = "wheat"),
    check_output_files = function(...) {
      list(error = TRUE, missing = "mod_swheat.sti")
    },
    .package = "SticsRFiles"
  )

  out <- expect_output(
    run_system("stics_exe", workspace, usm = "wheat"),
    regexp = "missing output file"
  )

  expect_true(out[[1]]$error)
  expect_match(out[[1]]$message, "mod_swheat.sti", fixed = TRUE)
})

test_that("run_system displays the usm names when verbose", {
  workspace <- local_test_workspace()
  dir.create(file.path(workspace, "wheat"))

  local_mocked_bindings(
    check_stics_exe = function(...) invisible(TRUE),
    run_system_cmd = function(command, output = FALSE, ...) {
      status <- TRUE
      attr(status, "output") <- "Stics ran"
      status
    }
  )
  local_mocked_bindings(
    get_usm_txt = function(...) list(nom = "wheat"),
    check_output_files = function(...) list(error = FALSE, missing = NULL),
    .package = "SticsRFiles"
  )

  expect_output(
    run_system("stics_exe", workspace, usm = "wheat", verbose = TRUE),
    regexp = "wheat"
  )
})

test_that("run_system can skip the executable check", {
  workspace <- local_test_workspace()
  dir.create(file.path(workspace, "wheat"))
  checked <- FALSE

  local_mocked_bindings(
    check_stics_exe = function(...) {
      checked <<- TRUE
      invisible(TRUE)
    },
    run_system_cmd = function(command, output = FALSE, ...) TRUE
  )
  local_mocked_bindings(
    get_usm_txt = function(...) list(nom = "wheat"),
    check_output_files = function(...) list(error = FALSE, missing = NULL),
    .package = "SticsRFiles"
  )

  run_system("stics_exe", workspace, usm = "wheat", check = FALSE)
  expect_false(checked)

  run_system("stics_exe", workspace, usm = "wheat", check = TRUE)
  expect_true(checked)
})
