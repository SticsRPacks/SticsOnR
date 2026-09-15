# run_system_cmd() is the single entry point for all the system calls of the
# package. It is tested here with OS commands instead of the Stics executable,
# so that no model installation is needed.

test_that("run_system_cmd returns TRUE on success", {
  echo_cmd <- Sys.which("echo")
  skip_if(echo_cmd == "", "No echo command available on this system")

  status <- run_system_cmd(echo_cmd, com_args = "stics")

  expect_true(as.logical(status))
  # The output is not attached when output = FALSE
  expect_false("output" %in% names(attributes(status)))
  expect_false("message" %in% names(attributes(status)))
})

test_that("run_system_cmd attaches the command output when required", {
  echo_cmd <- Sys.which("echo")
  skip_if(echo_cmd == "", "No echo command available on this system")

  status <- run_system_cmd(echo_cmd, com_args = "stics", output = TRUE)

  expect_true(as.logical(status))
  expect_equal(attr(status, "output"), "stics")
})

test_that("run_system_cmd returns FALSE with a message on a failing command", {
  false_cmd <- Sys.which("false")
  skip_if(false_cmd == "", "No false command available on this system")

  status <- suppressWarnings(run_system_cmd(false_cmd))

  expect_false(as.logical(status))
  expect_true("message" %in% names(attributes(status)))
})

test_that("run_system_cmd fails on a file that is not executable", {
  not_an_exe <- file.path(tempdir(), "not_an_exe.txt")
  writeLines("I am not an executable", not_an_exe)
  on.exit(unlink(not_an_exe), add = TRUE)

  status <- suppressWarnings(run_system_cmd(not_an_exe))

  expect_false(as.logical(status))
})
