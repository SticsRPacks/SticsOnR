# Tests on run_javastics(). JavaStics, Java and the Stics executable are not
# needed here: the SticsRFiles checks are mocked (see helper-javastics.R) and
# the JavaStics command line is replaced by a fake script echoing its
# arguments.

test_that("run_javastics returns nothing on an invalid workspace", {
  javastics <- local_test_workspace()
  workspace <- local_test_workspace()

  calls <- local_javastics_mocks(workspace_path = NULL)

  expect_null(suppressMessages(run_javastics(javastics, workspace)))
  # No usm has been run
  expect_null(calls$cmd_type)
})

test_that("run_javastics restores the working directory", {
  javastics <- local_test_workspace()
  workspace <- local_test_workspace()
  work_dir <- getwd()

  local_javastics_mocks(workspace_path = NULL)

  suppressMessages(run_javastics(javastics, workspace))
  expect_equal(getwd(), work_dir)
})

test_that("run_javastics accepts the stics executable synonyms", {
  javastics <- local_test_workspace()
  workspace <- local_test_workspace()

  # os_suffix() is mocked to "_test"
  for (name in c("modulostics", "stics_modulo", "sticsmodulo")) {
    calls <- local_javastics_mocks(workspace_path = NULL)
    suppressMessages(
      run_javastics(javastics, workspace, stics_exe = name)
    )
    expect_equal(calls$stics_exe[1], "stics_modulo_test")
  }
})

test_that("run_javastics keeps a custom stics executable name", {
  javastics <- local_test_workspace()
  workspace <- local_test_workspace()

  calls <- local_javastics_mocks(workspace_path = NULL)
  suppressMessages(
    run_javastics(javastics, workspace, stics_exe = "stics_custom.exe")
  )

  expect_equal(calls$stics_exe[1], "stics_custom.exe")
})

test_that("run_javastics stops when no given usm exists", {
  javastics <- local_test_workspace()
  workspace <- local_test_workspace()

  local_javastics_mocks(
    usms_list = c("wheat", "maize"),
    workspace_path = workspace
  )

  expect_error(
    run_javastics(javastics, workspace, usm = "unknown_usm"),
    regexp = "Not any usm exist"
  )
})

test_that("run_javastics warns when only some of the usms exist", {
  javastics <- local_test_workspace()
  workspace <- local_test_workspace()

  local_javastics_mocks(
    usms_list = c("wheat", "maize"),
    workspace_path = workspace,
    command = local_fake_javastics_cmd()
  )

  expect_warning(
    suppressMessages(
      run_javastics(
        javastics,
        workspace,
        usm = c("wheat", "unknown_usm"),
        verbose = FALSE
      )
    ),
    regexp = "At least one usm does not exist"
  )
})

test_that("run_javastics runs all the workspace usms by default", {
  skip_on_os("windows")

  javastics <- local_test_workspace()
  workspace <- local_test_workspace()

  calls <- local_javastics_mocks(
    usms_list = c("wheat", "maize"),
    workspace_path = workspace,
    command = local_fake_javastics_cmd()
  )

  out <- suppressMessages(
    run_javastics(javastics, workspace, verbose = FALSE)
  )

  expect_length(out, 2)
  expect_equal(
    vapply(out, function(x) x$name, character(1)),
    c("wheat", "maize")
  )
  expect_false(out[[1]]$error)
  expect_false(out[[2]]$error)
  # The fake command echoes "<cmd_string> <usm name>"
  expect_equal(out[[1]]$message, "run wheat")
  # The workspace given to JavaStics is the checked one
  expect_equal(calls$cmd_workspace, workspace)
  expect_equal(calls$cmd_type, "run")
})

test_that("run_javastics reports an error returned by the command line", {
  skip_on_os("windows")

  javastics <- local_test_workspace()
  workspace <- local_test_workspace()

  local_javastics_mocks(
    usms_list = "wheat",
    workspace_path = workspace,
    # The echoed arguments hold "Error", as JavaStics does when it fails
    command = local_fake_javastics_cmd(),
    cmd_string = "run --Error"
  )

  out <- expect_message(
    run_javastics(javastics, workspace, verbose = TRUE),
    regexp = "Error during simulation"
  )

  out <- suppressMessages(run_javastics(javastics, workspace, verbose = FALSE))
  expect_true(out[[1]]$error)
  expect_match(out[[1]]$message, "Error", fixed = TRUE)
})

test_that("run_javastics tells the user when all the usms ran", {
  skip_on_os("windows")

  javastics <- local_test_workspace()
  workspace <- local_test_workspace()

  local_javastics_mocks(
    usms_list = "wheat",
    workspace_path = workspace,
    command = local_fake_javastics_cmd()
  )

  expect_message(
    run_javastics(javastics, workspace, verbose = TRUE),
    regexp = "All usms ran successfully"
  )
})

test_that("run_javastics generates the text files then runs Stics if optim", {
  skip_on_os("windows")

  javastics <- local_test_workspace()
  workspace <- local_test_workspace()

  calls <- local_javastics_mocks(
    usms_list = "wheat",
    workspace_path = workspace,
    command = local_fake_javastics_cmd(),
    run_system_out = list(list(name = "wheat", error = FALSE, message = "ok"))
  )

  out <- suppressMessages(
    run_javastics(javastics, workspace, optim = TRUE, verbose = FALSE)
  )

  # JavaStics is only used to generate the text files ...
  expect_equal(calls$cmd_type, "generate")
  # ... and the model is then run directly on the workspace
  expect_equal(
    calls$run_system_exe,
    file.path(javastics, "bin", "stics_modulo_test")
  )
  expect_equal(calls$run_system_workspace, workspace)
  expect_false(out[[1]]$error)
  expect_equal(out[[1]]$message, "ok")
})

test_that("run_javastics keeps a copy of the history file", {
  skip_on_os("windows")

  javastics <- local_test_workspace()
  workspace <- local_test_workspace()

  local_javastics_mocks(
    usms_list = "wheat",
    workspace_path = workspace,
    command = local_fake_javastics_cmd(history_dir = workspace)
  )

  suppressMessages(
    run_javastics(javastics, workspace, keep_history = TRUE, verbose = FALSE)
  )

  expect_true(file.exists(file.path(workspace, "modhistory_wheat.sti")))
})

test_that("run_javastics does not keep the history file if not required", {
  skip_on_os("windows")

  javastics <- local_test_workspace()
  workspace <- local_test_workspace()

  # A history copy left by a previous run must be removed
  old_copy <- file.path(workspace, "modhistory_wheat.sti")
  file.create(old_copy)

  local_javastics_mocks(
    usms_list = "wheat",
    workspace_path = workspace,
    command = local_fake_javastics_cmd(history_dir = workspace)
  )

  suppressMessages(
    run_javastics(javastics, workspace, keep_history = FALSE, verbose = FALSE)
  )

  expect_false(file.exists(old_copy))
})

test_that("run_javastics removes the history file before running", {
  skip_on_os("windows")

  javastics <- local_test_workspace()
  workspace <- local_test_workspace()

  # A history file left by a previous run must be removed first
  histo_file <- file.path(workspace, "modhistory.sti")
  file.create(histo_file)

  local_javastics_mocks(
    usms_list = "wheat",
    workspace_path = workspace,
    # This fake command does not produce any history file
    command = local_fake_javastics_cmd()
  )

  suppressMessages(
    run_javastics(javastics, workspace, keep_history = TRUE, verbose = FALSE)
  )

  expect_false(file.exists(histo_file))
  expect_false(file.exists(file.path(workspace, "modhistory_wheat.sti")))
})

test_that("run_javastics displays the generation output when verbose", {
  skip_on_os("windows")

  javastics <- local_test_workspace()
  workspace <- local_test_workspace()

  calls <- local_javastics_mocks(
    usms_list = "wheat",
    workspace_path = workspace,
    command = local_fake_javastics_cmd(),
    cmd_string = "generate",
    run_system_out = list(list(name = "wheat", error = FALSE, message = "ok"))
  )

  # With optim = TRUE and verbose = TRUE, the JavaStics output is written
  # directly to the console (stdout = ""), so it cannot be captured here:
  # only the resulting behaviour is checked.
  out <- suppressMessages(
    run_javastics(javastics, workspace, optim = TRUE, verbose = TRUE)
  )

  expect_equal(calls$cmd_type, "generate")
  expect_equal(calls$run_system_workspace, workspace)
  expect_false(out[[1]]$error)
})
