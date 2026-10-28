# Tests on stics_wrapper_options(), restricted to the cases that do not
# require a Stics executable nor a JavaStics installation.

test_that("stics_wrapper_options returns a template without arguments", {
  options <- stics_wrapper_options()

  expect_type(options, "list")
  # `successive` is part of the template, as documented, even if NULL
  expect_named(
    options,
    c(
      "javastics",
      "stics_exe",
      "workspace",
      "parallel",
      "cores",
      "time_display",
      "verbose",
      "successive",
      "force"
    )
  )
  expect_true("successive" %in% names(options))

  expect_equal(options$javastics, "unknown")
  expect_equal(options$stics_exe, "unknown")
  expect_equal(options$workspace, "unknown")
  expect_false(options$parallel)
  expect_equal(options$cores, NA)
  expect_false(options$time_display)
  expect_true(options$verbose)
  expect_null(options$successive)
  expect_false(options$force)
})

test_that("stics_wrapper_options skips all the checks when force is TRUE", {
  # None of these paths exist: no check must be done, and no error raised.
  options <- stics_wrapper_options(
    javastics = "/unknown/javastics",
    stics_exe = "/unknown/stics_exe",
    workspace = "/unknown/workspace",
    parallel = TRUE,
    cores = 2,
    time_display = TRUE,
    verbose = FALSE,
    force = TRUE,
    successive = list(c("usm1", "usm2"))
  )

  expect_equal(options$javastics, "/unknown/javastics")
  expect_equal(options$stics_exe, "/unknown/stics_exe")
  expect_equal(options$workspace, "/unknown/workspace")
  expect_true(options$parallel)
  expect_equal(options$cores, 2)
  expect_true(options$time_display)
  expect_false(options$verbose)
  expect_equal(options$successive, list(c("usm1", "usm2")))
  expect_true(options$force)
})

test_that("stics_wrapper_options needs a workspace", {
  expect_error(
    stics_wrapper_options(javastics = "/unknown/javastics"),
    regexp = "workspace argument is mandatory"
  )
  expect_error(
    stics_wrapper_options(stics_exe = "modulostics"),
    regexp = "workspace argument is mandatory"
  )
})

test_that("stics_wrapper_options errors on an unknown executable", {
  expect_error(
    stics_wrapper_options(
      workspace = tempdir(),
      stics_exe = file.path(tempdir(), "no_such_stics_exe")
    ),
    regexp = "was not found"
  )
})

test_that("stics_wrapper_options keeps extra arguments when force is TRUE", {
  # As when not forced, the `...` arguments are appended to the options
  options <- stics_wrapper_options(
    workspace = "/unknown/workspace",
    force = TRUE,
    a_new_option = "a_value"
  )

  expect_equal(options$a_new_option, "a_value")
  # successive is kept in the list, even if NULL
  expect_true("successive" %in% names(options))
  expect_null(options$successive)
})

test_that("stics_wrapper_options forced options can be checked again", {
  # Forced options are typically sent to a remote, then given to
  # stics_wrapper() which calls stics_wrapper_options() on them again
  options <- stics_wrapper_options(
    workspace = "/unknown/workspace",
    stics_exe = "/unknown/stics_exe",
    force = TRUE,
    a_new_option = "a_value"
  )

  options_again <- do.call(stics_wrapper_options, options)

  expect_equal(options_again, options)
})

# The following tests use a fake JavaStics directory and fake stics
# executables (see helper-stics-exe.R), so the checks of the arguments are
# really exercised, without any JavaStics installation.

test_that("stics_wrapper_options finds an executable listed in JavaStics", {
  javastics <- local_fake_javastics_dir()
  workspace <- local_test_workspace()

  options <- stics_wrapper_options(
    javastics = javastics,
    stics_exe = "stics_custom_test",
    workspace = workspace,
    verbose = FALSE
  )

  # The executable file name of the model identifier is used
  expect_equal(options$stics_exe, file.path(javastics, "stics_custom"))
  expect_equal(options$javastics, javastics)
  expect_equal(options$workspace, workspace)
  expect_false(options$force)
})

test_that("stics_wrapper_options finds an executable of the bin folder", {
  skip_on_os("windows")

  javastics <- local_fake_javastics_dir()
  workspace <- local_test_workspace()
  # "modulostics" is resolved to the standard executable name of the platform
  local_fake_stics_exe(
    name = paste0("stics_modulo", SticsRFiles:::os_suffix()),
    dir = file.path(javastics, "bin")
  )

  options <- stics_wrapper_options(
    javastics = javastics,
    stics_exe = "modulostics",
    workspace = workspace,
    verbose = FALSE
  )

  expect_equal(
    options$stics_exe,
    file.path(javastics, "bin", paste0("stics_modulo", SticsRFiles:::os_suffix()))
  )
})

test_that("stics_wrapper_options accepts a path to an executable", {
  skip_on_os("windows")

  workspace <- local_test_workspace()
  exe <- local_fake_stics_exe()

  expect_message(
    options <- stics_wrapper_options(
      stics_exe = exe,
      workspace = workspace,
      verbose = TRUE
    ),
    regexp = "Using stics"
  )

  expect_equal(options$stics_exe, exe)
  # The optional arguments are added in the list
  options <- stics_wrapper_options(
    stics_exe = exe,
    workspace = workspace,
    verbose = FALSE,
    new_option = "value"
  )
  expect_equal(options$new_option, "value")
})

test_that("stics_wrapper_options accepts the standard name synonyms", {
  skip_on_os("windows")

  javastics <- local_fake_javastics_dir()
  workspace <- local_test_workspace()
  exe_name <- paste0("stics_modulo", SticsRFiles:::os_suffix())
  local_fake_stics_exe(name = exe_name, dir = file.path(javastics, "bin"))

  for (name in c("stics_modulo", "sticsmodulo")) {
    options <- stics_wrapper_options(
      javastics = javastics,
      stics_exe = name,
      workspace = workspace,
      successive = list(c("usm1", "usm2")),
      verbose = FALSE
    )

    expect_equal(options$stics_exe, file.path(javastics, "bin", exe_name))
    # The successions are kept in the options list
    expect_equal(options$successive, list(c("usm1", "usm2")))
  }
})
