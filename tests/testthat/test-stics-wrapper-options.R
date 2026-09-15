# Tests on stics_wrapper_options(), restricted to the cases that do not
# require a Stics executable nor a JavaStics installation.

test_that("stics_wrapper_options returns a template without arguments", {
  options <- stics_wrapper_options()

  expect_type(options, "list")
  # NB: `successive` is documented in the template but assigning NULL to a
  # list element drops it, so it is absent from the returned template.
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
      "force"
    )
  )

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

test_that("stics_wrapper_options drops extra arguments when force is TRUE", {
  # The forced branch returns early: the `...` arguments are not appended.
  options <- stics_wrapper_options(
    workspace = "/unknown/workspace",
    force = TRUE,
    a_new_option = "a_value"
  )

  expect_null(options$a_new_option)
})
