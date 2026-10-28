# Tests on stics_wrapper(). The Stics model and the SticsRFiles functions
# generating and reading the text files are mocked (see helper-wrapper.R), so
# these tests run on any machine, without Stics nor JavaStics.
#
# `force = TRUE` is used in the model options to skip the checks on the
# executable and on the JavaStics installation.

wrapper_options <- function(workspace, ...) {
  utils::modifyList(
    list(
      workspace = workspace,
      stics_exe = "fake_stics",
      force = TRUE,
      verbose = FALSE
    ),
    list(...)
  )
}

test_that("stics_wrapper requires javastics to force parameters values", {
  workspace <- local_wrapper_workspace()

  expect_error(
    stics_wrapper(
      model_options = wrapper_options(workspace),
      param_values = list(dlaimax = 0.001)
    ),
    regexp = "a JavaStics path must be set in model_options list"
  )
})

test_that("stics_wrapper stops when the workspace holds no usm", {
  workspace <- local_test_workspace()

  expect_error(
    stics_wrapper(model_options = wrapper_options(workspace)),
    regexp = "Not any Stics directories found in"
  )
})

test_that("stics_wrapper simulates all the usms of the workspace", {
  workspace <- local_wrapper_workspace(c("usm1", "usm2"))
  calls <- local_wrapper_mocks()

  res <- stics_wrapper(model_options = wrapper_options(workspace))

  expect_false(res$error)
  expect_equal(calls$run_dirs, c("usm1", "usm2"))
  expect_named(res$sim_list, c("usm1", "usm2"))
  expect_s3_class(res$sim_list, "cropr_simulation")
  # All the simulated data are returned when no selection is required
  expect_equal(res$sim_list$usm1, fake_sim_table())
})

test_that("stics_wrapper only simulates the required situations", {
  workspace <- local_wrapper_workspace(c("usm1", "usm2"))
  calls <- local_wrapper_mocks()

  res <- stics_wrapper(
    model_options = wrapper_options(workspace),
    situation = "usm2"
  )

  expect_equal(calls$run_dirs, "usm2")
  expect_named(res$sim_list, "usm2")
})

test_that("stics_wrapper warns about the situations without a folder", {
  workspace <- local_wrapper_workspace("usm1")
  local_wrapper_mocks()

  out <- run_with_warnings(
    stics_wrapper(
      model_options = wrapper_options(workspace),
      situation = c("usm1", "unknown_usm")
    )
  )

  expect_warned(out$warnings, "for USMs unknown_usm")
})

test_that("stics_wrapper warns on an empty mask and returns no result", {
  workspace <- local_wrapper_workspace("usm1")
  local_wrapper_mocks()

  out <- run_with_warnings(
    stics_wrapper(
      model_options = wrapper_options(workspace),
      sit_var_dates_mask = list()
    )
  )

  expect_warned(out$warnings, "sit_var_dates_mask is empty")
  expect_warned(out$warnings, "will not return simulated results")
  expect_null(out$value$sim_list)
})

test_that("stics_wrapper reports a model execution error", {
  workspace <- local_wrapper_workspace("usm1")
  local_wrapper_mocks(error = TRUE)

  out <- run_with_warnings(
    stics_wrapper(model_options = wrapper_options(workspace))
  )

  expect_warned(out$warnings, "Error running the Stics model for USM usm1")
  expect_warned(out$warnings, "will not return simulated results")
  expect_true(out$value$error)
  expect_null(out$value$sim_list)
})

test_that("stics_wrapper reports an error while reading the outputs", {
  workspace <- local_wrapper_workspace("usm1")
  local_wrapper_mocks(sim = NULL)

  out <- run_with_warnings(
    stics_wrapper(model_options = wrapper_options(workspace))
  )

  expect_warned(out$warnings, "Error reading outputs for")
  expect_true(out$value$error)
})

test_that("stics_wrapper reports a failure of the parameters forcing", {
  workspace <- local_wrapper_workspace("usm1")
  local_wrapper_mocks(force_ok = FALSE)

  out <- run_with_warnings(
    stics_wrapper(
      model_options = wrapper_options(workspace, javastics = "fake_js"),
      param_values = list(dlaimax = 0.001)
    )
  )

  expect_warned(
    out$warnings,
    "Error when generating the forcing parameters file for USM usm1"
  )
  expect_true(out$value$error)
})

test_that("stics_wrapper runs the model once per set of parameters values", {
  workspace <- local_wrapper_workspace("usm1")
  calls <- local_wrapper_mocks()

  res <- stics_wrapper(
    model_options = wrapper_options(workspace, javastics = "fake_js"),
    param_values = data.frame(dlaimax = c(0.001, 0.002))
  )

  # One run and one forcing per set of parameters values
  expect_equal(calls$run_dirs, c("usm1", "usm1"))
  expect_length(calls$forced, 2)
  expect_equal(calls$forced[[1]]$dlaimax, 0.001)
  expect_equal(calls$forced[[2]]$dlaimax, 0.002)
  # The results of both runs are gathered in a single table
  expect_equal(nrow(res$sim_list$usm1), 2 * nrow(fake_sim_table()))
})

test_that("stics_wrapper forces the parameters of the simulated situation", {
  workspace <- local_wrapper_workspace(c("usm1", "usm2"))
  calls <- local_wrapper_mocks()

  stics_wrapper(
    model_options = wrapper_options(workspace, javastics = "fake_js"),
    param_values = data.frame(
      situation = c("usm1", "usm2"),
      variete = c(1, 2),
      dlaimax = c(0.001, 0.002)
    )
  )

  expect_length(calls$forced, 2)
  # The situation column is used to select the values, then dropped
  expect_false("situation" %in% names(calls$forced[[1]]))
  expect_equal(calls$forced[[1]]$dlaimax, 0.001)
  expect_equal(calls$forced[[2]]$dlaimax, 0.002)
  # The variete parameter must come first
  expect_equal(names(calls$forced[[1]])[1], "variete")
})

test_that("stics_wrapper replaces the zeros of the phenological stages", {
  workspace <- local_wrapper_workspace("usm1")
  local_wrapper_mocks(
    sim = fake_sim_table(vars = list(ilevs = c(0, 0, 123), lai_n = 1:3))
  )

  res <- stics_wrapper(model_options = wrapper_options(workspace))

  # The zeros are replaced by the following non-zero value
  expect_equal(res$sim_list$usm1$ilevs, c(123, 123, 123))
  # The other variables are untouched
  expect_equal(res$sim_list$usm1$lai_n, 1:3)
})

test_that("stics_wrapper selects the required variables", {
  workspace <- local_wrapper_workspace("usm1")
  local_wrapper_mocks()

  res <- stics_wrapper(
    model_options = wrapper_options(workspace),
    var = "lai_n"
  )

  expect_named(res$sim_list$usm1, c("Date", "Plant", "Dominance", "lai_n"))
})

test_that("stics_wrapper selects the required dates", {
  workspace <- local_wrapper_workspace("usm1")
  local_wrapper_mocks()

  res <- stics_wrapper(
    model_options = wrapper_options(workspace),
    dates = as.Date("2020-01-02")
  )

  expect_equal(res$sim_list$usm1$Date, as.Date("2020-01-02"))
})

test_that("stics_wrapper warns about the required dates not simulated", {
  workspace <- local_wrapper_workspace("usm1")
  local_wrapper_mocks()

  out <- run_with_warnings(
    stics_wrapper(
      model_options = wrapper_options(workspace, verbose = TRUE),
      dates = as.Date(c("2020-01-02", "2021-06-15"))
    )
  )

  expect_warned(
    out$warnings,
    "Requested date\\(s\\) 2021-06-15 is\\(are\\) not simulated"
  )
  expect_equal(out$value$sim_list$usm1$Date, as.Date("2020-01-02"))
  # The required results are incomplete
  expect_true(out$value$error)
})

test_that("stics_wrapper warns when none of the required dates is simulated", {
  workspace <- local_wrapper_workspace("usm1")
  local_wrapper_mocks()

  out <- run_with_warnings(
    stics_wrapper(
      model_options = wrapper_options(workspace),
      dates = as.Date("2021-06-15")
    )
  )

  expect_warned(out$warnings, "Not any requested date")
  expect_warned(out$warnings, "will not return simulated results")
  expect_true(out$value$error)
})

test_that("stics_wrapper uses the mask to select variables and dates", {
  workspace <- local_wrapper_workspace(c("usm1", "usm2"))
  calls <- local_wrapper_mocks()

  mask <- list(
    usm1 = data.frame(
      Date = as.Date(c("2020-01-01", "2020-01-03")),
      lai_n = c(NA, NA)
    )
  )

  res <- stics_wrapper(
    model_options = wrapper_options(workspace),
    sit_var_dates_mask = mask
  )

  # Only the situation of the mask is simulated and returned
  expect_equal(calls$run_dirs, "usm1")
  expect_named(res$sim_list, "usm1")
  expect_named(res$sim_list$usm1, c("Date", "Plant", "Dominance", "lai_n"))
  expect_equal(
    res$sim_list$usm1$Date,
    as.Date(c("2020-01-01", "2020-01-03"))
  )
  expect_false(res$error)
})

test_that("stics_wrapper adds the missing variables in var.mod and re-runs", {
  workspace <- local_wrapper_workspace("usm1")
  calls <- local_wrapper_mocks()

  # The variable is never simulated: the model is run again once var.mod has
  # been modified, then the user is warned
  out <- run_with_warnings(
    stics_wrapper(
      model_options = wrapper_options(workspace, verbose = TRUE),
      var = c("lai_n", "unknown_var")
    )
  )
  res <- out$value

  expect_warned(out$warnings, "has been modified and the model re-run")
  expect_warned(out$warnings, "although added in")
  expect_equal(calls$run_dirs, c("usm1", "usm1"))
  expect_length(calls$varmod, 1)
  expect_equal(calls$varmod[[1]], c("lai_n", "unknown_var"))
  # The available variable is still returned
  expect_named(res$sim_list$usm1, c("Date", "Plant", "Dominance", "lai_n"))
  # The result is incomplete (a required variable is missing): this must be
  # reported as an error, even if no selection is done on dates
  expect_true(res$error)
})

test_that("stics_wrapper warns when no required variable is a stics one", {
  workspace <- local_wrapper_workspace("usm1")
  # A table without any of the Date, Plant and Dominance keywords
  local_wrapper_mocks(
    sim = tibble::tibble(lai_n = c(0.1, 0.2)),
    force_ok = TRUE
  )

  out <- run_with_warnings(
    stics_wrapper(
      model_options = wrapper_options(workspace),
      var = "unknown_var"
    )
  )

  expect_warned(out$warnings, "not simulated by the Stics model")
  expect_warned(out$warnings, "is \\(are\\) not valid STICS variable\\(s\\)")
  expect_warned(out$warnings, "will not return simulated results")
  expect_true(out$value$error)
})

test_that("stics_wrapper handles a succession of usms", {
  workspace <- local_wrapper_workspace(c("usm1", "usm2"))
  calls <- local_wrapper_mocks(create_recup = TRUE)

  res <- stics_wrapper(
    model_options = wrapper_options(
      workspace,
      successive = list(c("usm1", "usm2"))
    )
  )

  expect_false(res$error)
  # The usms of a succession are run in order
  expect_equal(calls$run_dirs, c("usm1", "usm2"))
  # codesuite is set to 1 before the run of the second usm, then reset to 0
  expect_equal(calls$codesuite, c(1, 0))
  # The recup file of the previous usm has been copied in the second usm
  expect_true(file.exists(file.path(workspace, "usm2", "recup.tmp")))
  # ... and renamed in the first one, for a next call of the wrapper
  expect_true(file.exists(file.path(workspace, "usm1", "recup1.tmp")))
})

test_that("stics_wrapper fails when the previous usm creates no recup file", {
  workspace <- local_wrapper_workspace(c("usm1", "usm2"))
  local_wrapper_mocks(create_recup = FALSE)

  expect_error(
    stics_wrapper(
      model_options = wrapper_options(
        workspace,
        successive = list(c("usm1", "usm2"))
      )
    ),
    regexp = "recup.tmp file not found"
  )
})

test_that("stics_wrapper warns on an inconsistent use of the snow module", {
  workspace <- local_wrapper_workspace(c("usm1", "usm2"))
  # The snow module is used by the first usm only
  local_wrapper_mocks(
    create_recup = TRUE,
    codesnow = list(usm1 = 1, usm2 = 0)
  )

  out <- run_with_warnings(
    expect_error(
      stics_wrapper(
        model_options = wrapper_options(
          workspace,
          successive = list(c("usm1", "usm2"))
        )
      ),
      regexp = "snow module"
    )
  )

  expect_warned(out$warnings, "snow module is not used in the current USM")
  expect_warned(out$warnings, "snow_variables.txt")
})

test_that("stics_wrapper drops the successions with a missing usm", {
  workspace <- local_wrapper_workspace("usm1")
  calls <- local_wrapper_mocks()

  out <- run_with_warnings(
    stics_wrapper(
      model_options = wrapper_options(
        workspace,
        successive = list(c("usm1", "unknown_usm"))
      )
    )
  )

  expect_warned(
    out$warnings,
    "The corresponding successions of USMs will not be simulated"
  )
  # usm1 is still simulated, but as a standalone usm
  expect_equal(calls$run_dirs, "usm1")
  expect_length(calls$codesuite, 0)
  expect_false(out$value$error)
})

test_that("stics_wrapper displays the duration when required", {
  workspace <- local_wrapper_workspace("usm1")
  local_wrapper_mocks()

  expect_output(
    stics_wrapper(
      model_options = wrapper_options(workspace, time_display = TRUE)
    ),
    regexp = "Time difference"
  )
})

test_that("stics_wrapper checks the executable when force is FALSE", {
  skip_on_os("windows")

  workspace <- local_wrapper_workspace("usm1")
  exe <- local_fake_stics_exe()
  local_wrapper_mocks()

  res <- stics_wrapper(
    model_options = list(
      workspace = workspace,
      stics_exe = exe,
      verbose = FALSE
    )
  )

  expect_false(res$error)
})

test_that("stics_wrapper handles a situation without parameters to force", {
  workspace <- local_wrapper_workspace(c("usm1", "usm2"))
  calls <- local_wrapper_mocks()

  res <- stics_wrapper(
    model_options = wrapper_options(workspace, javastics = "fake_js"),
    param_values = data.frame(situation = "usm1", dlaimax = 0.001)
  )

  # usm2 is simulated without forcing any parameter
  expect_equal(calls$run_dirs, c("usm1", "usm2"))
  expect_length(calls$forced, 2)
  expect_true(is.na(calls$forced[[2]][[1]]))
  expect_false(res$error)
})

test_that("stics_wrapper transfers the snow file of a succession", {
  workspace <- local_wrapper_workspace(c("usm1", "usm2"))
  # Both usms use the snow module
  calls <- local_wrapper_mocks(
    create_recup = TRUE,
    create_snow = TRUE,
    codesnow = list(usm1 = 1, usm2 = 1)
  )

  res <- stics_wrapper(
    model_options = wrapper_options(
      workspace,
      successive = list(c("usm1", "usm2"))
    )
  )

  expect_false(res$error)
  # The snow variables of the previous usm are given to the next one
  expect_true(file.exists(file.path(workspace, "usm2", "snow_variables.txt")))
  expect_true(file.exists(file.path(workspace, "usm1", "snow_variables1.txt")))
})

test_that("stics_wrapper warns when the snow module is added in a succession", {
  workspace <- local_wrapper_workspace(c("usm1", "usm2"))
  # The snow module is used by the second usm only
  local_wrapper_mocks(
    create_recup = TRUE,
    codesnow = list(usm1 = 0, usm2 = 1)
  )

  out <- run_with_warnings(
    stics_wrapper(
      model_options = wrapper_options(
        workspace,
        successive = list(c("usm1", "usm2"))
      )
    )
  )

  expect_warned(out$warnings, "snow module is used in the current USM")
})
