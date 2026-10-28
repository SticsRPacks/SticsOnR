# Tests on select_results(), limited to the selection branches that do not
# need a Stics simulation output on the disk.

sim_tmp <- data.frame(
  Date = as.Date(c("2020-01-01", "2020-01-02")),
  lai_n = c(0.1, 0.2),
  masec_n = c(1.1, 1.2)
)

test_that("select_results returns all the simulated data if required", {
  res <- select_results(
    keep_all_data = TRUE,
    sit_var_dates_mask = NULL,
    var = NULL,
    dates = NULL,
    situation = "usm1",
    sim_tmp = sim_tmp,
    varmod_modified = FALSE,
    verbose = FALSE,
    run_dir = tempdir()
  )

  expect_equal(res$sim_list, sim_tmp)
  expect_false(res$flag_error)
  expect_true(res$flag_rqd_res)
  expect_false(res$simulate)
  expect_null(res$message)
  expect_false(res$varmod_modified)
})

test_that("select_results returns NULL for a situation out of the mask", {
  mask <- list(usm1 = sim_tmp)

  res <- select_results(
    keep_all_data = FALSE,
    sit_var_dates_mask = mask,
    var = NULL,
    dates = NULL,
    situation = "usm_not_in_mask",
    sim_tmp = sim_tmp,
    varmod_modified = FALSE,
    verbose = FALSE,
    run_dir = tempdir()
  )

  expect_null(res$sim_list)
  expect_false(res$flag_error)
  expect_true(res$flag_rqd_res)
  expect_false(res$simulate)
})

test_that("select_results forwards the varmod_modified flag", {
  res <- select_results(
    keep_all_data = TRUE,
    sit_var_dates_mask = NULL,
    var = NULL,
    dates = NULL,
    situation = "usm1",
    sim_tmp = sim_tmp,
    varmod_modified = TRUE,
    verbose = FALSE,
    run_dir = tempdir()
  )

  expect_true(res$varmod_modified)
})

# Regression tests: the flags set when some required variables are not
# simulated must not be reset when no selection is done on dates.

sim_full <- data.frame(
  Date = as.Date(c("2020-01-01", "2020-01-02")),
  Plant = "plant1",
  Dominance = "Principal",
  lai_n = c(0.1, 0.2)
)

test_that("select_results keeps the missing variables flag without dates", {
  local_mocked_bindings(
    var_to_col_names = function(x) x,
    .package = "SticsRFiles"
  )

  expect_warning(
    res <- select_results(
      keep_all_data = FALSE,
      sit_var_dates_mask = NULL,
      var = c("lai_n", "unknown_var"),
      dates = NULL,
      situation = "usm1",
      sim_tmp = sim_full,
      varmod_modified = TRUE,
      verbose = FALSE,
      run_dir = tempdir()
    ),
    regexp = "unknown_var not simulated"
  )

  # The available variable is returned ...
  expect_named(res$sim_list, c("Date", "Plant", "Dominance", "lai_n"))
  # ... but the result is flagged as incomplete
  expect_false(res$flag_rqd_res)
  expect_false(res$flag_error)
  expect_false(res$simulate)
})

test_that("select_results flags a complete result without dates", {
  local_mocked_bindings(
    var_to_col_names = function(x) x,
    .package = "SticsRFiles"
  )

  res <- select_results(
    keep_all_data = FALSE,
    sit_var_dates_mask = NULL,
    var = "lai_n",
    dates = NULL,
    situation = "usm1",
    sim_tmp = sim_full,
    varmod_modified = FALSE,
    verbose = FALSE,
    run_dir = tempdir()
  )

  expect_true(res$flag_rqd_res)
  expect_false(res$flag_error)
  expect_null(res$message)
})
