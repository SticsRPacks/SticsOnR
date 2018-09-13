context("Test statistics")
library(sticRs)


# stati_stics -------------------------------------------------------------

test_that("Test that stati_stics can read a path or a data.frame", {
  # By passing the USM path:
  expect_true(is.data.frame(stati_stics(dirpath = "example_data",
                                        obs_name = c("wheat_1.obs","wheat_2.obs"))))
  Out= eval_output(dirpath = "example_data",
                   obs_name = c("wheat_1.obs","wheat_2.obs"))
  # By passing the eval_output output as input:
  expect_true(is.data.frame(stati_stics(Out)))
})

test_that("Test that stati_stics returns right errors", {
  # Should return an error if inputting one name only for mixed species
  expect_error(stati_stics("example_data", obs_name = "wheat_1.obs"))
})

test_that("Test that stati_stics returns the right format", {
  # Either using a path
  out= stati_stics(dirpath = "example_data",obs_name = c("wheat_1.obs","wheat_2.obs"))
  expect_equal(colnames(out)[1:5],c("variable","Dominance","Version","n_obs","mean_obs"))
  expect_equal(unique(out$Dominance)[order(unique(out$Dominance))],
               c("Associated","Principal"))
})

test_that("Test stati_stics consistancy", {
  tmp <- tempfile()
  expect_known_output(stati_stics("example_data",
                                  obs_name = c("wheat_1.obs","wheat_2.obs"))[1,],tmp)
})




# predictor_assessment ----------------------------------------------------

sim= c(4,-2,10)
obs= c(4,3,10)

test_that("Test R2", {
  expect_equal(R2(sim, obs),0.7093023, tolerance = .0001)
})

test_that("Test RMSE", {
  expect_equal(RMSE(sim, obs),2.886751, tolerance = .0001)
})

test_that("Test nRMSE", {
  expect_equal(nRMSE(sim, obs),50.94267, tolerance = .0001)
})

test_that("Test MAE", {
  expect_equal(MAE(sim, obs),1.666667, tolerance = .0001)
})

test_that("Test ABS", {
  expect_equal(ABS(sim, obs),1.666667, tolerance = .0001)
})

test_that("Test MSE", {
  expect_equal(MSE(sim, obs),8.333333, tolerance = .0001)
})

test_that("Test EF", {
  expect_equal(EF(sim, obs),0.127907, tolerance = .0001)
})

test_that("Test NSE", {
  expect_equal(NSE(sim, obs),0.127907, tolerance = .0001)
})

test_that("Test Bias", {
  expect_equal(Bias(sim, obs),-1.666667, tolerance = .0001)
})

test_that("Test MAPE", {
  expect_equal(MAPE(sim, obs),0.5555556, tolerance = .0001)
})

test_that("Test FVU", {
  expect_equal(FVU(sim, obs),0.5813953, tolerance = .0001)
})

test_that("Test RME", {
  expect_equal(RME(sim, obs),-0.5555556, tolerance = .0001)
})





