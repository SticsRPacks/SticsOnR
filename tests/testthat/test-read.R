context("Test read functions")

library(SticsOnR)


# eval_output -------------------------------------------------------------

test_that("Test that eval_output returns a data.frame", {
  expect_true(is.data.frame(eval_output(dirpath = "example_data",
                                        obs_name = c("wheat_1.obs","wheat_2.obs"))))
})

test_that("Test that eval_output returns right errors", {
  # Should return an error if inputting one name only for mixed species
  expect_error(eval_output(dirpath = "example_data", mixed = T, obs_name = "only_one_name"),
               regexp = "Expected two obs. files in obs_name when mixed is TRUE, found less")
  # Should return an error if inputting more than one name for sole crops
  expect_error(eval_output(dirpath = "example_data",mixed = F, obs_name = c("1","2")))
})

test_that("Test that eval_output returns the right format", {
  out= eval_output(dirpath = "example_data",obs_name = c("wheat_1.obs","wheat_2.obs"))
  expect_equal(colnames(out)[1:6],c("Dominance","Date","ian","mo","jo","jul"))
  expect_equal(unique(out$Dominance)[order(unique(out$Dominance))],
               c("Associated","Principal"))
  expect_equal(class(out$Date)[1],"POSIXct")
})

test_that("Test eval_output consistancy", {
  tmp <- tempfile()
  expect_known_output(tail(eval_output(dirpath = "example_data",
                                       obs_name = c("wheat_1.obs","wheat_2.obs")),2),tmp)
})

# read_param --------------------------------------------------------------

test_that("Test read_param output consistancy", {
  tmp <- tempfile()
  expect_known_output(read_param(dirpath = "example_data"),tmp)
})

test_that("Test that read_param returns right errors", {
  expect_error(read_param(dirpath = "example_data",param= "zdklji"))
})

