context("Test read functions")

library(SticsOnR)


# read_met ----------------------------------------------------------------

test_that("Test that read_met returns a data.frame", {
  expect_true(is.data.frame(read_met(dirpath = "example_data")))
})

test_that("read_met returns the right format", {
  met= read_met(dirpath = "example_data")
  expect_equal(ncol(met),10)
  expect_equal(nrow(met),730)
  expect_equal(class(met[,1])[1],"POSIXct")
  expect_equal(class(met[,2]),"character")
  expect_equal(class(met[,3]),"numeric")
})

test_that("Test read_output consistancy", {
  expect_known_hash(read_met(dirpath = "example_data"),
                    hash = "62633fe9a0")
})

# read_obs ----------------------------------------------------------------

test_that("Test that read_obs returns a data.frame", {
  expect_true(is.data.frame(read_obs(dirpath = "example_data",
                                     filename = c("wheat_1.obs","wheat_2.obs"))))
})

test_that("Test read_obs automatic filename", {
  # Reading from the mod file names:
  obs= suppressWarnings(read_obs(dirpath = "example_data_tests/obs_mod",mixed = T))
  expect_true(is.data.frame(obs))
  # Reading from the unique obs directly:
  obs= suppressWarnings(read_obs(dirpath = "example_data_tests/obs_alone"))
  expect_true(is.data.frame(obs))
})

test_that("Test that read_obs returns right errors", {
  # Mixed=T but no filename but obs filenames match mod filenames
  expect_warning(read_obs(dirpath = "example_data_tests/obs_mod",mixed = T))
  # Mixed=T but no filename + obs filenames do not match mod filenames
  expect_warning(read_obs(dirpath = "example_data_tests/obs_mod2",mixed = T))
  # Mixed=T but no filename but one obs file in dirpath
  expect_warning(read_obs(dirpath = "example_data_tests/obs_alone",mixed = T))
  # No mixed parameter and no filename + no new_travail.usm
  expect_error(read_obs(dirpath = "example_data_tests/obs_two"))
})

test_that("Test read_obs output consistancy", {
  tmp <- tempfile()
  expect_known_output(read_obs(dirpath = "example_data",
                               filename = c("wheat_1.obs","wheat_2.obs")),tmp)
})

# read_output -------------------------------------------------------------

test_that("Test that read_output returns a data.frame", {
  expect_true(is.data.frame(read_output(dirpath = "example_data")))
})

test_that("Test that read_output returns right errors", {
  # Should return an error if inputting one name only for mixed species
  expect_error(read_output(dirpath = "example_data",mixed = T,name = "only_one_name"),
               regexp = "name argument should have a length of 2")
  # Should return an error if inputting more than one name for sole crops
  expect_error(read_output(dirpath = "example_data",mixed = F,name = c("1","2")),
               regexp = "name argument has more values than plant species")
})

test_that("Test that read_output returns the right format", {
  out= read_output(dirpath = "example_data")
  expect_equal(colnames(out)[1:5],c("Date","ian","mo","jo","jul"))
  expect_equal(unique(out$Dominance),c("Principal","Associated"))
  expect_equal(class(out[,1])[1],"POSIXct")
})

test_that("Test read_output consistancy", {
  tmp <- tempfile()
  expect_known_output(read_output(dirpath = "example_data"),tmp)
})


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

