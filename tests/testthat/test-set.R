context("Test setting functions")

library(sticRs)



# import_usm --------------------------------------------------------------

test_that("Test that import_usm writes the right folders and their names", {
  import_usm(dir.orig = "example_data",dir.targ = "tmp")
  expect_true(dir.exists("tmp"))
  expect_true(dir.exists("tmp/stics_usm"))
  import_usm(dir.orig = "example_data",dir.targ = "tmp")
  expect_true(dir.exists("tmp/stics_usm_1"))
  import_usm(dir.orig = "example_data",dir.targ = "tmp",usm_name = "name",
             all_files = T)
  expect_true(dir.exists("tmp/name"))
  expect_equal(list.files("example_data"),list.files("tmp/name"))
  unlink("tmp",recursive = T)
})


# set_param ---------------------------------------------------------------

test_that("Test that set_param works", {
  tmp= read_param(dirpath = "example_data_tests/set", param = "P_codelegume")
  set_param(dirpath = "example_data_tests/set",param = "P_codelegume",value = 10,plant = c(1,2))
  tmp2= read_param(dirpath = "example_data_tests/set", param = "P_codelegume")
  set_param(dirpath = "example_data_tests/set",param = "P_codelegume",value = tmp[1],plant = 1)
  set_param(dirpath = "example_data_tests/set",param = "P_codelegume",value = tmp[2],plant = 2)
  expect_equal(unname(tmp2),c("10","10"))
})

test_that("Test that set_param returns right errors", {
  expect_error(set_param(dirpath = "example_data",param= "zdklji"))
  expect_error(set_param(dirpath =  "example_data_tests/set",param = "nbplantes",
                         value = 2))
})

