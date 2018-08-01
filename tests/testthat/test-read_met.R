context("Read meteo file (read_met)")

library(sticRs)

test_that("read_met returns a data.frame", {
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
