context("Read meteo file (read_met)")

library(sticRs)

test_that("read_met returns a data.frame", {
  expect_true(is.data.frame(read_met(dirpath = "C:/Users/vezyr/OneDrive/Postdoc/sticRs/example_data")))
})
