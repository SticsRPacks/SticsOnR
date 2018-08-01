context("Test utilities/hleper functions")
library(sticRs)

test_that("Test Del_spe_col output", {
  library(data.table)
  expect_equal(colnames(Del_spe_col(data.table(`lai(n)`= 1:3))), "lai_n")
})

test_that("Test all_out_var output", {
  expect_equal(class(all_out_var()), "character")
  expect_gte(length(all_out_var()),542)
})
