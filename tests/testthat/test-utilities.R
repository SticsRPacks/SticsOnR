context("Test utilities/helper functions")
library(sticRs)

test_that("Test Del_spe_col output", {
  library(data.table)
  expect_equal(colnames(Del_spe_col(data.table(`lai(n)`= 1:3))), "lai_n")
})

test_that("Test all_out_var output", {
  expect_equal(class(all_out_var()), "character")
  expect_gte(length(all_out_var()),542)
})

test_that("Test find_STICS_var output", {
  expect_equal(class(find_STICS_var("hauteur")), "character")
  expect_equal(find_STICS_var("ulai\\(n\\)"),"ulai(n)")
})

test_that("Test rbind_sim output", {
  Dates= seq.Date(as.Date("2016-01-01"),as.Date("2016-01-04"), by="day")
  sim_1= list(outputs= list(
    P_sea_20= data.frame(Date= Dates[1:2], tmax= c(14,13)),
    P_sea_80= data.frame(Date= Dates[1:2], tmax= c(15,11))))
  sim_2= list(outputs= list(
    P_sea_20= data.frame(Date= Dates[3:4], tmax= c(16,12)),
    P_sea_80= data.frame(Date= Dates[3:4], tmax= c(8,13))))
  expect_equal(length(rbind_sim(sim_1, sim_2)), 2)
  expect_equal(mean(rbind_sim(sim_1, sim_2)$P_sea_20$tmax),13.75)
})


