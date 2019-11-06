context("Test utilities/helper functions")
library(SticsOnR)


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


