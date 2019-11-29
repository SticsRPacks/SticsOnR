## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
path_to_data= "tests/testthat/example_data/"
path_to_simulation= "tests/testthat/example_data/"
library(SticsOnR)

## ----eval=FALSE----------------------------------------------------------
#  # install.packages("git2r")
#  target_path= "path_where_to_download_folder"
#  git2r::clone("https://github.com/VEZY/STICS_dummy", target_path)

## ----echo=FALSE,out.width = '100%', fig.cap="Figure 1: Project structure"----
knitr::include_graphics("project_folders.png")

## ----eval=FALSE----------------------------------------------------------
#  path_to_data= "0-Data/stics_input"
#  path_to_simulation= "2-Simulations"
#  path_to_results= "3-Results"
#  stics_executable = "0-Data/stics_executable/stics.exe"

## ----eval=FALSE----------------------------------------------------------
#  library(SticsOnR)
#  import_usm(dir.orig = path_to_data, dir.targ = path_to_simulation, stics = stics_executable, usm_name = "Test_simulation")

## ----eval=FALSE----------------------------------------------------------
#  read_param(dirpath = path_to_simulation, param = "P_densitesem")

## ----eval=FALSE----------------------------------------------------------
#  tec.plant1.P_densitesem tec.plant2.P_densitesem
#                    "140"                   "140"

## ----eval=FALSE----------------------------------------------------------
#  set_param(dirpath = path_to_simulation, param = "P_densitesem", value = 145, plant = 1)

## ----eval=FALSE----------------------------------------------------------
#  set_param(dirpath = path_to_simulation, param = "P_densitesem", value = 145, plant = c(1,2))

