#' Run STICS
#'
#' @description Helper function to start a simulation using
#' STICS executable
#'
#' @param dirpath USM directory path
#'
#' @examples
#'\dontrun{
#' library(sticRs)
#' run_stics()
#'}
#' @export
#'
run_stics=function(dirpath=getwd()){
  wd= getwd()
  setwd(dirpath)
  system2("stics")
  setwd(wd)
}
