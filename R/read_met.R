#' Read STICS input meteo file
#'
#' @description Read the meteorology input for STICS ("climat.txt")
#'
#' @param dirpath  Directory path
#' @param filename The meteorology file name (default to \code{climat.txt}).
#'
#' @note The time-related variables are summarised into one POSIXct column named
#'       code{Date}.
#'
#' @return A data.frame of the input meteorological variables used as input for the
#'         STICS model.
#'
#' @seealso \code{\link{read_obs}}
#'
#' @importFrom data.table fread
#'
#' @examples
#'\dontrun{
#' library(sticRs)
#' Meteo= read_met()
#'}
#'
#' @export
#'
read_met= function(dirpath=getwd(), filename="climat.txt"){
  Met= data.table::fread(file.path(dirpath,filename), data.table = F)
  colnames(Met)= c("station","year","month","day","julian","ttmin","ttmax",
                   "ttrg","ttetp","ttrr","ttvent","ttpm","ttco2")
  Date= data.frame(Date=as.POSIXct(x = paste(Met$year,Met$month,Met$day, sep="-"),
                                   format = "%Y-%m-%d", tz="UTC"))
  Table_Met= cbind(Date,Met[,-c(grep("year|month|day|julian",colnames(Met)))])
  attr(Table_Met, "file")= filename
  return(Table_Met)
}
