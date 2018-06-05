#' Erase special characters from STICS outputs or observations
#'
#' @description Helper function to replace the first parenthesis by a
#' "_" and erase the second. For exemple, lai(n) becomes lai_n.
#'
#' @param x A data.frame
#'
#' @return A data.frame with R-ready column names.
#'
#' @examples
#'\dontrun{
#' library(sticRs) ; library(data.table)
#' a= data.table(`lai(n)`= c(1:8))
#' Del_spe_col(a)
#'}
#' @export
#'
Del_spe_col= function(x){
  .=NULL
  colnames(x)=
    gsub("\\(","_",colnames(x))%>%
    gsub("\\)","",.)
  return(x)
}
