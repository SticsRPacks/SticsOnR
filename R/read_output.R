#' Read STICS outputs (mod_s*)
#'
#' @description Read STICS model outputs for sole or mixed crops.
#'
#' @param dirpath Directory path
#' @param mixed   (optional) Is the simulation on mixed species (boolean)
#'
#' @details If \code{mixed} is not specified (or equal to \code{NULL}), the function try to
#'          read the number of species from the input files.
#'
#' @note The STICS outputs are controled from the \code{var.mod} file.
#'       \code{\link{set_out_var}} can be used to set the output variables.
#'
#' @return A data.frame (sole crop) or a list of two data.frames (mixed crops) of
#'         the STICS outputs.
#'
#' @seealso \code{\link{read_param}}, \code{\link{set_param}}.
#'
#' @importFrom data.table fread
#'
#' @examples
#'\dontrun{
#' library(sticRs)
#' Output= read_output()
#'}
#'
#' @export
#'
read_output= function(dirpath=getwd(), mixed= NULL){
  .=NULL # to avoid CRAN note for pipe
  if(is.null(mixed)){
    nbplants=
      read_usm(filepath = file.path(dirpath,"new_travail.usm"))$P_nbplantes%>%
      as.numeric
    if(nbplants>1){mixed= T}else{mixed= F}
  }

  if(mixed){
    Plant_1_mod= list.files(dirpath)%>%.[grep("mod_sp",.)]
    Plant_2_mod= list.files(dirpath)%>%.[grep("mod_sa",.)]
    Table_1= data.table::fread(file.path(dirpath,Plant_1_mod), data.table = F)
    Table_2= data.table::fread(file.path(dirpath,Plant_2_mod), data.table = F)
    output= list(Table_1=Table_1,Table_2=Table_2)

  }else{
    Plant_1_mod= list.files(dirpath)%>%.[grep("mod_s",.)]
    Table_1= data.table::fread(file.path(dirpath,Plant_1_mod), data.table = F)
    output= Table_1
  }

  return(output)
}
