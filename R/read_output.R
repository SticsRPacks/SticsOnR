#' Read STICS outputs (mod_s*)
#'
#' @description Read STICS model outputs for sole or mixed crops.
#'
#' @param dirpath Directory path
#' @param mixed   (optional) Is the simulation on mixed species (boolean)
#' @param name    Plant name for the output. Especially usefull for mixed crops.
#'                If \code{NULL}, the function tries to read it using \code{\link{read_usm}}
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
read_output= function(dirpath=getwd(), mixed= NULL, name= NULL){
  .=NULL # to avoid CRAN note for pipe
  if(is.null(mixed)){
    nbplants=
      read_usm(filepath = file.path(dirpath,"new_travail.usm"))$P_nbplantes%>%
      as.numeric
    if(nbplants>1){mixed= T}else{mixed= F}
  }

  if(is.null(name)){
    name= read_usm(filepath = file.path(dirpath,"new_travail.usm"))$P_fplt
  }else{
    if(mixed&length(name)!=2){
      stop("name argument should have a length of 2")
    }else if(!mixed&length(name)>1){
      stop("name argument has more values than plant species")
    }
  }
  if(mixed){
    Plant_1_mod= list.files(dirpath)%>%.[grep("mod_sp",.)]
    Plant_2_mod= list.files(dirpath)%>%.[grep("mod_sa",.)]

    if(length(Plant_1_mod)==0|length(Plant_2_mod)==0){
      if(length(Plant_1_mod)==0&length(Plant_2_mod)==0){
        Plant_test= list.files(dirpath)%>%.[grep("mod_s",.)]
        if(length(Plant_test)>0){
          stop("Can't find mod_sp and mod_sa files in the USM directory, but mod_s was found. ",
               "Please set mixed to FALSe to read sole crop USMs")
        }
      }else{
        if(length(Plant_1_mod)==0){
          stop("Can't find mod_sp files in the USM directory")
        }else{
          stop("Can't find mod_sa files in the USM directory")
        }
      }
    }

    Table_1= data.table::fread(file.path(dirpath,Plant_1_mod), data.table = F)
    Table_2= data.table::fread(file.path(dirpath,Plant_2_mod), data.table = F)
    Table_1$Plant= name[1] ; Table_2$Plant= name[2]
    Table_1$Dominance= "Principal" ; Table_2$Dominance= "Associated"
    output= rbind(Table_1,Table_2)
    attrfiles= data.frame(Plant= name, file= c(Plant_1_mod, Plant_2_mod))
  }else{
    Plant_1_mod= list.files(dirpath)%>%.[grep("mod_s",.)]
    if(length(Plant_1_mod)>1){
      if(length(grep("mod_sp|mod_sa",Plant_1_mod))){
        stop("mod_s is not found in the USM directory, but mod_sp and mod_sa were found. ",
             "Please set mixed to TRUE to read mixed species USM")
      }else{
        stop("Several mod_s files were found in the USM directory, please keep one only")
      }
    }
    if(length(Plant_1_mod)==0){
      stop("Can't find any mod_s files in the USM directory")
    }
    Table_1= data.table::fread(file.path(dirpath,Plant_1_mod), data.table = F)
    Table_1$Plant= name[1]
    Table_1$Dominance= "Sole crop"
    output= Table_1
    attrfiles= Plant_1_mod
    attrfiles= data.frame(Plant= name, file= c(Plant_1_mod))
  }
  output= Del_spe_col(output)
  Date= data.frame(Date=as.POSIXct(x = paste(output$ian,output$mo,output$jo, sep="-"),
                                   format = "%Y-%m-%d", tz="UTC"))
  output= cbind(Date,output)
  attr(output,"file")= attrfiles

  return(output)
}
