#' Read STICS observation file (*.obs)
#'
#' @description Read STICS observation file for sole or mixed crops.
#'
#' @param dirpath  Directory path
#' @param filename A vector of observation file name(s). Optional, see details.
#' @param mixed    (optional) Is the simulation on mixed species (boolean)
#'
#' @details If \code{filename} is not specified (or equal to \code{NULL}), the
#'          function tries first to match the \code{mod_s*} names, and then to
#'          use the \code{.obs} file if there is one only (used for sole crops or
#'          for both mixed crops).
#'          If \code{mixed} is not specified (or equal to \code{NULL}), the function
#'          tries to read the number of species from the input files.
#'
#' @return A data.frame (sole crop) or a list of two data.frames (mixed crops) of
#'         the STICS-formated observations.
#'
#' @seealso \code{\link{read_output}}
#'
#' @importFrom data.table fread
#'
#' @examples
#'\dontrun{
#' library(sticRs)
#' Meas= read_obs()
#'}
#'
#' @export
#'
read_obs= function(dirpath=getwd(), filename=NULL, mixed= NULL){
  .=NULL # to avoid CRAN note for pipe
  if(is.null(mixed)){
    nbplants=
      read_usm(filepath = file.path(dirpath,"new_travail.usm"))$P_nbplantes%>%
      as.numeric
    if(nbplants>1){mixed= T}else{mixed= F}
  }

  # If no filename is given, trying to:
  # (1) use the mod_s* names or
  # (2) use the *.obs file if there is only one
  if(is.null(filename)){
    if(mixed){
      Plant_name=
        list.files(dirpath)%>%.[grep("mod_sp",.)]%>%gsub("mod_sp","",.)%>%
        strsplit(.,"\\.")%>%.[[1]]%>%.[1]
      Plant_name=
        list.files(dirpath)%>%.[grep("mod_sa",.)]%>%gsub("mod_sa","",.)%>%
        strsplit(.,"\\.")%>%.[[1]]%>%.[1]%>%c(Plant_name,.)
    }else{
      Plant_name=
        list.files(dirpath)%>%.[grep("mod_s",.)]%>%gsub("mod_s","",.)%>%
        strsplit(.,"\\.")%>%.[[1]]%>%.[1]
    }

    # If the *.obs names are the same used for mod_s* files, read them accordingly...
    if(all(file.exists(file.path(dirpath,paste0(Plant_name,".obs"))))){
      Table_obs= lapply(Plant_name, function(x){
        data.table::fread(file.path(dirpath,paste0(x,".obs")), data.table = F)
      })
      names(Table_obs)= paste0("Plant_",seq_along(Plant_name))

      cat("Observation file names read from matching mod_s* file names.\nmod_s* names:",
          Plant_name, "\n*.obs:",paste0(Plant_name,".obs"))
    }else{
    # ...else try to read a single *.obs file (multiple .obs file are not allowed)
      obs_files= list.files(dirpath)%>%.[grep("\\.obs$",.)]
      if(length(obs_files)==1){
        Table_obs= data.table::fread(file.path(dirpath,obs_files), data.table = F)
        if(mixed){Table_obs= list(Table_obs,Table_obs)}
      }else{
        stop("\nObservation file names do not match mod_s* file names and several *.obs ",
             "file names are present. Please provide the *.obs file names using the ",
             "filename parameter")
      }
    }
  }else{
    Table_obs= lapply(Plant_name, function(x){
      data.table::fread(file.path(dirpath,paste0(x,".obs")), data.table = F)
    })
  }

  if(length(Table_obs)==1){
    Table_obs= Table_obs[[1]]
  }

  return(Table_obs)
}
