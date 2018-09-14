#' Read STICS observation file (*.obs)
#'
#' @description Read STICS observation file for sole or mixed crops.
#'
#' @param dirpath  Directory path
#' @param filename A vector of observation file name(s). Optional, see details.
#' @param mixed    (optional) Is the simulation on mixed species (boolean)
#'
#' @details If \code{filename} is not specified (or equal to \code{NULL}), the
#'          function tries first to match the \code{mod_s*} names for the same \*.obs
#'          names, and then to use the \code{.obs} file if there is one only
#'          (used for sole crops or for both mixed crops). If there are no .obs files,
#'          or two but not specified for reading, the function returns \code{NULL}
#'          If \code{mixed} is not specified (or equal to \code{NULL}), the function
#'          tries to read the number of species from the input files.
#'
#' @return A data.frame (sole crop) or a list of two data.frames (mixed crops) of
#'         the STICS-formated observations. Return \code{NULL} if no files were found,
#'         or more files than useable.
#'
#' @seealso \code{\link{read_output}}
#'
#' @importFrom data.table fread rbindlist
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
    if(file.exists(file.path(dirpath,"new_travail.usm"))){
      nbplants=
        read_usm(filepath = file.path(dirpath,"new_travail.usm"))$P_nbplantes%>%
        as.numeric
      if(nbplants>1){mixed= T}else{mixed= F}
    }else{
      if(length(list.files(dirpath)%>%.[grep("\\.obs$",.)])==1){
        # If there is only one .obs file, the value of mixed doesn't matter
        mixed=F
      }else{
        stop("mixed= NULL, there are several .obs files, and new_travail.usm",
             " cannot be found, please set the mixed parameter")
      }
    }
  }

  # If no filename is given, trying to:
  # (1) use the mod_s* names or
  # (2) use the *.obs file if there is only one
  if(is.null(filename)){
    if(mixed){
      Plant_name=
        list.files(dirpath)%>%.[grep("mod_sp",.)]%>%gsub("mod_sp","",.)%>%
        strsplit(.,"\\.")%>%{if(length(.)>0){.[[1]]%>%.[1]}}
      Plant_name=
        list.files(dirpath)%>%.[grep("mod_sa",.)]%>%gsub("mod_sa","",.)%>%
        strsplit(.,"\\.")%>%{if(length(.)>0){.[[1]]%>%.[1]}}%>%c(Plant_name,.)
    }else{
      Plant_name=
        list.files(dirpath)%>%.[grep("mod_s",.)]%>%gsub("mod_s","",.)%>%
        strsplit(.,"\\.")%>%{if(length(.)>0){.[[1]]%>%.[1]}}
    }

    # If the *.obs names are the same used for mod_s* files, read them accordingly...
    if(all(file.exists(file.path(dirpath,paste0(Plant_name,".obs"))))){
      Table_obs= lapply(Plant_name, function(x){
        Out= data.table::fread(file.path(dirpath,paste0(x,".obs")), data.table = F)
        Out[Out<=-999.99]= NA
        Out$Plant= x
        Del_spe_col(Out)
      })
      Table_obs= data.table::rbindlist(Table_obs, fill=T)
      attrfiles= Plant_name
      warning("Observation file names read from matching mod_s* file names.\nmod_s* names:",
              Plant_name, "\n*.obs:",paste0(Plant_name,".obs"))
    }else{
    # ...else try to read a single *.obs file (multiple .obs file are not allowed)
      obs_files= list.files(dirpath)%>%.[grep("\\.obs$",.)]
      if(length(obs_files)==1){
        Table_obs= data.table::fread(file.path(dirpath,obs_files), data.table = F)
        Table_obs= Del_spe_col(Table_obs)
        attrfiles= obs_files
        Table_obs[Table_obs<=-999.99]= NA
        Table_obs$Plant= obs_files
        warning("Observation file guessed from the only '.obs' file in dirpath",
                Plant_name, "\n*.obs:",paste0(Plant_name,".obs"))
      }else{
        warning("\nObservation file names do not match mod_s* file names and several *.obs ",
             "file names are present. Please provide the *.obs file names using the ",
             "filename parameter")
        Table_obs= NULL
      }
    }
  }else{
    Table_obs= lapply(filename, function(x){
      Out= data.table::fread(file.path(dirpath,x), data.table = F)
      Out[Out<=-999.99]= NA
      Out$Plant= x
      Del_spe_col(Out)
    })
    Table_obs= data.table::rbindlist(Table_obs, fill=T)
    attrfiles= filename
  }

  if(!is.null(Table_obs)){
    Date= data.frame(Date=as.POSIXct(x = paste(Table_obs$ian,Table_obs$mo,Table_obs$jo, sep="-"),
                                     format = "%Y-%m-%d", tz="UTC"))
    Table_obs= cbind(Date,Table_obs)
    attr(Table_obs, "file")= attrfiles
  }

  return(Table_obs)
}
