
#' Create a simulation unit (USM) for the STICS model
#'
#' @description Uses a pre-existing USM, copy files to target folder(s) of simulation,
#'              set the USMs ready for simulation. \code{set_usm} create as many sub-folders
#'              as USM neede in the target folder.
#'
#' @param dir.orig  Path to the directory from which copy the simulation files. If
#'                  \code{NULL} (the default), uses the package dummy USM.
#' @param dir.targ  Path to the target directory for simulation. Created if missing.
#' @param dir.stics Path to the STICS model executable (optional, only needed if not present
#'                  in dir.orig)
#' @param usm_name  Vector name of the USM(s).
#'
#' @details This function is a helper function used by other package functions
#'
#' @examples
#'\dontrun{
#' # Set a dummy simulation in the folder "1-Simulations", relative to the
#' # project path:
#'
#' library(sticRs)
#' set_usm(dir.targ = "1-Simulations")
#'
#'}
#'
#' @export
set_usm= function(dir.orig=NULL, dir.targ= getwd(),
                  dir.stics= NULL,usm_name= NULL){
  if(is.null(dir.orig)){
    # Add example data files:
    Files= list.files("0-DATA/dummy/Wheat_Wheat/", full.names = T)
  }else{
    Files= list.files(dir.orig, full.names = T)
  }

  if(is.null(usm_name)){
    count= 1
    usm_name= dummy_name= "stics_usm"
    dir_names= basename(list.dirs(dir.targ)[-1])
    while(length(grep(usm_name,dir_names))>0){
      usm_name= paste0(dummy_name,"_",count)
      count= count+1
    }
  }

  usm_path= file.path(dir.targ,usm_name)

  if(!dir.exists(usm_path)){
    dir.create(usm_path)
    overw= F
  }else{
    File_already=
      basename(Files)[file.exists(
        list.files(usm_path, full.names = T),basename(Files))]
    if(length(File_already)>0){
      overw= NULL
      count= 1
      while(is.null(overw)){
        if(count==1){
          cat(paste("Files:\n",
                    paste(File_already, collapse = "\n "),
                    "\nalready in the target folder, overwrite (y/n)?"))
        }
        count= count+1
        ans= readline()
        if(ans=="y"){overw= T}else if(ans=="n"){overw= F}else{
          cat("Please type y for yes, n for no\n")
        }
      }
    }else{
      overw= F
    }
  }

  written= file.copy(from = Files, to= usm_path,
                     recursive = T,overwrite = overw)
  if(!is.null(dir.stics)){
    written= c(written, file.copy(from = dir.stics, to= usm_path,
                                  recursive = T, overwrite = overw))
  }
  if(any(written)){
    cat(paste("Files:\n",
              paste(basename(Files[written]), collapse = ", "),
              "\nwere all sucessfully written in",usm_path))
  }

  if(any(!written)){
    cat(paste("\nFiles:\n",
              paste(basename(Files[!written]), collapse = ", "),
              "\nwere not replaced in",usm_path,
              "following your request to not overwrite"))
  }

}


#' Replace STICS input file parameter
#'
#' @description Replace or set an input parameter from a pre-existing STICS input
#'              file. Generally used after calling \code{\link{set_usm}}.
#'
#' @param filepath File path
#' @param param    Parameter name
#' @param value    New value
#'
#' @seealso \code{\link{set_usm}}.
#'
#' @examples
#'\dontrun{
#' # Replace the interrow distance parameter to 0.01:
#'
#' library(sticRs)
#' set_param(filepath = "fictec1.txt", param= "interrang", value= 0.01)
#'
#'}
#'
#' @export
set_param= function(filepath, param= "interrang", value= 0.01){

  # write (fictec,'(A6,i1,A4)') 'fictec',i,'.txt'
  # read.fortran(file = filepath,format = 'A50')
}




#' @rdname set_param
#' @export
set_station= function(filepath,param,value){
  params= readLines(filepath)
  ref= read_station(filepath)
  ref_index= grep(param,names(ref))
  params[ref_index*2]= value
  writeLines(params,filepath)
}


#' @rdname set_param
#' @export
set_ini= function(filepath,param,value){
  params= readLines(filepath)
  ref= read_ini(filepath)
  ref_index= grep(param,names(ref))
  params[ref_index*2]= value
  writeLines(params,filepath)
}


#' @rdname set_param
#' @export
set_general= function(filepath,param,value){
  params= readLines(filepath)
  ref_index= grep(gsub('P_','',param),params)+1
  if(!length(ref_index)>0){
    stop(paste(param,"parameter not found in:\n",filepath))
  }
  if(length(ref_index)!=length(value)){
    stop(paste("Length of input value different from parameter value length.\n",
               "Original values:\n",paste(params[ref_index],collapse= ", "),
               "\ninput:\n",paste(value,collapse= ", ")))
  }
  params[ref_index]= value
  writeLines(params,filepath)
}

#' @rdname set_param
#' @export
set_plant= function(filepath,param,value){
  params= readLines(filepath)
  ref_index= grep(gsub('P_','',param),params)+1

  if(!length(ref_index)>0){
    stop(paste(param,"parameter not found in:\n",filepath))
  }
  if(length(ref_index)!=length(value)){
    stop(paste("Length of input value different from parameter value length.\n",
               "Original values:\n",paste(params[ref_index],collapse= ", "),
               "\ninput:\n",paste(value,collapse= ", ")))
  }
  params[ref_index]= value
  writeLines(params,filepath)
}

#' @rdname set_param
#' @export
set_tec= function(filepath,param,value){
  params= readLines(filepath)
  ref_index= grep(gsub('P_','',param),params)+1

  if(!length(ref_index)>0){
    stop(paste(param,"parameter not found in:\n",filepath))
  }
  if(length(ref_index)!=length(value)){
    stop(paste("Length of input value different from parameter value length.\n",
               "Original values:\n",paste(params[ref_index],collapse= ", "),
               "\ninput:\n",paste(value,collapse= ", ")))
  }
  params[ref_index]= value
  writeLines(params,filepath)
}


#' @rdname set_param
#' @export
set_soil= function(filepath,param,value){
  ref= read_soil(filepath)
  ref[param]= value

  writeLines(c(ref$P_typsol,ref$P_argi,ref$P_Norg,ref$P_profhum,ref$P_calc,
               ref$P_pH,ref$P_concseuil,ref$P_albedo,ref$P_q0,ref$P_ruisolnu,
               ref$P_obstarac,ref$P_pluiebat,ref$P_mulchbat,ref$P_zesx,
               ref$P_cfes,ref$P_z0solnu ,ref$P_CsurNsol,ref$P_penterui),
             filepath)

  write(c(ref$P_typsol,ref$P_argi,ref$P_Norg,ref$P_profhum,ref$P_calc,
          ref$P_pH,ref$P_concseuil,ref$P_albedo,ref$P_q0,ref$P_ruisolnu,
          ref$P_obstarac,ref$P_pluiebat,ref$P_mulchbat,ref$P_zesx,
          ref$P_cfes,ref$P_z0solnu ,ref$P_CsurNsol,ref$P_penterui),
        filepath,append = T)

  write(c(ref$P_codecailloux,ref$P_codemacropor,ref$P_codefente,
          ref$P_codrainage,ref$P_coderemontcap,ref$P_codenitrif,
          ref$P_codedenit),
        filepath,append = T)

  write(c(ref$P_profimper,ref$P_ecartdrain,ref$P_ksol,ref$P_profdrain,
          ref$P_capiljour,ref$P_humcapil,ref$P_profdenit,ref$P_vpotdenit),
        filepath,append = T)

  for(icou in 1:5){
    writeLines(c(ref$P_epc[icou],ref$P_hccf[icou],ref$P_hminf[icou],
                 ref$P_DAF[icou],ref$P_cailloux[icou],ref$P_typecailloux[icou],
                 ref$P_infil[icou],ref$P_epd[icou]),
               filepath,append = T)
  }
}
