
#' Create a simulation unit (USM) for the STICS model
#'
#' @description Uses a pre-existing USM, copy files to target folder(s) of simulation,
#'              set the USMs ready for simulation. \code{import_usm} create as many sub-folders
#'              as USM neede in the target folder.
#'
#' @param dir.orig  Path to the directory from which copy the simulation files. If
#'                  \code{NULL} (the default), uses the package dummy USM.
#' @param dir.targ  Path to the target directory for simulation. Created if missing.
#' @param stics     Path to the STICS model executable (optional, only needed if not
#'                  present in dir.orig)
#' @param usm_name  Vector name of the USM(s).
#' @param overwrite Boolean. Overwrite files and folders if already present. See details.
#' @param verbose   Boolean. Does the function output success and failure messages ?
#'
#' @details This function is a helper function used by other package functions.
#'  If \code{overwrite= F}, the function show to the user which files
#'  are already present, and asks the user what to do, so be careful while using
#'  this function for programming.
#'
#' @examples
#'\dontrun{
#' # Set a dummy simulation in the folder "1-Simulations", relative to the
#' # project path:
#'
#' library(sticRs)
#' import_usm(dir.targ = "1-Simulations")
#'
#'}
#'
#' @export
import_usm= function(dir.orig=NULL, dir.targ= getwd(),stics= NULL,
                     usm_name= NULL,overwrite= T,verbose=NULL){
  if(is.null(verbose)&!interactive()){verbose=F}else{verbose=T}
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

  if(!dir.exists(usm_path)|overwrite){
    if(overwrite){
      unlink(usm_path,recursive = T)
    }
    dir.create(usm_path,recursive = T)
    overw= F
  }else{
    File_already=
      basename(Files)[file.exists(
        list.files(usm_path, full.names = T),basename(Files))]
    if(!is.null(stics)){
      File_already=
        c(File_already,
          basename(stics)[file.exists(stics)])
    }

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
  Filenames= basename(Files[written])
  if(!is.null(stics)){
    Already_stic= file.exists(stics[[1]])
    if(Already_stic&!overwrite){
      overw= NULL ; count= 1
      while(is.null(overw)){
        if(count==1){
          cat(paste("Stics executable already in the folder, overwrite (y/n)?"))
        }
        count= count+1
        ans= readline()
        if(ans=="y"){overw= T}else if(ans=="n"){overw= F}else{
          cat("Please type y for yes, n for no\n")
        }
      }
    }else{
      overw= T
    }
    written= c(written, file.copy(from = stics, to= usm_path,
                                  recursive = T, overwrite = overw))
    Filenames= c(Filenames, "stics executable")
  }
  if(any(written)&verbose){
    cat(paste("Files:\n",
              paste(Filenames[written], collapse = ", "),
              "\nwere all sucessfully written in",usm_path,"\n"))
  }

  if(any(!written)&verbose){
    cat(paste("\n\nFiles:\n",
              paste(Filenames[!written], collapse = ", "),
              "\nwere not replaced in",usm_path,
              "following your request to not overwrite","\n"))
  }

}


#' Set (replace) STICS input file parameters
#'
#' @description Replace or set an input parameter from a pre-existing STICS input
#'              file. Generally used after calling \code{\link{import_usm}}.
#'
#' @param dirpath  USM directory path
#' @param filepath Path to the parameter file
#' @param param    Parameter name
#' @param value    New parameter value
#' @param plant    Plant index. Optional, only for plant or technical parameters
#' @param vars     Vector of variable names for STICS output requirements
#' @param add      Boolean. Append input to existing file (add to the list)
#'
#' @details The \code{plant} parameter can be either equal to \code{1}, \code{2} for
#'          the associated plant in the case of intercrop, or \code{c(1,2)} for both
#'          Princiapl and associated plants.
#'          \code{\link{all_out_var}} is a helper function that returns all possible
#'          output variables.
#'
#' @note \code{set_out_var} is not used by \code{set_param}. To replace the output
#'       variables required from STICS, please directly call \code{set_out_var}.
#'
#' @seealso \code{\link{import_usm}}.
#'
#' @importFrom magrittr "%>%"
#'
#' @examples
#'\dontrun{
#' # Replace the interrow distance parameter to 0.01:
#'
#' library(sticRs)
#' set_param(dirpath = "stics_usm/usm_1", param= "interrang", value= 0.01)
#'
#'}
#'
#' @export
set_param= function(dirpath=getwd(),param,value,add=F,plant=1){
  param_val= read_param(dirpath = dirpath, param = param)
  file_type=
    lapply(strsplit(names(param_val),"\\."), function(x){x[1]})%>%
    unlist%>%unique

  if(length(file_type)>1){
    stop("Parameter found in several files:",paste(file_type,collapse = ", "),
         "\nPlease use the set_* functions directly to set the parameter value.")
  }
  switch(file_type,
         ini= {
           set_ini(filepath = file.path(dirpath,"ficini.txt"),
                   param = param, value = value, add= add)
         },
         general= {
           set_general(filepath = file.path(dirpath,"tempopar.sti"),
                       param = param, value = value, add= add)
         },
         tmp= {
           set_tmp(filepath = file.path(dirpath,"tempoparV6.sti"),
                   param = param, value = value, add= add)
         },
         soil= {
           set_soil(filepath = file.path(dirpath,"param.sol"),
                    param = param, value = value)
         },
         usm= {
           set_usm(filepath = file.path(dirpath,"new_travail.usm"),
                   param = param, value = value)
         },
         station= {
           set_station(filepath = file.path(dirpath,"station.txt"),
                       param = param, value = value, add= add)
         },
         tec= {
           tmp= lapply(plant, function(x){
             set_tec(filepath = file.path(dirpath,paste0("fictec",x,".txt")),
                     param = param, value = value, add= add)
           })
         },
         plant= {
           tmp= lapply(plant, function(x){
             set_plant(filepath = file.path(dirpath,paste0("ficplt",x,".txt")),
                       param = param, value = value, add= add)
           })
         },
         stop("Parameter not found")
  )

}


#' @rdname set_param
#' @export
set_usm= function(filepath="new_travail.usm",param,value,add=F){
  set_file(filepath,param,value,add)
}

#' @rdname set_param
#' @export
set_station= function(filepath="station.txt",param,value,add=F){
  set_file(filepath,param,value,add)
}


#' @rdname set_param
#' @export
set_ini= function(filepath= "ficini.txt",param,value,add=F){
  set_file(filepath,param,value,add)
}


#' @rdname set_param
#' @export
set_general= function(filepath= "tempopar.sti",param,value,add=F){
  set_file(filepath,param,value,add)
}

#' @rdname set_param
#' @export
set_tmp= function(filepath= "tempoparv6.sti",param,value,add=F){
  set_file(filepath,param,value,add)
}

#' @rdname set_param
#' @export
set_plant= function(filepath="ficplt1.txt",param,value,add=F){
  set_file(filepath,param,value,add)
}

#' @rdname set_param
#' @export
set_tec= function(filepath="fictec1.txt",param,value,add=F){
  set_file(filepath,param,value,add)
}

#' @rdname set_param
#' @export
set_soil= function(filepath="param.sol",param,value){
  ref= read_soil(filepath)
  if(length(ref[[param]])!=length(value)){
    stop(paste("Length of input value different from parameter value length.\n",
               "Original values:\n",paste(ref[[param]],collapse= ", "),
               "\ninput:\n",paste(value,collapse= ", ")))
  }
  ref[[param]]= format(value, scientific=F)

  writeLines(paste(" "," "," ",ref$P_numsol[1]," "," "," ",ref$P_typsol,
                   ref$P_argi,ref$P_Norg,ref$P_profhum,ref$P_calc,
                   ref$P_pH,ref$P_concseuil,ref$P_albedo,ref$P_q0,
                   ref$P_ruisolnu,ref$P_obstarac,ref$P_pluiebat,
                   ref$P_mulchbat,ref$P_zesx,ref$P_cfes,
                   ref$P_z0solnu ,ref$P_CsurNsol,ref$P_penterui),
             filepath)

  write(paste(" "," "," ",ref$P_numsol[1]," "," "," ",ref$P_codecailloux,ref$P_codemacropor,
              ref$P_codefente,ref$P_codrainage,ref$P_coderemontcap,
              ref$P_codenitrif,ref$P_codedenit),
        filepath,append = T)

  write(paste(" "," "," ",ref$P_numsol[1]," "," "," ",ref$P_profimper,ref$P_ecartdrain,ref$P_ksol,
              ref$P_profdrain,ref$P_capiljour,ref$P_humcapil,
              ref$P_profdenit,ref$P_vpotdenit),
        filepath,append = T)

  for(icou in 1:5){
    write(paste(" "," "," ",ref$P_numsol[1]," "," "," ",ref$P_epc[icou],ref$P_hccf[icou],
                ref$P_hminf[icou],ref$P_DAF[icou],ref$P_cailloux[icou],
                ref$P_typecailloux[icou],ref$P_infil[icou],
                ref$P_epd[icou]),
          filepath,append = T)
  }
}

#' @rdname set_param
#' @export
set_out_var= function(filepath="var.mod",vars=c("lai(n)","masec(n)"),add= F){
  cat(vars,file=filepath, sep="\n",append = add)
}



#' Internal function to set some STICS input file parameters
#'
#' @description Replace or set an input parameter from a pre-existing STICS input
#'              file. This function is called by some of the generic \code{set_*}
#'              functions under the hood.
#'
#' @param filepath Path to the parameter file
#' @param param    Parameter name
#' @param value    New parameter value
#' @param add      Boolean. Append input to existing file (add to the list)
#'
#' @details The function uses \code{\link[base]{sys.call}} to know from which function
#'          of the \code{set_*} family it is called, so it won't work properly if called
#'          by the user directly. This is why this function is internal.
#'
#' @note This function is not used for \code{\link{set_soil}}.
#'
#' @seealso \code{\link{set_param}}.
#'
#' @keywords internal
#' @export
set_file= function(filepath,param,value,add){
  # access the function name from which set_file was called
  type= strsplit(deparse(sys.call(-1)),split = "\\(")[[1]][1]
  params= readLines(filepath)

  switch(type,
         set_usm = {
           ref= read_usm(filepath)
           if(grep(param,names(ref))<grep("P_fplt",names(ref))){
             ref_index= grep(param,names(ref))*2
           }else{
             ref_index= grep(gsub("P_","",param),params)+1
           }
         },
         set_station= {
           ref= read_station(filepath)
           ref_index= grep(param,names(ref))*2
         },
         set_ini= {
           ref= read_ini(filepath)
           ref_index= grep(param,names(ref))*2
         },
         # Default here
         {
           ref_index= grep(gsub('P_','',param),params)+1
         }
  )

  if(!length(ref_index)>0){
    if(add){
      value= paste(value, collapse = " ")
      params= c(params,param,value)
      ref_index= grep(gsub('P_','',param),params)+1
    }else{
      stop(paste(param,"parameter not found in:\n",filepath))
    }
  }else{
    if(add){
      stop(paste("Parameter",param,"already present in the file, try to replace its value",
                 "instead of adding the parameter"))
    }
  }

  if(length(ref_index)!=length(value)){
    stop(paste("Length of input value different from parameter value length.\n",
               "Original values:\n",paste(params[ref_index],collapse= ", "),
               "\ninput:\n",paste(value,collapse= ", ")))
  }
  params[ref_index]= format(value, scientific=F)
  writeLines(params,filepath)
}
