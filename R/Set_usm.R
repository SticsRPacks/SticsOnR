
#' Create a simulation unit (USM) for the STICS model
#'
#' @description Uses a pre-existing USM, copy files to target folder(s) of simulation,
#'              set the USMs ready for simulation. \code{set_usm} create as many sub-folders
#'              as USM neede in the target folder.
#'
#' @param dir.orig  Path to the directory from which copy the simulation files. If
#'                  \code{NULL} (the default), uses the package dummy USM.
#' @param dir.targ  Path to the target directory for simulation. Created if missing.
#' @param dir.stics Path to the STICS model executable.
#' @param usm_name  Vector name of the USM(s).
#'
#' @details This function is a helper function used by other package functions
#'
#' @examples
#'
#' @export
set_usm= function(dir.orig=NULL, dir.targ= getwd(), dir.stics= dir.targ,
                  usm_name= NULL){
  if(is.null(dir.orig)){
    # Add example data files:
    Files= list.files("0-DATA/dummy/Wheat_Wheat/", full.names = T)
  }else{
    Files= list.files(dir.dummy, full.names = T)
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

  if(!dir.exists(dir.targ)){
    dir.create(dir.targ)
    overw= F
  }else{
    File_already=
      basename(Files)[file.exists(list.files(dir.targ, full.names = T),basename(Files))]
    if(length(File_already)>0){
      overw= NULL
      count= 1
      while(is.null(overw)){
        if(count==1){
          cat(paste("Files:\n",paste(File_already, collapse = "\n "),
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

  written= file.copy(from = Files, to= dir.targ, recursive = T,overwrite = overw)
  cat(paste("Files:\n",paste(basename(Files[written]), collapse = ", "),
            "\nwere all sucessfully written in",dir.targ))
  if(any(!written)){
    cat(paste("Files:\n",paste(basename(Files[written]), collapse = ", "),
              "\nwere not replaced in",dir.targ,"following your request to not overwrite"))
  }

}

dir.targ= "1-Simulations/Test"
dir.stics= "1-Simulations/Test"
