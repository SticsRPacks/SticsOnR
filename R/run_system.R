run_system <- function(model_path,data_dir,usm_dir_names=NULL) {
  #' @title Running usm(s) from a/several directory(ies)
  #' or a/several subdirectory(ies) named with usm name
  #'
  #' @description This function uses Stics directly through a system call
  #'
  #' @param model_path Path of Stics executable file
  #' @param data_dir Path of a Stics input directory or a vector of,
  #' or root directory of Stics directories (if usm_dir_names is given)
  #' @param usm_dir_names Name(s) of sub-directory(ies) of data_dir
  #'
  #' @return A list with usm names and execution error status
  #'
  #' @examples
  #' run_system("/home/username/bin/Stics","/home/username/Work/SticsInputsDir")
  #' run_system("/home/username/bin/Stics",c("/home/username/Work/SticsInputsDir1",
  #' "/home/username/Work/SticsInputsDir2"))
  #'
  #' run_system("/home/username/bin/Stics",/home/username/Work/SticsInputsRootDir","wheat")
  #' run_system("/home/username/bin/Stics",/home/username/Work/SticsInputsRootDir",
  #' c("wheat","maize"))
  #'
  #' @export
  #'
  # ----------------------------------------------------------------------
  #  MODIFICATIONS (last commit)
  #  $Date: 2019-06-03 11:56:53 +0200 (lun. 03 juin 2019) $
  #  $Author: plecharpent $
  #  $Revision: 1429 $
  # ----------------------------------------------------------------------

  if (! is.null(usm_dir_names)) {
    run_dir=file.path(data_dir,usm_dir_names)
  } else {
    run_dir = data_dir
  }

  # testing id dirs exist
  dirs_exist <- file.exists(run_dir)

  if ( !all(dirs_exist) ) {
    print(paste0(run_dir[!dirs_exist],collapse = ", "))
    stop("One or more directories does/do not exist !")
  }

  if (!file.exists(model_path)){
    stop(paste("Executable file doesn't exist !",model_path))
  }
  # testing if the model is executable
  val <- try(system(paste(model_path,'--version'),intern = FALSE,ignore.stdout = TRUE), silent = TRUE)
  if (val != 0) {
    stop("The file is not executable or is not a Stics executable !")
  }

  nb_usms <- length(run_dir)
  usms_out <- vector("list", nb_usms)

  for (d in 1:nb_usms) {
    usm_out=list()
    usm_dir <- run_dir[d]
    usm_out$name=basename(usm_dir)

    setwd(usm_dir)
    usm_out$error <- try(system(model_path,ignore.stdout = TRUE,
                                ignore.stderr = TRUE),silent = TRUE)

    if (is.numeric(usm_out$error) & usm_out$error > 0) {
      usm_out$message="Model execution error !"
    }

    # If history file doesn't exist or all of the output files
    # don't exist: output error
    usm_chk_out=check_output_files(usm_dir)
    if (!usm_out$error & usm_chk_out$error) {
      print(paste("Error, missing output file(s) : ", usm_dir))
      usm_out$error=1
      usm_out$message=paste("No output files: ", usm_chk_out$missing)
    }

    usms_out[[d]] <- usm_out
  }

  return(invisible(usms_out))
}
