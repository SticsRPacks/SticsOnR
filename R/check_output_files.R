check_output_files <- function(run_dir,usm_name=NULL) {
  #' @title Checking model outputs files
  #'
  #' @description This function checks if the model outputs files are correctly produced
  #'
  #' @param run_dir Directory name or path (absolute) of a working directory,
  #'  or root directory of working directories
  #' @param usm_name Usm name, to be set if it is the name of a sub-directory of the given argument run_dir
  #'
  #' @return A list with fields $error (logical, TRUE if any missing file, FALSE otherwise), $missing (missing files vector)
  #'
  #' @examples
  #' check_output_files("/home/username/Work/SticsInputsDir")
  #' check_output_files("/home/username/Work/SticsInputsRootDir","usmDir")
  #'
  #' @export
  #'
  # ----------------------------------------------------------------------
  #  MODIFICATIONS (last commit)
  #  $Date: 2018-04-10 14:51:40 +0200 (mar. 10 avril 2018) $
  #  $Author: plecharpent $
  #  $Revision: 1154 $
  # ----------------------------------------------------------------------
  if (is.null(usm_name)) {
    usm=basename(run_dir)
  } else {
    usm=usm_name
  }
  files_list=c("modhistory.sti",paste0("mod_b",usm,".sti"),
               "mod_rapport.sti",paste0("mod_s",usm,".sti"))
  no_hist=!file.exists(file.path(run_dir,files_list[1]))
  no_balance=!file.exists(file.path(run_dir,files_list[2]))
  no_report=!file.exists(file.path(run_dir,files_list[3]))
  no_daily=file.exists(file.path(run_dir,files_list[4]))
  no_files = c(no_hist,no_balance,no_report,no_daily)
  out = list()
  out$error=no_hist | (no_balance & no_daily & no_report)
  out$missing=files_list[no_files]

  return(out)
}
