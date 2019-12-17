#' @title Running usm(s) from a/several directory(ies)
#' or a/several subdirectory(ies) named with usm name
#'
#' @description This function uses Stics directly through a system call
#'
#' @param model_path Path of Stics executable file
#' @param data_dir Path of a Stics input directory or a vector of,
#' or root directory of Stics directories (if usm_dir_names is given)
#' @param usm_dir_names Name(s) of sub-directory(ies) of data_dir
#' @param check_exe Logical, T for checking the model executable, F otherwise
#'
#' @return A list with usm names and execution error status
#'
#' @examples
#' \dontrun{
#' run_stics("/home/username/bin/Stics","/home/username/Work/SticsInputsDir")
#' run_stics("/home/username/bin/Stics",c("/home/username/Work/SticsInputsDir1",
#' "/home/username/Work/SticsInputsDir2"))
#'
#' run_stics("/home/username/bin/Stics","/home/username/Work/SticsInputsRootDir","wheat")
#' run_stics("/home/username/bin/Stics","/home/username/Work/SticsInputsRootDir",
#' c("wheat","maize"))
#'}
#'
#' @export

run_stics <- function(model_path,data_dir,usm_dir_names=NULL,
                       check_exe = TRUE) {

  usms_out <- run_system(model_path, data_dir, usm_dir_names = usm_dir_names,
                         check_exe = check_exe)
  return(invisible(usms_out))
}
