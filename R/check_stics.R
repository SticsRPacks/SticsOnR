#' Checking if given path is a Stics executable path
#'
#' @param model_path Model executable path
#'
#' @return System output (error,...)
#'
#' @export
#'
# @examples
check_stics <- function(model_path) {

  if (!file.exists(model_path)){
    stop(paste("Executable file doesn't exist !",model_path))
  }
  # catching returned error status
  err_status <- run_system_cmd(model_path, args='--version')

  if ( err_status ) {
    stop("The file is not executable or is not a Stics executable !")
  }


  return(invisible(err_status))
}
