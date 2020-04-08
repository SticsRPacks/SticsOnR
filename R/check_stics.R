#' Checking if given path is a Stics executable path
#'
#' @param model_path Model executable path
#' @param version Logical, or getting system command return i.e.
#' model version or not (default)
#' @param stop Logical for stopping or not execution
#'
#' @return System output (error,...)
#'
#@export
#'
# @examples
#' @keywords internal
#'
check_stics <- function(model_path, version = FALSE, stop = TRUE) {

  # TODO:
  #  - evaluate if usefull to manage multiple checks
  #  - how to manage messages and stop for multiple exe checks
  # For checking multiple executables
  # if ( length(model_path) > 1 ) {
  #   err_status <- lapply( model_path,
  #                         function(x) check_stics(x, version = version))
  #   return(invisible(err_status))
  # }

  if (!file.exists(model_path)  && stop ){
    stop(paste("Executable file doesn't exist !",model_path))
  }

  # Setting executable status if needed (linux, Mac)
  if (! set_file_executable(model_path)) {
    stop(paste("Executable file is not runnable: ", model_path))
  }

  # making a copy for checking version
  start_dir <- getwd()
  tmp_model_dir <- tempdir()
  exe <- basename(model_path)
  file.copy(model_path,tmp_model_dir)

  # changing to dir tmp_model_dir
  setwd(tmp_model_dir)

  # catching returned error status
  err_status <- suppressWarnings(run_system_cmd(paste0("./",exe), com_args='--version', output = version))

  # exiting if any error
  if ( err_status && stop ) {
    stop(paste("The file is not executable or is not a Stics executable: \n",
               model_path))
  }

  # If version is required
  if ( version ) {
    # attaching the version attribute & removing the output one
    attr(err_status,"version") <- gsub(pattern = "Modulostics version : ",
                                       x = trimws(attr(err_status,"output")),
                                       replacement = "")
    attr(err_status, "output") <- NULL
  }

  setwd(start_dir)

  return(invisible(err_status))
}
