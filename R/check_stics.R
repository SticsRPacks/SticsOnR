#' Checking if given path is a Stics executable path
#'
#' @param model_path Model executable path
#' @param version Logical, or getting system command return i.e.
#' model version or not (default)
#' @param stop Logical for stopping or not execution
#'
#' @return System output (error,...)
#'
#' @export
#'
# @examples
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
  # catching returned error status
  err_status <- run_system_cmd(model_path, args='--version', output = version)

  # exiting if any error
  if ( err_status && stop ) {
    stop("The file is not executable or is not a Stics executable !")
  }

  # If version is required
  if ( version ) {
    # attaching the version attribute & removing the output one
    attr(err_status,"version") <- gsub(pattern = "Modulostics version : ",
                                       x = trimws(attr(err_status,"output")),
                                       replacement = "")
    attr(err_status, "output") <- NULL
  }

  return(invisible(err_status))
}
