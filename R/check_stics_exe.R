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
check_stics_exe <- function(model_path, version = FALSE, stop = TRUE) {

  # Need to set the directory to the one of the exe for system calls
  start_dir <- getwd()
  on.exit(setwd(start_dir))

  # Check that file exist:
  if(!file.exists(model_path)){
    if(stop){
      stop(paste("Executable file doesn't exist: ",model_path))
    }else{
      cli::cli_alert_danger("Executable file does not exist: {.val {model_path}}")
    }
  }

  # Check that file is an executable for the user's OS:
  file_infos= file.info(model_path)$exe
  if(file_infos=="no"){
    if(stop){
      stop(paste("File",model_path,"is either not executable, or an exe for another OS."))
    }else{
      cli::cli_alert_danger("File {.val {model_path}} is either not executable, or an exe for another OS.")
    }
  }

  # Make the file executable if needed for linux or Mac
  if(!set_file_executable(model_path)){
    stop(paste("Executable file is not runnable: ", model_path))
  }

  # changing to dir tmp_model_dir
  setwd(dirname(model_path))

  # catching returned error status
  err_status <- suppressWarnings(run_system_cmd(basename(model_path), com_args='--version', output = version))

  # exiting if any error
  if(err_status && stop){
    stop(paste("The file is not executable or is not a Stics executable: \n",
               model_path))
  }

  # If version is required
  if(version){
    # attaching the version attribute & removing the output one
    attr(err_status,"version") <- gsub(pattern = "Modulostics version : ",
                                       x = trimws(attr(err_status,"output")),
                                       replacement = "")
    attr(err_status, "output") <- NULL
  }

  return(invisible(err_status))
}
