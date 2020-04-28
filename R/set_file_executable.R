#' Setting a file to executable access mode
#'
#' @description Setting executable status on for a file under Linux or Mac OS.
#'
#' @param file_path A file path
#'
#' @return Execution status: TRUE for successfull operation, FALSE otherwise
#'
#' @keywords internal
#'
#'
set_file_executable <- function(file_path) {

  if(is_windows()){
    return(invisible(TRUE))
  }

  # The file does not exist
  if(!file.exists(file_path)){
    warning(paste("File does no exist:", file_path))
    return(invisible(FALSE))
  }

  # if OS != windows, set chmod +x exe
  ret <- suppressWarnings(system(paste("chmod +x",file_path),
                intern = TRUE,
                ignore.stdout = FALSE,
                ignore.stderr = FALSE))

  # Checking if any errors
  status <- attr(ret,"status")
  if(!base::is.null(status) && status){
    warning(paste("A problem occurs when setting executable status on:", file_path))
    return(invisible(FALSE))
  }

  return(invisible(TRUE))
}
