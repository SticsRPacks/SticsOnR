#' Checking if given path in a Stics executable
#'
#' @param model_path Model executable path
#'
#' @return System output (error,...)
#' @export
#'
# @examples
check_stics <- function(model_path) {

  if (!file.exists(model_path)){
    stop(paste("Executable file doesn't exist !",model_path))
  }
  # # testing if the model is executable
  # #val <- try(system(paste(model_path,'--version'),intern = FALSE,ignore.stdout = FALSE), silent = TRUE)
  # val2 <- try(system2( command = model_path, args = '--version',
  #                      stderr = TRUE,
  #                      stdout = FALSE),
  #             silent = TRUE)
  # #print(val2)
  #
  # #if (val != 0) {
  # if (length(val2) != 0) {
  #   stop("The file is not executable or is not a Stics executable !")
  # }


  ret <- run_system_cmd(model_path, args='--version')

  #return(invisible(val2))

  if ( ! ret) {
    stop("The file is not executable or is not a Stics executable !")
  }


  return(invisible(ret))
}
