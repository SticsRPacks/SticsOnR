#' Getting the number of Stics executable registered in JavaStics
#'
#' @param javastics_path JavaStics installation root folder
#' @param check Logical for executing or not a check on executables files
#'
#' @return a number of registered models in JavaStics preferences,
#' and corresponding exiseing executable file
#'
#' @keywords internal
#'
#@export
#'
#@examples

get_java_models_nb <- function(javastics_path, check = FALSE) {

  # Getting registered models info
  models_info <- get_java_models(javastics_path)
  models_exe <- models_info$exe
  models_tags <- models_info$tags

  # Checking existing executable files
  exe_paths <- file.path(javastics_path, "bin", models_exe)
  exist_exe <- file.exists(exe_paths)

  # Checking if not all exe exist
  if (! all(exist_exe) ) {
    exe_list <- paste(models_exe[! exist_exe], collapse = "\n")
    warning(paste("Some executable files do not exist : \n",
                  exe_list))
  }

  # TODO: to be activated
  # Perform checking of executables !
  # checks <- lapply(models_exe, function(x)
  # check_stics(x, version = TRUE, stop = FALSE))
  #
  # if ( ! all(checks) ) {
  #   warning()
  # }

  # adding executable paths with tags names as attribute
  exe_nb <- sum(exist_exe)
  names(exe_paths) <- models_tags
  attr(exe_nb, "exe") <- exe_paths[exist_exe]

  # Returning the effective number of executables ...
  return(exe_nb)
}
