#' Stics executable version
#'
#' @description Return the model version of each stics executable available in
#' JavaStics (using --version).
#' @param javastics_path JavaStics installation root folder
#' @param stics_exe      Stics executable name (see details)
#'
#' @details The `stics_exe` is **not** the name of the executable file, but the
#' identification name. Please use `list_stics_exe()` to list all available executables,
#' and `add_stics_exe()` to add a new one. The identification names can be retreived using
#' `names(list_stics_exe(javastics_path)$stics_list)`
#'
#' @return A named list with stics executables from the model executable
#'
#' @keywords internal
get_stics_models_exe <- function(javastics_path, stics_exe = NULL) {

  # Getting models list from JavaStics preferences
  models <- list_stics_exe(javastics_path)$stics_list
  if(!is.null(stics_exe)){
    models <- models[stics_exe]
  }
  models_exe= try(normalizePath(file.path(javastics_path,"bin", models), winslash = "/"))

  if(inherits(models_exe, "try-error")){
    stop("One or several stics executables listed in JavaStics is/are not found in the bin folder.",
         "Please check if the executable is there (you can list the exe using `list_stics_exe()`)")
  }

  # Getting versions from the model executables
  models_exe <-
    lapply(models_exe, function(x){
      attr(check_stics(x, version = TRUE, stop = FALSE),"version")
    })
  names(models_exe)= names(models)

  return(models_exe)
}
