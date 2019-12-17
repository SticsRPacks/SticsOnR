#' Getting Stics versions list of executable in a JavaStics directory
#'
#' @param javastics_path JavaStics installation root folder
#' @param exe_tags Versions tags vector
#'
#' @return A named list ( with model tags ) of versions strings
#' (got from the model executable)
#'
#' @keywords internal
#'
#@export
#'
#@examples

get_java_models_version <- function(javastics_path, exe_tags = NULL) {

  # Getting models list from JavaStics preferences
  models_exe <- attr(get_java_models_nb(javastics_path),"exe")

  # Selecting models from exe_tags
  models_idx <- rep(TRUE,length(models_exe))
  if (! is.null(exe_tags) ) {
    models_idx <- names(models_exe) %in% exe_tags
  }
  if (! any(models_idx)) {
    tags <- sprintf("%s\n",exe_tags)
    warning(paste("Not any model executables correponding to tags :",
                  tags, collapse = "\n"))
    return(list())
  }
  models_exe <- models_exe[models_idx]

  # Getting versions from the model executables
  models_version <- lapply(models_exe, function(x)
    check_stics(x, version = TRUE, stop = FALSE))

  # Retrieving versions strings list
  versions_list <- lapply(models_version, function(x) { attr(x,"version")})

  # Keeping only functionnal versions (i.e. no error exexuting exe with --version arg)
  versions_list <- versions_list[! unlist(models_version)]

  # List of version strings
  return(versions_list)

}
