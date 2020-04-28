#' @title Adding a new Stics model executable in JavaStics
#'
#' @description Add a new executable of the Stics model to use in JavaStics, and set it as
#' the one to use by default. Please refer to `set_stics_exe()` to change the stics
#' executable used by JavaStics, and `list_stics_exe()` to list all available executables.
#'
#' @details The executable file may be in the current JavaStics bin folder. If not,
#' the `exe` argument is considered as a path and the executable will be copied in
#' the bin directory.
#'
#' @param javastics_path JavaStics installation root folder
#' @param stics_name      Stics executable name/identifier (not executable file)
#' @param exe_path       Stics executable file path
#'
#' @examples
#' \dontrun{
#'  add_stics_exe("/path/to/JavaSTICS/dir","model_name","model_exe_path")
#' }
#'
#' @export
add_stics_exe <- function(javastics_path, stics_name, exe_path) {

  # checking javastics path
  check_java_path(javastics_path)

  if(exist_stics_exe(javastics_path, stics_name)){
    warning("The model name already exists,
            selecting it in this configuration: ", javastics_path)
    set_stics_exe(javastics_path, stics_name)
    return()
  }

  exe_file_name <- basename(exe_path)
  java_exe_path <- file.path(javastics_path, "bin", exe_file_name)

  if (!file.exists(exe_path) & !file.exists(java_exe_path)) {
    stop("The model executable file doesn't exist : ", exe)
  }

  # Importing the new exe in the bin directory
  if (!file.exists(java_exe_path)) {
    file.copy(exe_path, java_exe_path)
  }

  xml_path <- file.path(javastics_path, "config", "preferences.xml")
  xml_path_ori <- file.path(javastics_path, "config", "preferences_ori.xml")
  xml_path_prev <- file.path(javastics_path, "config", "preferences_prev.xml")

  # saving original file
  if (!file.exists(xml_path_ori)) {
    file.copy(xml_path, xml_path_ori)
  }

  # saving a previous version
  file.copy(xml_path, xml_path_prev)

  # Getting the existing list in pref file
  stics_exe_list <- list_stics_exe(javastics_path)
  nb_models <- length(stics_exe_list$stics_list)

  # Adding the new exe in the list
  stics_exe_list$stics_list[nb_models + 1]= exe_file_name
  names(stics_exe_list$stics_list)[nb_models + 1]= stics_name

  # writing models list string
  # and setting the current used model withthe added one
  stics_exe_string= paste0(sprintf("{%s\t%s},", names(stics_exe_list$stics_list), stics_exe_list$stics_list), collapse = "")
  xml_pref <- SticsRFiles:::xmldocument(xml_path)
  SticsRFiles:::setValues(xml_pref, '//entry[@key="model.last"]', stics_name)
  SticsRFiles:::setValues(xml_pref, '//entry[@key="model.list"]', stics_exe_string)

  # writing file
  SticsRFiles:::saveXmlDoc(xml_pref, xml_path)

  # Setting exe_path to executable (OS != windows)
  # and checking if it is a Stics exe file
  check_stics_exe(java_exe_path)

}
