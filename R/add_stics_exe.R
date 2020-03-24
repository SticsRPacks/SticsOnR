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
#' @param stics_exe      Stics executable name/identifier (not executable file)
#' @param exe            Stics executable name or path
#'
#' @examples
#' \dontrun{
#'  add_stics_exe("/path/to/JavaSTICS/dir","model_name","model_exe_name")
#' }
#'
#' @export
add_stics_exe <- function(javastics_path, stics_exe, exe) {

  # checking javastics path
  check_java_path(javastics_path)

  if (exist_stics_exe(javastics_path, stics_exe)) {
    warning("The model name already exists,
            selecting it in this configuration: ", javastics_path)
    set_stics_exe(javastics_path, stics_exe)
    return()
  }

  exe_name <- basename(exe)
  java_exe_path <- file.path(javastics_path, "bin", exe_name)

  if (!file.exists(exe) & !file.exists(java_exe_path)) {
    stop("The model executable file doesn't exist : ", exe)
  }

  if (!file.exists(java_exe_path)) {
    file.copy(exe, java_exe_path)
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

  stics_exe <- list_stics_exe(javastics_path)
  nb_models <- length(stics_exe$stics_list)

  stics_exe$stics_list[nb_models + 1]= exe_name
  names(stics_exe$stics_list)[nb_models + 1]= stics_exe

  # writing models list string
  stics_exe_string= paste0(sprintf("{%s\t%s},", names(stics_exe$stics_list), stics_exe$stics_list), collapse = "")

  xml_pref <- SticsRFiles:::xmldocument(xml_path)
  SticsRFiles:::setValues(xml_pref, '//entry[@key="model.last"]', stics_exe)
  SticsRFiles:::setValues(xml_pref, '//entry[@key="model.list"]', stics_exe_string)

  # writing file
  SticsRFiles:::saveXmlDoc(xml_pref, xml_path)

  # if OS != windows, set chmod +x exe
  if (!is_windows()) {
    system(paste("chmod +x", java_exe_path))
  }
}
