#' @title Adding a new Stics model version to use in JavaStics configuration
#'
#' @description Creating a new entry in preferences with a new model name
#'  and executable file, and select it
#'
#' @details The executable file may be in the current JavaStics bin folder,
#' or elsewhere and will be copied in the bin directory
#'
#' @param javastics_path JavaStics installation root folder
#' @param java_model_tag Model name/identifier (not executable file)
#' @param java_model_exe Model executable name or path
#'
#' @examples
#' \dontrun{
#'  add_java_model("/path/to/JavaSTICS/dir",
#'  "model_name","model_exe_name")
#' }
#'
#@export

add_java_model <- function(javastics_path, java_model_tag, java_model_exe) {


  # checking javastics path
  check_java_path(javastics_path)

  if (exist_java_model(javastics_path, java_model_tag)) {
    warning("The model name already exists,
            selecting it in this configuration: ", javastics_path)
    set_java_model(javastics_path, java_model_tag)
    return()
  }

  exe_name <- basename(java_model_exe)
  java_exe_path <- file.path(javastics_path, "bin", exe_name)

  if (!file.exists(java_model_exe) & !file.exists(java_exe_path)) {
    stop("The model executable file doesn't exist : ", java_model_exe)
  }

  if (!file.exists(java_exe_path)) {
    file.copy(java_model_exe, java_exe_path)
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

  java_models <- get_java_models(javastics_path)
  nb_models <- length(java_models$tags)

  java_models$exe[nb_models + 1] <- exe_name
  java_models$tags[nb_models + 1] <- java_model_tag

  # writing models list string
  fmt <- "{%s\t%s},"
  java_models_string <- ""
  for (i in 1:(nb_models + 1)) {
    java_models_string <- paste0(java_models_string,
                                 sprintf(fmt, java_models$tags[i],
                                         java_models$exe[i]))
  }

  xml_pref <- SticsRFiles:::xmldocument(xml_path)
  SticsRFiles:::setValues(xml_pref, '//entry[@key="model.last"]', java_model_tag)

  SticsRFiles:::setValues(xml_pref, '//entry[@key="model.list"]', java_models_string)

  # writing file
  SticsRFiles:::saveXmlDoc(xml_pref, xml_path)

  # if OS != windows, set chmod +x exe
  if (!is_os_name("windows")) {
    system(paste("chmod +x", java_exe_path))
  }
}
