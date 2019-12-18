#' @title Selecting Stics model to use
#'
#' @description Setting in preferences the Stics model version to use
#'
#' @param javastics_path JavaStics installation root folder
#' @param java_model_tag Model name (not executable file)
#'
#' @examples
#' \dontrun{
#' set_java_model("/home/plecharpent/Work/JavaSTICS-v131-stics-v841","model_name")
#'}
#'
#' @keywords internal
#'
#@export

set_java_model <- function(javastics_path,java_model_tag){

  # checking javastics path
  check_java_path(javastics_path)

  # if no preference have been set yet
  if (!exists_java_pref(javastics_path)) set_java_pref(javastics_path)

  if (!exist_java_model(javastics_path,java_model_tag)) {
    stop("The provided model name doesn't exist in this configuration : ",javastics_path,".\n Add it before with add_java_model function!")
  }
  xml_path=file.path(javastics_path,"config","preferences.xml")
  xml_path_ori=file.path(javastics_path,"config","preferences_ori.xml")
  xml_path_prev=file.path(javastics_path,"config","preferences_prev.xml")

  # saving original file
  if (!file.exists(xml_path_ori)) {
    file.copy(xml_path,xml_path_ori)
  }

  # saving a previous version
  file.copy(xml_path,xml_path_prev)

  xml_pref=SticsRFiles:::xmldocument(xml_path)
  current_model=SticsRFiles:::getValues(xml_pref,'//entry[@key="model.last"]')
  # no need to set the model
  if (current_model == java_model_tag) {
    return(invisible())
  }
  # setting model version
  SticsRFiles:::setValues(xml_pref,'//entry[@key="model.last"]',java_model_tag)

  # if OS != windows, set chmod +x exe
  if (!is_os_name("windows")){
    exe_path=file.path(javastics_path,"bin",get_model_exe(javastics_path,java_model_tag))
    system(paste("chmod +x",exe_path))
  }

  # saving modified file
  SticsRFiles:::saveXmlDoc(xml_pref,xml_path)

}
