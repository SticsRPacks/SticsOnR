#' @title Remove a Stics model executable from JavaStics
#'
#' @description Remove a stics model from the list of available model executables in
#' JavaStics (modifies the "preferences.xml" file).
#'
#'
#' @param javastics_path Path to the JavaStics installation directory
#' @param stics_exe      Stics executable identifier name (see details)
#'
#' @details The executable file in the current JavaStics bin folder will not be deleted,
#' please do it by hand instead (in "JavaStics.../bin").
#' The `stics_exe` is **not** the name of the executable file, but the
#' identification name. Please use `list_stics_exe()` to list all available executables,
#' and `add_stics_exe()` to add a new one. The identification names can be retreived using
#' `names(list_stics_exe(javastics_path)$stics_list)`
#'
#' @return Nothing. Update the "preference.xml" file in the config of JavaStics.
#'
#' @examples
#' \dontrun{
#' remove_stics_exe("path/to/JavaSTICS-v131-stics-v841","model_name")
#'}
#'
#' @export
remove_stics_exe <- function(javastics_path,stics_exe){

  # checking javastics path
  check_java_path(javastics_path)

  if (!exist_stics_exe(javastics_path,stics_exe)) {
    warning("The model doesn't exists or his name is miss spelled : ",stics_exe,
            ".\n Call names(list_stics_exe(javastics_path)$stics_list) to list existing executables")
    return()
  }

  # exe_name=basename(java_model_exe)
  # java_exe_path=file.path(javastics_path,"bin",exe_name)
  #
  # if (!file.exists(java_model_exe) & !file.exists(java_exe_path)){
  #   stop("The model executable file doesn't exist : ",java_model_exe)
  # }


  xml_path=file.path(javastics_path,"config","preferences.xml")
  xml_path_ori=file.path(javastics_path,"config","preferences_ori.xml")
  xml_path_prev=file.path(javastics_path,"config","preferences_prev.xml")

  # saving original file
  if (!file.exists(xml_path_ori)) {
    file.copy(xml_path,xml_path_ori)
  }

  # saving a previous version
  file.copy(xml_path,xml_path_prev)

  stics_exe <- list_stics_exe(javastics_path)
  nb_models <- length(stics_exe$stics_list)

  # Remove the model version:
  stics_exe$stics_list= stics_exe$stics_list[-grep(stics_exe,names(stics_exe$stics_list))]

  # writing models list string
  stics_exe_string= paste0(sprintf("{%s\t%s},", names(stics_exe$stics_list), stics_exe$stics_list), collapse = "")

  xml_pref= SticsRFiles ::: xmldocument(xml_path)

  # removing model from last if needed
  if(stics_exe$current == stics_exe){
    warning("JavaStics was using this Stics executable currently",
            " Please set a new model executable to use using `set_stics_exe()`")
    SticsRFiles:::setValues(xml_pref,'//entry[@key="model.last"]',"")
  }

  SticsRFiles ::: setValues(xml_pref,'//entry[@key="model.list"]',java_models_string)

  # writing file
  SticsRFiles ::: saveXmlDoc(xml_pref,xml_path)
}
