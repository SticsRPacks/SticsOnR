#' @title Select the Stics executable
#'
#' @description Select the Stics model executable to use from the preference file in JavaStics.
#'
#' @param javastics_path Path to the JavaStics installation directory
#' @param stics_exe      Stics executable name (see details)
#'
#' @details The `stics_exe` is **not** the name of the executable file, but the
#' identification name. Please use `list_stics_exe()` to list all available executables,
#' and `set_stics_exe()` to add and select a new one. The identification names can be retreived using
#' `names(list_stics_exe(javastics_path)$stics_list)`
#'
#' @note "stics_modulo", "sticsmodulo" and "modulostics" are synonyms for the standard STICS executable.
#'
#' @return Nothing. Update the "preference.xml" file in the config of JavaStics.
#'
#' @examples
#' \dontrun{
#' select_stics_exe("path/to/JavaSTICS-v131-stics-v841", "modulostics")
#'}
#'
#' @export
select_stics_exe <- function(javastics_path, stics_exe= "stics_modulo"){
  # checking javastics path
  check_java_path(javastics_path)

  if(stics_exe=="stics_modulo"|stics_exe=="sticsmodulo"){
    stics_exe= "modulostics"
  }

  # if no preference have been set yet
  if(!exists_javastics_pref(javastics_path)){
    init_javastics_pref(javastics_path)
  }

  # If the executable does not exist yet in the preference file:
  if(!exist_stics_exe(javastics_path,stics_exe)){
    stop("The provided model name doesn't exist in this configuration : ",
         javastics_path,
         ".\n Add it before with `set_stics_exe()` function!")
  }

  xml_path= file.path(javastics_path,"config","preferences.xml")
  xml_path_ori= file.path(javastics_path,"config","preferences_ori.xml")
  xml_path_prev= file.path(javastics_path,"config","preferences_prev.xml")

  # saving original file
  if (!file.exists(xml_path_ori)) {
    file.copy(xml_path,xml_path_ori)
  }

  # saving a previous version
  file.copy(xml_path,xml_path_prev)

  xml_pref=SticsRFiles:::xmldocument(xml_path)
  current_model=SticsRFiles:::getValues(xml_pref,'//entry[@key="model.last"]')
  # no need to set the model
  if(current_model == stics_exe) {
    return(invisible())
  }
  # setting model exe
  SticsRFiles:::setValues(xml_pref,'//entry[@key="model.last"]',stics_exe)


  exe_path=file.path(javastics_path,"bin",
                     list_stics_exe(javastics_path)$stics_list[[stics_exe]])

  # saving modified file
  SticsRFiles:::saveXmlDoc(xml_pref,xml_path)
}


#' @title Check if a stics executable is available
#'
#' @description Checks if a stics model executable is available in JavaStics (in the
#' "preference.xml" file).
#'
#' @param javastics_path JavaStics installation root folder
#' @param stics_exe  Stics executable name (see details)
#'
#' @details The current model executable available in JavaStics can be listed using
#' `list_stics_exe()`.
#'
#' @examples
#' \dontrun{
#' exist_stics_exe("path/to/JavaSTICS-v131-stics-v841","stics_name")
#'}
#'
#' @return Existing status, logical
#'
#' @keywords internal
exist_stics_exe <- function(javastics_path,stics_exe){
  is.element(stics_exe,names(list_stics_exe(javastics_path)$stics_list))
}
