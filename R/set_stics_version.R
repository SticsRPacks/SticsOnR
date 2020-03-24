
#' Change the version of stics
#'
#' @description Set the version of stics used by JavaStics. The versions available
#' can be listed using `list_stics_versions()`.
#'
#' @note To add a new version, put the executable in the bin directory of JavaStics, then
#' open JavaStics, press `ctrl+M`, go to the "Add" tab and add the new version.
#'
#' @param javastics_path Path to the JavaStics installation directory
#' @param stics The name of the stics version to use (as given in the
#' "Stics model selection" tab) from JavaStics
#'
#' @return Nothing. Modifies the `preferences.xml` in the config.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  set_stics_version("path/to/JavaSTICS-1.41-stics-9.1", "stics_intercrop")
#' }
set_stics_version= function(javastics_path,stics= "stics_modulo"){
  if(stics=="stics_modulo"){
    stics= "modulostics"
  }
  if(stics!=list_stics_versions(javastics)$current){
    stics_list_names= names(list_stics_versions(javastics)$stics_list)
    if(stics!="stics_modulo"){
      if(!stics%in%stics_list_names){
        stop("The required stics version is not available yet, please add it using JavaStics",
             "The version(s) available for the moment is (are): ",paste(stics_list_names, collapse= ", "))
      }
    }
    prefs= file.path(javastics_path,'config','preferences.xml')
    xml_docs <- SticsRFiles:::xmldocument(prefs)
    keys_names= SticsRFiles:::getAttrsValues(xml_docs, "//entry","key")
    keys_values= SticsRFiles:::getValues(xml_docs, "//entry")
    keys_values[grep("model.last",keys_names)]= stics
    SticsRFiles:::setValues(xml_docs, "//entry", values_list = keys_values)
    SticsRFiles:::saveXmlDoc(xml_docs, prefs)
  }
}

