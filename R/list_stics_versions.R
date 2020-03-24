
#' List stics versions
#'
#' @description Return all stics versions names and executable listed for use
#'
#' @param javastics_path Path to the JavaStics installation directory
#'
#' @details The standard versins shipped with JavaStics is called modulostics and its
#' associated executable is named `stics_modulo.exe`.
#'
#' @return A list of two:
#' - stics_list: named list of the stics executable
#' - current: the version currently in use
#'
#' @export
#'
#' @examples
#' \dontrun{
#' list_stics_versions("path/to/JavaSTICS-1.41-stics-9.1")
#' }
list_stics_versions= function(javastics_path){
  prefs= file.path(javastics_path,'config','preferences.xml')

  # If the preferences file does not exist, it means JavaStics uses the default one:
  if(!file.exists(prefs)){
    return(list(stics_list= list(modulostics= "stics_modulo.exe"),
                current= list(modulostics= "stics_modulo.exe")))
  }

  xml_docs <- SticsRFiles:::xmldocument(prefs)
  keys_names= SticsRFiles:::getAttrsValues(xml_docs, "//entry","key")
  keys_values= SticsRFiles:::getValues(xml_docs, "//entry")

  current_stics= keys_values[grep("model.last",keys_names)]
  stics_list= keys_values[grep("model.list",keys_names)]
  stics_list_parsed= gsub("\\{|\\}","",stics_list)%>%strsplit(.,",")%>%unlist()

  stics_list_parsed= gsub("\\{|\\}","",stics_list)%>%strsplit(.,",|\t")%>%unlist()
  stics_list_names= stics_list_parsed[(1:length(stics_list_parsed))%%2==1]
  stics_list= as.list(stics_list_parsed[(1:length(stics_list_parsed))%%2==0])
  names(stics_list)= stics_list_names
  list(stics_list= stics_list, current= current_stics)
}
