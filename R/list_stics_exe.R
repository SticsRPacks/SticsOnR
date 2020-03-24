#' @title List stics executables available in JavaStics
#'
#' @description Return all stics identifier names and executable available in JavaStics
#'
#' @param javastics_path Path to the JavaStics installation directory
#'
#' @return A list of two:
#' - stics_list: named list of the stics executable
#' - current: the executable currently in use
#'
#' @examples
#' \dontrun{
#'  list_stics_exe("path/to/JavaSTICS-v131-stics-v841")
#' }
#'
#' @importFrom magrittr "%>%"
#'
#' @export
list_stics_exe <- function(javastics_path){

  # checking javastics path
  check_java_path(javastics_path)

  # If the preferences file does not exist, it means JavaStics uses the default one:
  if(!exists_javastics_pref(javastics_path)){
    #warning("No model have been selected in JavaStics, add model first !")
    if(is_windows()){
      models= list(stics_list= list(modulostics= "stics_modulo.exe"),
                   current= list(modulostics= "stics_modulo.exe"))
    }else if(is_unix()){
      models= list(stics_list= list(modulostics_linux= "stics_modulo"),
                   current= list(modulostics_linux= "stics_modulo"))
    }else if(is_mac()){
      models= list(stics_list= list(modulostics_mac= "stics_modulo_mac"),
                   current= list(modulostics_mac= "stics_modulo_mac"))
    }else{
      stop("Can't set the stics model executable: unable to recognize the operating system")
    }
    return(models)
  }

  xml_pref= SticsRFiles:::xmldocument(file.path(javastics_path,"config","preferences.xml"))
  current_stics= SticsRFiles:::getValues(xml_pref,'//entry[@key="model.last"]')

  stics_list= SticsRFiles:::getValues(xml_pref,'//entry[@key="model.list"]')
  stics_list_parsed= gsub("\\{|\\}","",stics_list)%>%strsplit(",|\t")%>%unlist()
  stics_list_names= stics_list_parsed[(1:length(stics_list_parsed))%%2==1]
  stics_list= as.list(stics_list_parsed[(1:length(stics_list_parsed))%%2==0])
  names(stics_list)= stics_list_names
  list(stics_list= stics_list, current= current_stics)
}


#' @title Evaluate if model preferences have been set
#'
#' @description Testing if preferences.xml file exist in JavaSTICS installation folder
#'
#' @param javastics_path JavaStics installation folder
#'
#' @examples
#' \dontrun{
#' exists_pref <- exists_javastics_pref("path/to/JavaSTICS-v131-stics-v841")
#'}
#'
#' @return logical value, TRUE if file exists, FALSE otherwise
#'
#' @keywords internal
exists_javastics_pref <- function(javastics_path){
  # checking javastics path
  check_java_path(javastics_path)

  # Returning if file exists
  return(file.exists(file.path(javastics_path,"config","preferences.xml")))
}

