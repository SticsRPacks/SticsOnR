#' @title List stics executables available in JavaStics
#'
#' @description Return all stics identifier names and executable available in JavaStics
#'
#' @param javastics_path Path to the JavaStics installation directory
#'
#' @return A list of two:
#' - stics_list: named list of the stics executable
#' - current: a named list of the executable currently in use
#'
#' @details The information is read from the `preference.xml` file in JavaStics. The function first
#' controls that it exists and is complete, and if not it creates it using an OS-specific version.
#'
#' @examples
#' \dontrun{
#'  list_stics_exe("path/to/JavaSTICS-v131-stics-v841")
#' }
#'
#' @importFrom magrittr "%>%"
#'
#' @keywords internal
list_stics_exe <- function(javastics_path){

  # checking javastics path
  check_java_path(javastics_path)

  # If the preferences file does not exist, or is incomplete, it means JavaStics was
  # never used before. So we have to use a template for the preferences.
  is_pref= exists_javastics_pref(javastics_path)

  config_pref= file.path(javastics_path,"config","preferences.xml")

  if(!is_pref){
    cli::cli_alert_info("Preference file not found in {.code javastics_path}.")
    init_javastics_pref(javastics_path, overwrite = FALSE)
  }else{
    # If the preferences is availabble, control that it is complete (it is not on at install)
    xml_pref= SticsRFiles:::xmldocument(config_pref)
    current_stics= SticsRFiles:::getValues(xml_pref,'//entry[@key="model.last"]')
    if(is.null(current_stics)){
      cli::cli_alert_info("Preference file in {.code javastics_path} was found incomplete.")
      init_javastics_pref(javastics_path, overwrite = TRUE)
    }
  }

  xml_pref= SticsRFiles:::xmldocument(config_pref)
  current_stics= SticsRFiles:::getValues(xml_pref,'//entry[@key="model.last"]')

  stics_list= SticsRFiles:::getValues(xml_pref,'//entry[@key="model.list"]')
  stics_list_parsed= gsub("\\{|\\}","",stics_list)%>%strsplit(",|\t")%>%unlist()
  stics_list_names= stics_list_parsed[(1:length(stics_list_parsed))%%2==1]
  stics_list= as.list(stics_list_parsed[(1:length(stics_list_parsed))%%2==0])
  names(stics_list)= stics_list_names
  list(stics_list= stics_list, current= stics_list[current_stics])
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



