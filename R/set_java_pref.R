#' @title Setting default preferences
#'
#' @description According to system detection generating a default preference file for a OS specific executable
#'
#' @param javastics_path JavaStics installation root folder
#'
#' @examples
#'\dontrun{
#' current_model <- set_java_pref("/home/plecharpent/Work/JavaSTICS-v131-stics-v841")
#'}
#'
#' @return Current Stics model name used in JavaStics default configuration
#'
#' @keywords internal
#'
#@export

set_java_pref <- function(javastics_path){

  # checking javastics path
  check_java_path(javastics_path)

  # Checking if file exists
  xml_path=file.path(javastics_path,"config","preferences.xml")

  if (!file.exists(xml_path)){
    pref_path=file.path(javastics_path,"bin","resources","prefs")

    # Getting a copy the right preferences file to the config directory
    os_unknown <- is_unknown_os()
    if ( os_unknown ) {
      stop(paste("Unknown system:",attr(os_unknown, "name"),"\n unable to configure default model selection !"))
    }

    if ( is_unix() ) pref_name="preferences_lin.xml"

    if ( is_mac() ) pref_name="preferences_mac.xml"

    if ( is_windows() ) pref_name="preferences_win.xml"

    # Copying a default pref file specific to OS name
    file.copy(file.path(pref_path,pref_name),xml_path)
  }

  xml_pref=SticsRFiles:::xmldocument(xml_path)
  current_model=SticsRFiles:::getValues(xml_pref,'//entry[@key="model.last"]')

  return(invisible(current_model))


}
