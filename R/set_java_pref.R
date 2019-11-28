set_java_pref <- function(javastics_path){
  #' @title Setting default preferences
  #'
  #' @description According to system detection generating a default preference file for a OS specific executable
  #'
  #' @param javastics_path JavaStics installation root folder
  #'
  #' @examples
  #' current_model <- set_java_pref("/home/plecharpent/Work/JavaSTICS-v131-stics-v841")
  #'
  #' @return Current Stics model name used in JavaStics default configuration
  #'
  #' @export
  #'


  # checking javastics path
  check_java_path(javastics_path)

  # Checking if file exists
  xml_path=file.path(javastics_path,"config","preferences.xml")

  if (!file.exists(xml_path)){
    pref_path=file.path(javastics_path,"bin","resources","prefs")

    if (is_os_name("linux")){
      pref_name="preferences_lin.xml"
    } else if (is_os_name(c("mac","darwin"))) {
      pref_name="preferences_mac.xml"
    } else if (is_os_name("windows")){
      pref_name="preferences_win.xml"
    } else {
      stop("Unknown system, unable to configure default model selection !")
    }

    # Copying a default pref file specific to OS name
    file.copy(file.path(pref_path,pref_name),xml_path)
  }

  xml_pref=xmldocument(xml_path)
  current_model=getValues(xml_pref,'//entry[@key="model.last"]')

  return(invisible(current_model))


}
