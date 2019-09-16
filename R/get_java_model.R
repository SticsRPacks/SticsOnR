get_java_model <- function(javastics_path){
  #' @title Getting current Stics instance in use
  #'
  #' @description Extracting the last used model from preferences file
  #'
  #' @param javastics_path JavaStics installation root folder
  #'
  #' @examples
  #' current_model <- get_java_model("/home/plecharpent/Work/JavaSTICS-v131-stics-v841")
  #'
  #' @return Current Stics model name used in JavaStics configuration
  #'
  #' @export
  #'
  # ----------------------------------------------------------------------
  #  MODIFICATIONS (last commit)
  #  $Date: 2017-05-13 08:40:55 +0200 (sam. 13 mai 2017) $
  #  $Author: plecharpent $
  #  $Revision: 949 $
  # ----------------------------------------------------------------------

  # checking javastics path
  check_java_path(javastics_path)

  # if no preference have been set yet
  if (!exists_java_pref(javastics_path)) set_java_pref(javastics_path)

  xml_path=file.path(javastics_path,"config","preferences.xml")

  if(!file.exists(xml_path)){
    stop("No model has been selected yet, try set_java_model without arguments to fix default model version.")
  }

  xml_pref=xmldocument(xml_path)
  current_model=getValues(xml_pref,'//entry[@key="model.last"]')

  return(current_model)


}
