exists_java_pref <- function(javastics_path){
  #' @title Evaluate if model preferences hav been set
  #'
  #' @description Testing if preferences.xml file exist in JavaSTICS installation folder
  #'
  #' @param javastics_path JavaStics installation root folder
  #'
  #' @examples
  #' \dontrun{
  #' exists_pref <- exists_java_pref("/home/plecharpent/Work/JavaSTICS-v131-stics-v841")
  #'}
  #'
  #' @return logical value, TRUE if file exists, FALSE otherwise
  #'
  #' @keywords internal
  #@export



  # checking javastics path
  check_java_path(javastics_path)

  # Returning if file exists
  return(file.exists(file.path(javastics_path,"config","preferences.xml")))

}
