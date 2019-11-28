get_java_workspace <- function(javastics_path){
  #' @title Getting current workspace in JavaStics preferences configuration
  #'
  #' @description Getting current JavaStics working directory,
  #' if not any setting to `example` directory
  #'
  #' @param javastics_path JavaStics installation root folder
  #'
  #' @examples
  #' get_java_wd("/path/to/JavaStics/directory")
  #'
  #' @export
  #'


  # checking javastics path
  check_java_path(javastics_path)

  # if no preference have been set yet
  if (!exists_java_pref(javastics_path)) set_java_pref(javastics_path)

  xml_path=file.path(javastics_path,"config","preferences.xml")

  xml_pref=xmldocument(xml_path)
  current_wd=getValues(xml_pref,'//entry[@key="workingDirectory.current"]')

  if(is.null(current_wd)) stop("JavaStics working directory hasn't been set (use set_java_wd to do so)!")

  return(current_wd)

}
