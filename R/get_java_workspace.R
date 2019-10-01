get_java_wd <- function(javastics_path){
  #' @title Getting current workspace
  #'
  #' @description Getting current javastics current working directory, if not any setting to `example` directory
  #'
  #' @param javastics_path JavaStics installation root folder
  #'
  #' @examples
  #' get_java_wd("/home/plecharpent/Work/JavaSTICS-v131-stics-v841")
  #'
  #' @export
  #'
  # ----------------------------------------------------------------------
  #  MODIFICATIONS (last commit)
  #  $Date: 2017-05-13 08:32:22 +0200 (sam. 13 mai 2017) $
  #  $Author: plecharpent $
  #  $Revision: 948 $
  # ----------------------------------------------------------------------

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
