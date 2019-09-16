set_java_wd <- function(javastics_path,java_wd){
  #' @title Setting JavaStics workspace
  #'
  #' @description Setting a new JavaStics working directory, a relative directory to JavaStics path or an absolute one
  #'
  #' @details Checking if the directory is a JavaStics workspace (any usms.xml file), and if it's already registered
  #' @details before setting new one
  #' @param javastics_path JavaStics installation root folder
  #' @param java_wd JavaStics working directory (absolute,relative to javastics_path)
  #'
  #' @examples
  #' set_java_wd("/home/plecharpent/Work/JavaSTICS-v131-stics-v841","my_wd")
  #' set_java_wd("/home/plecharpent/Work/JavaSTICS-v131-stics-v841","/path/to/my_wd")
  #'
  #'
  #' @export
  #'
  # ----------------------------------------------------------------------
  #  MODIFICATIONS (last commit)
  #  $Date: 2017-05-16 13:36:19 +0200 (mar. 16 mai 2017) $
  #  $Author: plecharpent $
  #  $Revision: 953 $
  # ----------------------------------------------------------------------

  # checking javastics path
  check_java_path(javastics_path)

  # if no preference have been set yet
  if (!exists_java_pref(javastics_path)) set_java_pref(javastics_path)

  xml_path=file.path(javastics_path,"config","preferences.xml")

  xml_pref=xmldocument(xml_path)

  # checking if java_wd is a relative to javaStics path or an absolute one exists
  if(dirname(java_wd)==".") {
    java_wd=file.path(javastics_path,java_wd)
  }

  # checking if it's a workspace
  check_java_wd(java_wd)

  # getting current registered wd
  current_wd=getValues(xml_pref,'//entry[@key="workingDirectory.current"]')

  # entry doesn't exist, normally it could not occur because we set pref file before,
  # but using JavaStics interface first doesn't fix a default workspace, so ...
  if (is.null(current_wd)){
    n=xmlParseString(paste0("<entry key=\"workingDirectory.current\">",java_wd,"</entry>"))
    addNodes(xml_pref,n)
  } else {
    # if it's not different from the new one,
    if (current_wd==java_wd || (dirname(java_wd)==javastics_path) && basename(java_wd)==current_wd) return()

    # else, setting entry value
    setValues(xml_pref,'//entry[@key="workingDirectory.current"]',java_wd)
  }

  # writing file
  saveXmlDoc(xml_pref,xml_path)

}
