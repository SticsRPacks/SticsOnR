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
#' \dontrun{
#'  set_java_wd("path/to/JavaSTICS","my_wd")
#'  set_java_wd("path/to/JavaSTICS","/path/to/my_wd")
#' }
#'
#' @export
set_java_workspace <- function(javastics_path,java_wd){

  # checking javastics path
  check_java_path(javastics_path)

  # if no preference have been set yet
  if (!exists_java_pref(javastics_path)) set_java_pref(javastics_path)

  xml_path=file.path(javastics_path,"config","preferences.xml")

  xml_pref= SticsRFiles:::xmldocument(xml_path)

  # checking if java_wd is a relative to javaStics path or an absolute one exists
  if(dirname(java_wd)==".") {
    java_wd=file.path(javastics_path,java_wd)
  }

  # checking if exists if it is a workspace a
  ws <- check_java_workspace(javastics_path,java_wd)
  if (methods::is.null(ws)) {
    return()
  }

  # getting current registered wd
  current_wd= SticsRFiles:::getValues(xml_pref,'//entry[@key="workingDirectory.current"]')

  # entry doesn't exist, normally it could not occur because we set pref file before,
  # but using JavaStics interface first doesn't fix a default workspace, so ...
  if (methods::is.null(current_wd)){
    n= XML::xmlParseString(paste0("<entry key=\"workingDirectory.current\">",java_wd,"</entry>"))
    SticsRFiles:::addNodes(xml_pref,n)
  } else {
    # if it's not different from the new one,
    if (current_wd==java_wd || (dirname(java_wd)==javastics_path) && basename(java_wd)==current_wd) return()

    # else, setting entry value
    SticsRFiles:::setValues(xml_pref,'//entry[@key="workingDirectory.current"]',java_wd)
  }

  # writing file
  SticsRFiles:::saveXmlDoc(xml_pref,xml_path)

}
