check_java_wd <- function(workspace_path){
  #' @title Checking JavaStics directory content
  #'
  #' @description Checking if directory exists and if it contains xml files for usms
  #' @details Rising an exeption for each checking step !
  #' @param workspace_path Workspace path
  #'
  #' @examples
  #'check_java_workspace("/home/plecharpent/Work/JavaSTICS-v131-stics-v841/example")
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

  if (!file.exists(workspace_path)) {
    stop("The workspace folder doesn't exist : ",workspace_path)
  }

  # checking if it's a workspace directory: only usms.xml
  if (!file.exists(file.path(workspace_path,"usms.xml"))) {
  #    && !file.exists(file.path(javastics_path,"JavaStics.jar"))) {
    stop("This directory is not a JavaStics workspace: ",javastics_path)
  }

}
