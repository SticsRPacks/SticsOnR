#' @title Checking and getting JavaStics workspace path
#'
#' @description Looking in JavaStics for a workspace path in JavasStics preferences
#' or producing a full path to a workspace located in JavaStics root directory
#' or validating an external absolute path. And also checking if the path is a
#' valid JavaStics workspace
#'
#' @param javastics_path Path of JavaStics installation directory
#' @param javastics_workspace_path An absolute or relative path (to JavsStics path)
#' of a workspace (Optional)
#'
#' @return An absolute javastics workspace path
#'
#' @export
check_java_workspace <- function(javastics_path,javastics_workspace_path=NULL) {

  setwd(javastics_path)

  ws <- NULL

  if (! methods :: is.null(javastics_workspace_path)){
    if(dirname(javastics_workspace_path) == "."){
      # relative path to javastics path
      ws=file.path(javastics_path,javastics_workspace_path)
    } else {
      ws=javastics_workspace_path
    }
  } else {
    tt<-try(ws <- get_java_workspace(javastics_path),silent=TRUE)
    if (methods :: is(tt,"try-error")) {
      warning("No workspace directory has been set, use set_java_wd to do so, or \n give it as input of the function !")
      return()
    }
  }

  if (methods::is.null(ws) || !dir.exists(ws)) {
    warning(paste("The given directory does not exist or JavaStics working directory is not set :\n",ws))
    return()
  }

  # checking if it's a workspace directory: searching usms.xml
  if (!file.exists(file.path(ws,"usms.xml"))) {
    warning("This directory is not a JavaStics workspace: ",ws)
    return()
  }
  return(ws)

}
