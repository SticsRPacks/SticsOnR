#' @title Checking and getting JavaStics workspace path
#'
#' @description Looking in JavaStics for a workspace path in JavasStics preferences
#' or producing a full path to a workspace located in JavaStics root directory
#' or validating an external absolute path. And also checking if the path is a
#' valid JavaStics workspace
#'
#' @param javastics_path Path of JavaStics installation directory
#' @param javastics_workspace_path An absolute or relative path (to JavaStics path)
#' of a workspace (Optional)
#'
#' @return An absolute javastics workspace path
#'
#' @keywords internal
#@export

check_java_workspace <- function(javastics_path,javastics_workspace_path=NULL) {

  # For keeping backward compatibility
  if (utils::packageVersion("SticsOnR") > "0.2.2") {
    SticsRFiles:::check_java_workspace(javastics_path = javastics_path,
                           javastics_workspace_path = javastics_workspace_path)
  } else {
    check_java_workspace(javastics_path = javastics_path,
                         javastics_workspace_path = javastics_workspace_path)
  }


  # Ensure that the user working directory is unchanged after the function has run
  current_wd= getwd()
  on.exit(setwd(current_wd))

  setwd(javastics_path)

  ws <- NULL

  if (! base :: is.null(javastics_workspace_path)){
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

  if (base::is.null(ws) || !dir.exists(ws)) {
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
