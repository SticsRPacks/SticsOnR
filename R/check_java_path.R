check_java_path <- function(javastics_path){
  #' @title Checking JavaStics directory content
  #'
  #' @description Checking if directory exists and if it contains JavaStics jar files
  #' @details Rising an exception for each checking step !
  #' @param javastics_path JavaStics installation root folder
  #'
  #' @examples
  #'check_java_path("/home/plecharpent/Work/JavaSTICS-v131-stics-v841")
  #'
  #'
  #' @export
  #'

  if (!file.exists(javastics_path)) {
    stop("The JavasStics folder doesn't exist : ",javastics_path)
  }

  # checking if it's a JavaStics root directory
  if (!file.exists(file.path(javastics_path,"JavaStics.exe")) &&
      !file.exists(file.path(javastics_path,"JavaStics.jar"))) {
    stop("This directory is not a JavaStics one: ",javastics_path)
  }

}
