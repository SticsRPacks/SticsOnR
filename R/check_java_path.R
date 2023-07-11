#' @title Checking JavaStics directory content
#'
#' @description Checking if directory exists and
#' if it contains JavaStics jar files
#' @details Rising an exception for each checking step !
#' @param javastics_path JavaStics installation root folder
#'
#' @examples
#' \dontrun{
#' check_java_path("/path/to/javastics")
#' }
#'
#' @keywords internal
#'
#' @noRd
#'


check_java_path <- function(javastics_path) {

  # For keeping backward compatibility
  if (utils::packageVersion("SticsOnR") > "0.2.2") {
    return(SticsRFiles:::check_java_path(javastics = javastics_path))
  }


  if (!file.exists(javastics_path)) {
    stop("The JavasStics folder doesn't exist : ", javastics_path)
  }

  # checking if it's a JavaStics root directory
  if (!file.exists(file.path(javastics_path, "JavaStics.exe")) &&
    !file.exists(file.path(javastics_path, "JavaStics.jar"))) {
    stop("This directory is not a JavaStics one: ", javastics_path)
  }
}
