#' @title Check if a stics executable is available
#'
#' @description Checks if a stics model executable is available in JavaStics (in the
#' "preference.xml" file).
#'
#' @param javastics_path JavaStics installation root folder
#' @param stics_exe  Stics executable name (see details)
#'
#' @details The current model executable available in JavaStics can be listed using
#' `list_stics_exe()`.
#'
#' @examples
#' \dontrun{
#' exist_stics_exe("path/to/JavaSTICS-v131-stics-v841","stics_name")
#'}
#'
#' @return Existing status, logical
#'
#' @keywords internal
exist_stics_exe <- function(javastics_path,stics_exe){
    is.element(stics_exe,names(list_stics_exe(javastics_path)$stics_list))
}
