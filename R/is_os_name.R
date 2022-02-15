#' @title Testing OS name
#'
#' @description Returning if the given OS name is the system name
#'
#' @param os_tag_name OS name(s) (see os_names list), optional
#'
#' @examples
#'\dontrun{
#' os_list <- is_os_name()
#' is_os_name <- is_os_name("windows")
#'}
#'
#' @return TRUE if os_tag_name is the current system OS, FALSE otherwise; OS names list if os_tag_name not provided
#'
#' @keywords internal
#'
#'
#'
is_os_name <- function(os_tag_name=character()){

  if (utils::packageVersion("SticsOnR") > "0.2.2") {
    return(SticsRFiles:::is_os_name(os_tag_name = os_tag_name))
  }

  os_names=c("windows","linux","mac","darwin")
  if (length(os_tag_name)==0) return(os_names)
  is_os_name=FALSE
  os_name=tolower(Sys.info()["sysname"])
  if (is.element(os_name,os_names) && any(is.element(os_tag_name,os_name))) is_os_name=TRUE

  # Storing the OS name as name attribute value
  attr(is_os_name, "name") <- os_name
  return(is_os_name)
}


#' Evaluating if the OS is a unix like type
#'
#' @return TRUE/FALSE
#' @export
#'
#' @examples
#' is_unix()
is_unix <- function() {

  if (utils::packageVersion("SticsOnR") > "0.2.2") {
    return(SticsRFiles::is_unix())
  }

  is_os_name(os_tag_name = "linux")
}

#' Evaluating if the OS is a windows type
#'
#' @return TRUE/FALSE
#' @export
#'
#' @examples
#' is_windows()
is_windows <- function() {
  if (utils::packageVersion("SticsOnR") > "0.2.2") {
    return(SticsRFiles::is_windows())
  }

  is_os_name(os_tag_name = "windows")
}

#' Evaluating if the OS is a Mac OS type
#'
#' @return TRUE/FALSE
#' @export
#'
#' @examples
#' is_mac()
is_mac <- function() {
  if (utils::packageVersion("SticsOnR") > "0.2.2") {
    return(SticsRFiles::is_mac())
  }
  is_os_name(os_tag_name = c("mac","darwin"))
}

is_unknown_os <- function() {
  if (utils::packageVersion("SticsOnR") > "0.2.2") {
    return(SticsRFiles:::is_unknown_os())
  }

  !is_os_name( os_tag_name = is_os_name() )
}

user_os <- function(){

  if (utils::packageVersion("SticsOnR") > "0.2.2") {
    return(SticsRFiles:::user_os())
  }

  if(is_unix()){
    "lin"
  }else if(is_windows()){
    "win"
  }else if(is_mac()){
    "mac"
  }else{
    "unknown"
  }
}

os_suffix <- function(){
  if (utils::packageVersion("SticsOnR") > "0.2.2") {
    return(SticsRFiles:::os_suffix())
  }

  if(is_unix()){
    ""
  }else if(is_windows()){
    ".exe"
  }else if(is_mac()){
    "_mac"
  }else{
    "unknown"
  }
}

