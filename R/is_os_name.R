is_os_name <- function(os_tag_name=character()){
  #' @title Testing OS name
  #'
  #' @description Returning if the given OS name is the system name
  #'
  #' @param os_tag_name OS name(s) (see os_names list), optional
  #'
  #' @examples
  #' os_list <- is_os_name()
  #' is_os_name <- is_os_name("windows")
  #'
  #' @return TRUE if os_tag_name is the current system OS, FALSE otherwise; OS names list if os_tag_name not provided
  #'
  #@export
  #'


  os_names=c("windows","linux","mac","darwin")
  if (length(os_tag_name)==0) return(os_names)
  is_os_name=FALSE
  os_name=tolower(Sys.info()["sysname"])
  if (is.element(os_name,os_names) && any(is.element(os_tag_name,os_name))) is_os_name=TRUE

  return(is_os_name)
}
