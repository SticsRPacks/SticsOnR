#' Run a system command
#'
#' @param command A system command
#' @param com_args A list of arguments to pass to the command
#' @param output A logical value to specify returning (T) or not (F, default)
#' the command output in the function return "output" attribute
#'
#' @return A logical status TRUE if it succeded or FALSE if not
#' a "message" attribute is set with an error message, and optionaly
#' an "output" attribute is set with the command output
#'
#' @keywords internal
#'
#'
# @examples
run_system_cmd <- function(command, com_args = "", output = FALSE) {

  # To fix command as an absolute path to the executable
  command <- normalizePath(command)

  err_status <- TRUE
  ret <- try(system2( command = command, args = com_args,
                      stderr = TRUE,
                      stdout = TRUE),
             silent = TRUE)
  #print(ret)

  # if any error, storing message as an attribute
  if("class" %in% names(attributes(ret)) &&
     attr(ret,"class") == "try-error"){
    err_status <- FALSE
    attr(err_status, "message") <- ret[1]
    return(err_status)
  }

  # Not a try-error ??? Why ?
  # TODO: to be merged with preceeding conditionnal block !!!!!
  if("status" %in% names(attributes(ret)) &&
     attr(ret,"status") > 0){
    err_status <- FALSE
    attr(err_status, "message") <- ret[1]
    return(err_status)
  }


  # Attaching the command output as a status attribute
  if(length(ret)){
    if(output) attr(err_status, "output") <- ret
  }

  return(invisible(err_status))

}

