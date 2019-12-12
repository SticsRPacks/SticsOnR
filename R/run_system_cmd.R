#' Run a system command
#'
#' @param command A system command
#' @param args A list of arguments to pass to the command
#' @param output A logical value to specify returning the command output (T) or not (F, default)
#'
#' @return A logical status T if it succeded or F if not (an attribute "message" is set with an error message)
#'
#@export
#'
# @examples
run_system_cmd <- function(command, args="", output=FALSE) {

  status = TRUE
  ret <- try(system2( command = command, args = args,
                       stderr = TRUE,
                       stdout = output),
              silent = TRUE)
  #print(ret)

  # if any error, storing message as an attribute
  if ( "class" %in% names(attributes(ret)) &&
       attr(ret,"class") == "try-error") {
    status = FALSE
    attr(status, "message") <- ret[1]
    return(status)
  }

  # Attaching the command output as a status attibute
  if (length(ret)) {
    if ( output ) attr(status, "output") <- ret
  }

  return(invisible(status))

}

