#' @title Running usm(s) from a/several directory(ies)
#' or a/several subdirectory(ies) named with usm name
#'
#' @description This function uses Stics directly through a system call
#'
#' @param stics_exe Path of Stics the executable file
#' @param workspace Path of the workspace containing the Stics (txt) input files
#' @param usm Vector of USM names. Optional, if provided, the function runs only
#'  the given USMs.
#' If not provided, the function runs all the USMs included in workspace.
#' @param check Logical, T for checking the model executable, F otherwise
#' @param verbose Logical value (optional), TRUE to display usms names,
#' FALSE otherwise (default)
#'
#' @return A list with usm names and execution error status
#'
#' @examples
#' \dontrun{
#'
#' # Specifying individual usm directories
#' run_stics("/home/username/bin/Stics", "/home/username/Work/SticsInputsDir")
#' run_stics("/home/username/bin/Stics", c(
#'   "/home/username/Work/SticsInputsDir1",
#'   "/home/username/Work/SticsInputsDir2"
#' ))
#'
#' # Specifying a parent directory of usms directories
#' # running one or several usms
#' run_stics(
#'   "/home/username/bin/Stics",
#'   "/home/username/Work/SticsInputsRootDir", "wheat"
#' )
#' run_stics(
#'   "/home/username/bin/Stics", "/home/username/Work/SticsInputsRootDir",
#'   c("wheat", "maize")
#' )
#' # running all usms
#' run_stics(
#'   "/home/username/bin/Stics", "/home/username/Work/SticsInputsRootDir",
#' )
#' }
#'
#' @export
#'

run_stics <- function(
  stics_exe,
  workspace,
  usm = NULL,
  check = TRUE,
  verbose = FALSE
) {
  # Defining the argument of run_system for running all usms subdirectories
  if (
    is.null(usm) &&
      !file.exists(file.path(workspace, "new_travail.usm"))
  ) {
    usm <- "all"
  }

  # Calling the internal underlying function for running the model
  usms_out <- run_system(
    stics_exe,
    workspace,
    usm,
    check,
    verbose
  )

  return(invisible(usms_out))
}
