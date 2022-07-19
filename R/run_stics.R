#' @title Running usm(s) from a/several directory(ies)
#' or a/several subdirectory(ies) named with usm name
#'
#' @description This function uses Stics directly through a system call
#'
#' @param stics_exe Path of Stics the executable file
#' @param workspace Path of the workspace containing the Stics (txt) input files.
#' @param usm Vector of USM names. Optional, if provided, the function runs only the given USMs.
#' If not provided, the function runs all the USMs included in workspace.
#' @param check Logical, T for checking the model executable, F otherwise
#' @param verbose Logical value (optional), TRUE to display usms names,
#' FALSE otherwise (default)
#' @param usm_dir_names `r lifecycle::badge("deprecated")` `usm_dir_names` is no
#'   longer supported, use `usm` instead.
#' @param data_dir `r lifecycle::badge("deprecated")` `data_dir` is no
#'   longer supported, use `workspace` instead.
#' @param model_path `r lifecycle::badge("deprecated")` `model_path` is no
#'   longer supported, use `stics_exe` instead.
#' @param check_exe `r lifecycle::badge("deprecated")` `check_exe` is no
#'   longer supported, use `check` instead.
#' @param display `r lifecycle::badge("deprecated")` `display` is no
#'   longer supported, use `verbose` instead.
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
#' run_stics("/home/username/bin/Stics", "/home/username/Work/SticsInputsRootDir", "wheat")
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

run_stics <- function(stics_exe,
                      workspace,
                      usm = NULL,
                      check = TRUE,
                      verbose = FALSE,
                      model_path = lifecycle::deprecated(),
                      data_dir = lifecycle::deprecated(),
                      usm_dir_names = lifecycle::deprecated(),
                      check_exe = lifecycle::deprecated(),
                      display = lifecycle::deprecated()) {
  if (lifecycle::is_present(model_path)) {
    lifecycle::deprecate_warn("1.0.0", "run_stics(model_path)", "run_stics(stics_exe)")
  } else {
    model_path <- stics_exe # to remove when we update inside the function
  }
  if (lifecycle::is_present(data_dir)) {
    lifecycle::deprecate_warn("1.0.0", "run_stics(data_dir)", "run_stics(workspace)")
  } else {
    data_dir <- workspace # to remove when we update inside the function
  }
  if (lifecycle::is_present(usm_dir_names)) {
    lifecycle::deprecate_warn("1.0.0", "run_stics(usm_dir_names)", "run_stics(usm)")
  } else {
    usm_dir_names <- usm # to remove when we update inside the function
  }
  if (lifecycle::is_present(check_exe)) {
    lifecycle::deprecate_warn("1.0.0", "run_stics(check_exe)", "run_stics(check)")
  } else {
    check_exe <- check # to remove when we update inside the function
  }
  if (lifecycle::is_present(display)) {
    lifecycle::deprecate_warn("1.0.0", "run_stics(display)", "run_stics(verbose)")
  } else {
    display <- verbose # to remove when we update inside the function
  }

  # Defining the argument of run_system for running all usms subdirectories
  if (is.null(usm_dir_names) &&
      !file.exists(file.path(workspace,"new_travail.usm"))) usm_dir_names <- "all"

  # Calling the internal underlying function for running the model
  usms_out <- run_system(model_path,
                         data_dir,
                         usm_dir_names,
                         check_exe,
                         display
  )

  return(invisible(usms_out))
}
