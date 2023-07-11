#' @title Running usm(s) from a/several directory(ies)
#' or a/several subdirectory(ies) named with usm name
#'
#' @description This function uses Stics directly through a system call
#'
#' @param stics_exe Path of Stics executable file
#' @param workspace Path of a Stics input directory or a vector of,
#' or root directory of Stics directories (if usm is given)
#' @param usm Name(s) vector of sub-directory(ies) of workspace
#' or "all" for extracting all sub-directories path
#' @param check Logical, T for checking the model executable, F otherwise
#' @param verbose Logical value (optional), TRUE to verbose usms names,
#' FALSE otherwise (default)
#'
#' @return A list in which each element contains: usm "name", "error" status
#'  (logical) and an output "message" (model execution output)
#'
#' @examples
#' \dontrun{
#'
#' # Specifying individual usm directories
#' run_system("/home/username/bin/Stics", "/home/username/Work/SticsInputsDir")
#' run_system(
#'   "/home/username/bin/Stics",
#'   c(
#'     "/home/username/Work/SticsInputsDir1",
#'     "/home/username/Work/SticsInputsDir2"
#'   )
#' )
#'
#' # Specifying a parent directory of usms directories
#' # running one or several usms
#' run_system(
#'   "/home/username/bin/Stics",
#'   "/home/username/Work/SticsInputsRootDir", "wheat"
#' )
#' run_system(
#'   "/home/username/bin/Stics",
#'   "/home/username/Work/SticsInputsRootDir",
#'   c("wheat", "maize")
#' )
#' # running all usms
#' run_system(
#'   "/home/username/bin/Stics",
#'   "/home/username/Work/SticsInputsRootDir",
#'   "all"
#' )
#' }
#'
#' @keywords internal
#'
#' @noRd
#'


run_system <- function(stics_exe,
                       workspace,
                       usm = NULL,
                       check = TRUE,
                       verbose = FALSE) {
  first_wd <- getwd()
  on.exit(setwd(first_wd))
  # Default one usm directory
  run_dir <- normalizePath(workspace, winslash = "/")

  if (!is.null(usm) && !usm == "all") {
    run_dir <- file.path(run_dir, usm)
  }

  if (!is.null(usm) && usm == "all") {
    run_dir <- setdiff(list.dirs(run_dir, full.names = TRUE), run_dir)
  }

  # testing id dirs exist
  # 'print(run_dir)
  dirs_exist <- file.exists(run_dir)

  if (!all(dirs_exist)) {
    print(paste0(run_dir[!dirs_exist], collapse = ", "))
    stop("One or more directories does/do not exist !")
  }


  # optional model executable checking
  if (check) check_stics_exe(stics_exe)

  nb_usms <- length(run_dir)
  usms_out <- vector("list", nb_usms)

  for (d in 1:nb_usms) {
    usm_out <- list()
    usm_dir <- run_dir[d]
    # ' usm_out$name <- basename(usm_dir)
    setwd(usm_dir)

    usm_out$name <- SticsRFiles::get_usm_txt()$nom

    if (verbose) print(usm_out$name)

    # new function call, keeping error message as attribute
    ret <- run_system_cmd(command = stics_exe, output = TRUE)
    usm_out$error <- !as.logical(ret)
    usm_out$message <- attr(ret, "output")

    if (usm_out$error) {
      # TODO: see if concatenation !
      # ' usm_out$message="Model execution error !"
      usm_out$message <- attr(ret, "message")
    }


    # If history file doesn't exist or all of the output files
    # don't exist: output error
    usm_chk_out <- SticsRFiles:::check_output_files(usm_dir)

    # additional check for missing output files
    if (!usm_out$error && usm_chk_out$error) {
      print(paste("Error, missing output file(s) : ", usm_dir))
      usm_out$error <- TRUE
      usm_out$message <- paste("No output files: ", usm_chk_out$missing)
    }

    usms_out[[d]] <- usm_out
  }

  return(invisible(usms_out))
}
