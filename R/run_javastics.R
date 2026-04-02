#' @title Running one or several usms from a javastics workspace
#'
#' @description This function uses basically Stics through his
#' JavaStics command line interface
#'
#' @param javastics Path of JavaStics
#' @param workspace Path of a JavaStics workspace
#' @param usm Vector of USM names. Optional, if provided, the function runs
#' only the given USMs.
#' If not provided, the function runs all the USMs included in workspace.
#' @param keep_history Logical value (optional) to keep a copy of history file
#' use `TRUE` (default), `FALSE` otherwise
#' @param optim Logical value (optional), `TRUE` to force code_optim value to 1,
#' `FALSE` otherwise (default)
#' @param verbose Logical value for displaying information while running
#' @param stics_exe The name, executable or path of the stics executable to use
#'  (optional, default to "modulostics", see details)
#' @param java_cmd The java virtual machine command name or executable path
#'
#' @details `stics_exe` may be :
#' 1. a model name pointing to a stics executable as done in JavaStics, e.g.
#' "modulostics" for `stics_modulo.exe`, the standard version of the model
#' shipping with JavaStics;
#' 2. a stics executable file available from the bin folder in JavaStics, e.g.
#' "stics_modulo.exe";
#' 3. a path to a stics executable file, eg.
#' "C:/Users/username/Desktop/stics.exe". NB: this file cannot be named
#' `stics_modulo.exe` because it is the name of the standard STICS shipping
#' with JavaStics (overwriting is not allowed).
#'
#' @return A list in which each element contains: usm "name", "error" status
#' (logical) and an output "message" (JavaStics commandline execution output)
#'
#'
#' @examples
#' \dontrun{
#' run_javastics("/path/to/JavaSTICS/folder", "example")
#' run_javastics("/path/to/JavaSTICS/folder", "/path/to/workspace")
#' run_javastics("/path/to/JavaSTICS/folder", "example", c("wheat", "pea"))
#' run_javastics("/path/to/JavaSTICS/folder", usm = c("wheat", "pea"))
#' run_javastics("/path/to/JavaSTICS/folder",
#'   usm = c("wheat", "pea"), optim = TRUE
#' )
#' }
#'
#' @export
#'
run_javastics <- function(
  javastics,
  workspace = NULL,
  usm = NULL,
  keep_history = TRUE,
  optim = FALSE,
  verbose = TRUE,
  stics_exe = "modulostics",
  java_cmd = "java"
) {
  # Ensure that the user working directory is unchanged after
  # the function has run
  current_wd <- getwd()
  on.exit(setwd(current_wd))

  # Use absolute path from now on:
  javastics <- normalizePath(javastics, winslash = "/")
  workspace <- normalizePath(
    workspace,
    winslash = "/",
    mustWork = FALSE
  )

  # Help people that don't remember well the standard name:
  if (stics_exe == "stics_modulo" || stics_exe == "sticsmodulo") {
    stics_exe <- "modulostics"
  }

  # Checking javastics path
  SticsRFiles:::check_java_path(javastics)

  # Getting right executable name for the platform
  if (stics_exe == "modulostics") {
    # using the exe name instead of the identifier to select the right one
    # for the user's OS
    stics_exe <- paste0("stics_modulo", SticsRFiles:::os_suffix())
  }

  # Model path
  stics_path <- file.path(javastics, "bin", stics_exe)

  # On exit, return to the version used before:
  on.exit(
    set_stics_exe(
      javastics = javastics,
      stics_exe = list_stics_exe(javastics)$current[[1]],
      verbose = FALSE
    ),
    add = TRUE
  )

  set_stics_exe(
    javastics = javastics,
    stics_exe = stics_exe,
    overwrite = TRUE,
    verbose = verbose
  )

  # Fixing the JavaStics path
  setwd(javastics)

  # Checking and getting JavaStics workspace path
  ws <- SticsRFiles:::check_java_workspace(javastics, workspace)
  if (is.null(ws)) {
    return()
  }

  # Retrieving usms names list from the usms.xml file
  full_usms_list <- SticsRFiles::get_usms_list(file.path(ws, "usms.xml"))

  # Checking and selecting usms, if needed
  if (is.null(usm)) {
    usm <- full_usms_list
  } else {
    usm_exist <- full_usms_list %in% usm

    # No usm
    if (!any(usm_exist)) {
      stop("Not any usm exist in the workspace !")
    }

    # Selecting existing usms
    if (sum(usm_exist) != length(usm)) {
      unknown_usms <- setdiff(full_usms_list[usm_exist], usm)
      warning(
        "At least one usm does not exist in the usms.xml file : ",
        unknown_usms
      )
      usm <- full_usms_list[usm_exist]
    }
  }

  nb_usms <- length(usm)
  usms_out <- vector("list", nb_usms)

  # Getting arguments to give to the system2 command
  # for executing files conversion or simulation runs
  # using JavaStics command line interface
  cmd_type <- "run"
  if (optim) cmd_type <- "generate"
  cmd_list <- SticsRFiles:::get_javastics_cmd(
    javastics,
    java_cmd = java_cmd,
    type = cmd_type,
    workspace = ws,
    verbose = verbose
  )
  command <- cmd_list[[1]]
  cmd_string <- cmd_list[[2]]

  histo_file <- file.path(workspace, "modhistory.sti")

  for (i in 1:nb_usms) {
    usm_name <- usm[i]
    usm_out <- list()
    usm_out$name <- usm_name

    # Managing historical files
    if (file.exists(histo_file)) {
      file.remove(histo_file)
    }
    histo_copy <- file.path(
      workspace,
      paste0(
        "modhistory_",
        usm_name,
        ".sti"
      )
    )
    if (file.exists(histo_copy)) {
      file.remove(histo_copy)
    }

    print(usm_name)

    if (optim) {
      system2(
        command = command,
        args = paste(cmd_string, usm_name),
        stdout = if (verbose) {
          ""
        } else {
          NULL
        }
      )
      tmp <- run_system(stics_path, workspace)

      usm_out$error <- tmp[[1]]$error
      usm_out$message <- tmp[[1]]$message
    } else {
      status <- system2(
        command = command,
        args = paste(cmd_string, usm_name),
        stdout = TRUE,
        stderr = TRUE,
      )

      if (verbose) {
        print(status)
      }

      err <- grep(pattern = "[eE]rror", tolower(status))
      if (length(err) > 0) {
        # Any error, keeping the line with Error message
        usm_out$error <- TRUE
        usm_out$message <- status
      } else {
        # No errors: keeping lines of JavaSticsCmd execution
        usm_out$error <- FALSE
        usm_out$message <- paste(status, sep = "\n")
      }
    }

    # Keeping a copy of modhistory file !
    if (keep_history && file.exists(histo_file)) {
      file.copy(histo_file, histo_copy)
    }

    # Storing usm output infos
    usms_out[[i]] <- usm_out
  }

  # Naming the list elements
  # Final message:
  worked <- !unlist(lapply(usms_out, function(x) x$error))

  if (verbose) {
    if (all(worked)) {
      cli::cli_alert_success("\nAll usms ran successfully!")
    } else {
      cli::cli_alert_danger(
        "Error during simulation of usm{?s} {.val {usm[!worked]}}"
      )
    }
  }

  # Returning usms list with execution return
  return(invisible(usms_out))
}
