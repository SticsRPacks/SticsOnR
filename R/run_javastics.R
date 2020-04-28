#' @title Running one or several usms from a javastics workspace
#'
#' @description This function uses basically Stics through his
#' JavaStics command line interface
#'
#' @param javastics_path Path of JavaStics installation directory
#' @param workspace_path Path of a JavaStics workspace (Optional)
#' @param usms_list Vector of usms to run (optional)
#' @param keep_history Logical value (optional) to keep a copy of history file
#' use TRUE (default), FALSE otherwise
#' @param optim Logical value (optional), TRUE to force code_optim value to 1,
#' FALSE otherwise (default)
#' @param verbose Logical value (optional), TRUE to display usms names + JavaSTics output,
#' FALSE otherwise (default)
#' @param stics_exe The name of the stics executable to use, default to "stics_modulo" (see details)
#'
#' @details Use `list_stics_exe()` to list all executables available for the `stics_exe` argument.
#'
#' @return A list in which each element contains: usm "name", "error" status (logical)
#' and an output "message" (JavaStics commandline execution output)
#'
#'
#' @examples
#' \dontrun{
#' run_javastics("/path/to/JavaSTICS-1.41-stics-9.1","example")
#' run_javastics("/path/to/JavaSTICS-1.41-stics-9.1","/path/to/workspace")
#' run_javastics("/path/to/JavaSTICS-1.41-stics-9.1","example",c("wheat","pea"))
#' run_javastics("/path/to/JavaSTICS-1.41-stics-9.1",usms_list=c("wheat","pea"))
#' run_javastics("/path/to/JavaSTICS-1.41-stics-9.1",usms_list=c("wheat","pea"), optim=TRUE)
#'}
#'
#' @export
#'

run_javastics <- function(javastics_path,
                          workspace_path=NULL,
                          usms_list=NULL,
                          keep_history=TRUE,
                          optim=FALSE,
                          verbose=FALSE,
                          stics_exe= "stics_modulo") {

  # Ensure that the user working directory is unchanged after the function has run
  current_wd= getwd()
  on.exit(setwd(current_wd))

  jexe="JavaSticsCmd.exe"
  # Getting right executable name for the platform


  # Checking javastics path
  check_java_path(javastics_path)

  # Model path
  stics_path <- file.path(javastics_path,"bin",stics_exe)

  select_stics_exe(javastics_path = javastics_path, stics_exe = stics_exe)

  #  Workspace path (absolute path from user wd + platform's canonical form)
  workspace_path= normalizePath(workspace_path)

  # Fixing the JavaStics path
  setwd(javastics_path)

  user_preferences= "bin/resources/prefs"

  # Checking and getting JavaStics workspace path
  ws <- check_java_workspace(javastics_path,workspace_path)
  if(is.null(ws)){
    return()
  }

  # Retrieving usms names list from the usms.xml file
  full_usms_list = SticsRFiles::get_usms_list(file.path(ws,"usms.xml"))

  # Checking and selecting usms, if needed
  if(length(usms_list) == 0){
    usms_list = full_usms_list
  }else{
    usm_exist <- full_usms_list %in% usms_list

    # No usm
    if(!any(usm_exist)){
      stop("Not any usm exist in the workspace !")
    }

    # Selecting existing usms
    if(sum(usm_exist) != length(usms_list)){
      unknown_usms <- setdiff(full_usms_list[usm_exist], usms_list )
      warning("At least one usm does not exist in the usms.xml file : ", unknown_usms)
      usms_list <- full_usms_list[usm_exist]
    }
  }

  nb_usms <- length(usms_list)
  usms_out <- vector("list", nb_usms)

  # cmd string without usm name
  cmd_generate= paste0('-jar ',jexe,' --generate-txt "',ws,'"')
  cmd_run= paste0('-jar ',jexe,' --run "',ws,'"')

  histo_file <- file.path(workspace_path,"modhistory.sti")

  for(i in 1:nb_usms){

    usm_name=usms_list[i]
    usm_out=list()
    usm_out$name=usm_name

    # Managing historical files
    if(file.exists(histo_file)){
      file.remove(histo_file)
    }
    histo_copy <- file.path(workspace_path,paste0("modhistory_",usm_name,".sti"))
    if(file.exists(histo_copy)){
      file.remove(histo_copy)
    }

    if(verbose){
      print(usm_name)
    }

    if(optim){
      system2(command = "java", args = paste(cmd_generate,usm_name),
              stdout= if(verbose){""}else{NULL})
      tmp=run_system(stics_path, workspace_path) #, optim=optim)

      usm_out$error <- tmp[[1]]$error
      usm_out$message <- tmp[[1]]$message

    }else{
      status <- system2(command = "java", args = paste(cmd_run,usm_name),
                        stdout= if(verbose){""}else{NULL})

      err <- grep(pattern = "[eE]rror", tolower(status))
      if(length(err)>0|status!=0){
        # Any error, keeping the line with Error message
        usm_out$error <- TRUE
        usm_out$message <- status
      }else{
        # No errors: keeping lines of JavaSticsCmd execution
        usm_out$error <- FALSE
        usm_out$message <- paste(status, collapse = "\n")
      }
    }

    # Keeping a copy of modhistory file !
    if(keep_history && file.exists(histo_file)){
      file.copy(histo_file, histo_copy)
    }

    # Storing usm output infos
    usms_out[[i]] <- usm_out
  }

  # Naming the list elements
  # names(usms_out) <- usms_list

  # Returning usms list with execution return
  return(invisible(usms_out))
}
