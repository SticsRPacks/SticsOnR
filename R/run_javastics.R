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
#' @param display Logical value (optional), TRUE to display usms names,
#' FALSE otherwise (default)
#'
#' @return A list with usms names and execution error status or NULL if any problem about
#' the JavaStics workspace or JavaStics installation directory
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
                          display=TRUE) {


  jexe="JavaSticsCmd.exe"
  stics_exe <- "stics_modulo"
  # Getting right executable name for the platform
  if ( optim && is_os_name(c("mac","darwin")) ) stics_exe <- "stics_modulo_mac"

  # Checking javastics path
  check_java_path(javastics_path)

  # Model path
  stics_path <- file.path(javastics_path,"bin",stics_exe)

  # Fixing the JavaStics path
  setwd(javastics_path)

  # Checking and getting JavaStics workspace path
  ws <- check_java_workspace(javastics_path,workspace_path)
  if (base::is.null(ws)) {
    return()
  }

  # Retrieving usms names list from the usms.xml file
  full_usms_list = SticsRFiles::get_usms_list(ws)[[1]]

  # Checking and selecting usms, if needed
  if (length(usms_list) == 0) {
    usms_list = full_usms_list
  } else {
    usm_exist <- full_usms_list %in% usms_list

    # No usm
    if ( ! any(usm_exist) ) stop("Not any usm exist in the workspace !")

    # Selecting existing usms
    if ( sum(usm_exist) != length(usms_list) ){
      unknown_usms <- setdiff(full_usms_list[usm_exist], usms_list )
      warning("At least one usm does not exist in the usms.xml file : ", unknown_usms)
      usms_list <- full_usms_list[usm_exist]
    }
  }

  usms_out=list()
  usms_out$names=usms_list
  usms_out$error=rep(NaN,length(usms_list))

  # cmd string without usm name
  cmd_generate=paste("java -jar",jexe,"--generate-txt",ws)
  cmd_run=paste("java -jar",jexe,"--run",ws)

  histo_file <- file.path(workspace_path,"modhistory.sti")

  for (i in 1:length(usms_list)){

    usm_name=usms_list[i]

    # Managing historical files
    if (base::file.exists(histo_file)) base::file.remove(histo_file)
    histo_copy <- file.path(workspace_path,paste0("modhistory_",usm_name,".sti"))
    if (base::file.exists(histo_copy)) base::file.remove(histo_copy)

    if (display) print(usm_name)

    if (optim) {
      system(paste(cmd_generate,usm_name), intern = T)
      tmp=run_system(stics_path, workspace_path) #, optim=optim)

      usms_out$error[i]=tmp[[1]]$error

    } else {
      status <- system(paste(cmd_run,usm_name), intern = T)
      err <- grep(pattern = "[eE]rror", tolower(status))
      if (length(err)) {
        # Any error, keeping the line with Error message
        usms_out$error[i]=status[err]
      } else {
        # No errors: keeping lines of JavaSticsCmd execution
        usms_out$error[i]=paste(status, collapse = "\n")
      }
    }

    # Keeping a copy of modhistory file !
    if (keep_history && base::file.exists(histo_file)) {
      base::file.copy(histo_file, histo_copy)
    }
  }

  # Returning usms list with execution return
  return(invisible(usms_out))
}
