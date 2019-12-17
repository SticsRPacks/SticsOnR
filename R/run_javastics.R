#' @title Running one or several usms from a javastics workspace
#'
#' @description This function uses basically Stics through his JavasStics command line interface
#'
#' @param javastics_path Path of JavaStics installation directory
#' @param javastics_workspace_path Path of a JavaStics workspace (Optional)
#' @param usms_list Vector of usms to run (Optional)
#' @param keep_history Logical value (Optional) to keep a copy of history file
#' use TRUE (default), FALSE otherwise
#' @param optim Logical value (Optional) to force code_optim value to 1, use TRUE,
#' FALSE otherwise (default)
#'
#' @return A list with usms names and execution error status or NULL ikf  any problem about
#' the JavaStics workspace or JavaStics installation directory
#'
#'
#' @examples
#' \dontrun{
#' run_java("/home/plecharpent/Work/JavaSTICS-v131-stics-v841","example")
#' run_java("/home/plecharpent/Work/JavaSTICS-v131-stics-v841","/home/plecharpent/data/example")
#' run_java("/home/plecharpent/Work/JavaSTICS-v131-stics-v841","example",c("ble","pois"))
#' run_java("/home/plecharpent/Work/JavaSTICS-v131-stics-v841",usms_list=c("ble","pois"))
#' run_java("/home/plecharpent/Work/JavaSTICS-v131-stics-v841",usms_list=c("ble","pois"), optim=TRUE)
#'}
#'
#' @export
#'

run_javastics <- function(javastics_path,javastics_workspace_path=NULL,
                     usms_list=c(),keep_history=TRUE, optim=FALSE) {


  jexe="JavaSticsCmd.exe"

  # checking javastics path
  check_java_path(javastics_path)

  # Fixing the JavaStics path
  setwd(javastics_path)

  # Checking and getting JavaStics workspace path
  ws <- check_java_workspace(javastics_path,javastics_workspace_path)
  if (base::is.null(ws)) {
    return()
  }

  # Retrieving usms names list from the usms.xml file
  full_usms_list = SticsRFiles::get_usms_list(ws)

  # Selecting if needed usms
  if (length(usms_list) == 0){
    usms_list = full_usms_list
  } else {
    # check & selecting existing usms
    #usm_exist=unlist(lapply(usms_list,function(x) is.element(x,full_usms_list)))

    usm_exist <- full_usms_list %in% usms_list

    # selecting existing usms
    if (!all(usm_exist)){
      warning("At least one usm doesn't exist in the usms.xml file : ",full_usms_list[!usm_exist])
      usm_list <- full_usms_list[usm_exist]
    }

    # No usm
    if ( ! any(usm_exist ) ) stop("Not any usm exist in workspace !")

  }


  usms_out=list()
  usms_out$names=usms_list
  usms_out$error=rep(NaN,length(usms_list))

  # cmd string without usm name
  cmd_generate=paste("java -jar",jexe,"--generate-txt",ws)
  cmd_run=paste("java -jar",jexe,"--run",ws)

  for (i in 1:length(usms_list)){

    usm_name=usms_list[i]

    print(usm_name)

    if (optim) {
      system(paste(cmd_generate,usm_name), intern = T)
      tmp=run_system(file.path(javastics_path,"bin","stics_modulo"),
                     javastics_workspace_path) #, optim=optim)

      usms_out$error[i]=tmp[[1]]$error

    } else {
      usms_out$error[i]=system(paste(cmd_run,usm_name), intern = T)
    }
    if (keep_history){
      # keeping a copy of modhistory file !
      file.copy(file.path(javastics_workspace_path,"modhistory.sti"),
                file.path(javastics_workspace_path,paste0("modhistory_",usm_name,".sti")))
    }
  }

  return(invisible(usms_out))
}
