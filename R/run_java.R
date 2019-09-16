run_java <- function(javastics_path,javastics_workspace_path=character(0),usms_list=c(),keep_history=TRUE){
  #' @title Running one or several usms from a javastics workspace
  #'
  #' @description This function uses basically Stics through his JavasStics command line interface !
  #'
  #' @param javastics_path Path of JavaStics installation directory
  #' @param javastics_workspace_path Path of a JavaStics workspace (Optional)
  #' @param usms_list List of usms to run (Optional)
  #' @param keep_history logical (Optional) to keep a copy of log file TRUE (default), FALSE otherwise
  #'
  #' @return A list with usm names and execution error status
  #'
  #' @examples
  #' run_java("/home/plecharpent/Work/JavaSTICS-v131-stics-v841","example")
  #' run_java("/home/plecharpent/Work/JavaSTICS-v131-stics-v841","/home/plecharpent/data/example")
  #' run_java("/home/plecharpent/Work/JavaSTICS-v131-stics-v841","example",c("ble","pois"))
  #' run_java("/home/plecharpent/Work/JavaSTICS-v131-stics-v841",usms_list=c("ble","pois"))
  #' @export
  #'
  # ----------------------------------------------------------------------
  #  MODIFICATIONS (last commit)
  #  $Date: 2019-06-20 15:17:20 +0200 (jeu. 20 juin 2019) $
  #  $Author: plecharpent $
  #  $Revision: 1446 $
  # ----------------------------------------------------------------------

  # checking javastics path
  check_java_path(javastics_path)

  setwd(javastics_path)

  # getting jar exe file
  jexe="JavaSticsCmd.exe"
  if (file.exists("JavaSticsCmd.jar")) jexe="JavaSticsCmd.jar"
  if (length(javastics_workspace_path) > 0){
    if(dirname(javastics_workspace_path) == "."){
      # relative path to javastics path
      ws=file.path(javastics_path,javastics_workspace_path)
    } else {
      ws=javastics_workspace_path
    }
  } else {
    tt<-try(ws <- get_java_wd(javastics_path),silent=TRUE)
    if (is(tt,"try-error")) {
      warning("No workspace directory has been set, use set_java_wd to do so, or \n give it as input of the function !");
      return()
    }
  }

  # cmd string without usm name
  cmd=paste("java -jar",jexe,"--run",ws)

  # getting usms list
  #xml_doc=xmldocument(file.path(ws,"usms.xml"))
  #full_usms_list=getAttrs(xml_doc,"//usm")

  # using get_usms_list
  full_usms_list = get_usms_list(ws)

  if (length(usms_list) == 0){
    usms_list = full_usms_list
  } else {
    # check
    usm_exist=unlist(lapply(usms_list,function(x) is.element(x,full_usms_list)))
    if (!all(usm_exist)){
      stop("At least one usm doesn't exist us usms.xml file : ",usm_list[!usm_exist])
    }
  }

  usms_out=list()
  usms_out$names=usms_list
  usms_out$error=rep(NaN,length(usms_list))

  for (i in 1:length(usms_list)){
    usm_name=usms_list[i]
    usms_out$error[i]=system(paste(cmd,usm_name))
    if (keep_history){
      # keeping a copy of modhistory file !
      file.copy(file.path(javastics_workspace_path,"modhistory.sti"),file.path(javastics_workspace_path,paste0("modhistory_",usm_name,".sti")))
    }
  }

  return(usms_out)
}
