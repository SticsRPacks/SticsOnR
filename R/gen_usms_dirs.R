gen_usms_dirs <- function(javastics_path, javastics_workspace_path = NULL,
                          target_path = NULL, usms_list = c()) {
  #' @title Generating one or several usms directories from a javastics workspace
  #' content
  #'
  #' @description TODO
  #'
  #' @param javastics_path Path of JavaStics installation directory
  #' @param javastics_workspace_path Path of a JavaStics workspace (Optional)
  #' @param target_path The path of the directory where to create usms directories (Optional),
  #' if not provided the JavaStics workspace will be used as root
  #' @param usms_list List of usms to run (Optional)
  #'
  #' @return A list of created directories with Stics input files inside or
  #' NULL if any problem about the JavaStics workspace or JavaStics directory
  #'
  #' @examples
  #' TODO
  #' @export
  #'
  # ----------------------------------------------------------------------
  #  MODIFICATIONS (last commit)
  #  $Date: 2019-09-24 15:00:48 +0200 (mar. 24 sept. 2019) $
  #  $Author: sbuis $
  #  $Revision: 1590 $
  # ----------------------------------------------------------------------


  jexe="JavaSticsCmd.exe"

  # checking javastics path
  check_java_path(javastics_path)

  setwd(javastics_path)

  # # getting jar exe file
  # jexe="JavaSticsCmd.exe"
  # if (file.exists("JavaSticsCmd.jar")) jexe="JavaSticsCmd.jar"
  # if (length(javastics_workspace_path) > 0){
  #   if(dirname(javastics_workspace_path) == "."){
  #     # relative path to javastics path
  #     ws=file.path(javastics_path,javastics_workspace_path)
  #   } else {
  #     ws=javastics_workspace_path
  #   }
  # } else {
  #   tt<-try(ws <- get_java_wd(javastics_path),silent=TRUE)
  #   if (is(tt,"try-error")) {
  #     warning("No workspace directory has been set, use set_java_wd to do so, or \n give it as input of the function !");
  #     return()
  #   }
  # }

  # DONE: Moved previous code to a new function for calculating and checking workspace path
  # Checking and getting JavaStics workspace path
  ws <- check_java_workspace(javastics_path,javastics_workspace_path)
  if (is.null(ws)) {
    return()
  }


  # Setting the javastics workspace as root directory for usms
  # directories to generate
  if (is.null(target_path)) target_path <- ws

  # Creating target dir if not exists
  if (! dir.exists(target_path)) {
    dir.create(target_path)
  }

  # Retrieving usm names list from the usms.xml file
  full_usms_list = get_usms_list(ws)

  if (length(usms_list) == 0){

    usms_list = full_usms_list
  } else {

    # Checking if the input usms_list is included in the full list
    usm_exist=unlist(lapply(usms_list,function(x) is.element(x,full_usms_list)))

    # Error if any unknown usm name !
    if (!all(usm_exist)){
      stop("At least one usm does not exist us usms.xml file : ",usm_list[!usm_exist])
    }

  }

  # Command string without usm name
  cmd_generate=paste("java -jar",jexe,"--generate-txt",ws)


  usms_number <- length(usms_list)

  # For storing if all files copy were successful or not
  # for each usm
  global_copy_status <- rep(FALSE, usms_number)


  # Full list of the files to copy

  files_list <- c("climat.txt",
                  "param.sol",
                  "ficini.txt",
                  "ficplt1.txt",
                  "fictec1.txt",
                  "station.txt",
                  "new_travail.usm",
                  "tempopar.sti",
                  "tempoparv6.sti",
                  "ficplt2.txt",
                  "fictec2.txt"
  )

  # Generating source files paths
  files_path <- file.path(ws, files_list)

  # Fixing files linked to associated crops
  mandatory_files <- c(rep(T,9), F , F)

  # outputs definition files
  out_files_def <- c("var.mod", "rap.mod", "prof.mod")
  out_files_path <- file.path(javastics_path, "config",out_files_def)



  for (i in 1:usms_number) {

    usm_name=usms_list[i]
    usm_path <- file.path(target_path, usm_name)

    if (!dir.exists(usm_path)) {
      dir.create(usm_path)
    }

    # Removing if any, optional files for associated crop
    # in the workspace
    file.remove(files_path[! mandatory_files])

    # Generating text files
    system(paste(cmd_generate,usm_name))

    # Copying files to the usm directory
    copy_status <- file.copy(from = files_path, to = usm_path, overwrite = T)

    # Detecting if there is an associated crop in the usm
    # all files copy status used
    if ( all(file.exists( files_path[! mandatory_files ])) ) {
      copy_status <- all(copy_status)
    } else {
      copy_status <- all(copy_status[mandatory_files])
    }

    # Copying default files for outputs definition
    out_copy_status <- all(file.copy(from = out_files_path,
                                     to = usm_path, overwrite = T))


    # Storing global files copy status
    global_copy_status[i] <- copy_status & out_copy_status


  }

  # Returning a list of created directories and files copy status
  # for each directory ( FALSE if any files copy error )
  return(invisible(list(usms_paths = usms_list, files_path = files_path,
                        copy_status = global_copy_status)))



}
