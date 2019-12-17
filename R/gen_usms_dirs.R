gen_usms_dirs <- function(javastics_path, javastics_workspace_path = NULL,
                          target_path = NULL, usms_list = c(), display = FALSE, flag_dirPerUsm=TRUE) {
                          #,parallelized = FALSE, exec_time = FALSE) {
  #' @title Generating one or several usms directories from a javastics workspace
  #' content
  #'
  #' @description TODO
  #'
  #' @param javastics_path Path of JavaStics installation directory
  #' @param javastics_workspace_path Path of a JavaStics workspace (Optional)
  #' @param target_path The path of the directory where to create usms directories (Optional),
  #' if not provided the JavaStics workspace will be used as root
  #' @param usms_list List of usms to generate (Optional, if not provided, all
  #' usms contained in javastics_workspace_path/usms.xml file will be generated)
  #' @param display Logical value for displaying (TRUE) ot not (FALSE) usm name
  #' @param flag_dirPerUsm logical, TRUE if one want to create one directory per USM,
  #' FALSE if USM files are generated in the target_path (only useful for usms_list of size one)
  #'
  #' @return A list of created directories with Stics input files inside or
  #' NULL if any problem about the JavaStics workspace or JavaStics directory
  #'
  # @examples
  # TODO
  #' @export
  #'
  #' @keywords internal
  #'


  ################### TODO ######################################
  # TODO : for parallel work add a copy of javastics_workspace_path
  # and calculate if at the beginning of the foreach loop !

  #library(doParallel)

  # parallel computing management
  # cores_nb <- 1
  # if ( parallelized ) {
  #   cores_nb <- 2
  # }
  #
  # cl <- parallel::makeCluster(cores_nb)
  # registerDoParallel(cl)
  #################################################################

  jexe="JavaSticsCmd.exe"

  # checking javastics path
  check_java_path(javastics_path)

  setwd(javastics_path)

  # Checking and getting JavaStics workspace path
  ws <- check_java_workspace(javastics_path,javastics_workspace_path)
  if (base::is.null(ws)) {
    return()
  }


  # Setting the javastics workspace as root directory for usms
  # directories to generate
  if (base::is.null(target_path)) target_path <- ws

  # Creating target dir if not exists
  if (! dir.exists(target_path)) {
    dir.create(target_path)
  }

  # Retrieving usm names list from the usms.xml file
  full_usms_list = SticsRFiles :: get_usms_list(ws)

  if (length(usms_list) == 0){

    usms_list = full_usms_list

  } else {

    # Checking if the input usms_list is included in the full list
    usms_exist <- usms_list %in% full_usms_list

    # Error if any unknown usm name !
    if (!all(usms_exist)){
      stop("At least one usm does not exist us usms.xml file : ",usms_list[!usms_exist])
    }

  }

  # Command string without usm name
  cmd_generate=paste("java -jar",jexe,"--generate-txt",ws)


  usms_number <- length(usms_list)

  # For storing if all files copy were successful or not
  # for each usm
  global_copy_status <- rep(FALSE, usms_number)
  obs_copy_status <- global_copy_status


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
                  "fictec2.txt")

  files_nb <- length(files_list)

  # TODO : add obs files copy using usm_name.obs !

  # Generating source files paths
  files_path <- file.path(ws, files_list)

  # Fixing files linked to associated crops
  mandatory_files <- c(rep(T,9), F , F)

  # outputs definition files
  out_files_def <- c("var.mod", "rap.mod", "prof.mod")
  out_files_path <- file.path(javastics_path, "config",out_files_def)


  #start_time <- Sys.time()

  for (i in 1:usms_number) {
  #foreach(i = 1:usms_number, .export = ".GlobalEnv") %dopar% {

    usm_name=usms_list[i]
    if (flag_dirPerUsm) {
      usm_path <- file.path(target_path, usm_name)
    } else {
      usm_path <- target_path
    }

    if (!dir.exists(usm_path)) {
      dir.create(usm_path)
    }

    # Removing if any, optional files for associated crop
    # in the workspace
    exist_opt_files <- file.exists(files_path[! mandatory_files])
    opt_files <- files_path[! mandatory_files][exist_opt_files]
    if ( length(opt_files) ) file.remove(opt_files)

    # Generating text files
    system(paste(cmd_generate,usm_name), intern = TRUE)

    # Copying files to the usm directory
    exist_files <- file.exists(files_path)
    copy_status <- all(file.copy(from = files_path[exist_files],
                                 to = usm_path, overwrite = T))

    # Copying default files for outputs definition
    out_copy_status <- all(file.copy(from = out_files_path,
                                     to = usm_path, overwrite = T))

    # Copying observation files
    obs_path <- file.path(ws, paste0(usm_name,".obs"))
    if ( file.exists(obs_path) ) {
      obs_copy_status[i] <- file.copy(from = obs_path,
                                      to = usm_path, overwrite = T)
    }

    # Storing global files copy status
    global_copy_status[i] <- copy_status & out_copy_status

    # displaying usm name
    if (display) cat(paste0(usm_name,"\n"))


  }

  # stopping the cluster
  # stopCluster(cl)
  # duration <- Sys.time() - start_time
  # print(duration)

  # Returning a list of created directories and files copy status
  # for each directory ( FALSE if any files copy error )
  return(invisible(list(usms_paths = usms_list, files_path = files_path,
                        copy_status = global_copy_status,
                        obs_copy_status = obs_copy_status)))



}
