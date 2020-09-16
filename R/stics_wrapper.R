#' @title Running usm(s) from txt input files stored in one directory
#' per `situation`, simulated results are returned in a list
#'
#' @description This function uses Stics directly through a system call, can
#' force Stics input parameters with values given in arguments.
#'
#' @param model_options List containing any information needed by the model.
#' In the case of Stics: `javastics_path` (or/and `stics_exe`) and
#' `data_dir` the path of the directory containing the Stics input data
#' for each USM (one folder per USM where Stics input files are stored in txt
#' format). See `stics_wrapper_options()` for more informations.
#'
#' @param param_values (optional) a tibble that contains values of Stics input
#' parameters to use in the simulations. Should have one named column per
#' parameter. An optional column named Situation containing the name of the
#' situations (USMs for Stics) allows to define different values of the parameters
#' for different situations. If param_values is not provided, the simulations will
#' be performed using the parameters values defined in the Stics input files referenced
#' in model_options argument.
#'
#' @param sit_names (optional) vector of situations (USMs) names for which results
#' must be returned. Results for all simulated situations are returned if not provided.
#'
#' @param var_names (optional) vector of variables names for which results
#' must be returned. Results for all simulated variables are returned if not provided.
#'
#' @param dates (optional) vector of dates (POSIXct) for which results
#' must be returned. Results for all dates simulated are returned if not provided.
#' If required dates varies between situations, either use stages or sit_var_dates_mask argument
#'
#' @param stages (optional) vector of stages for which results must be returned.
#'
#' @param sit_var_dates_mask (optional) List of situations: a named list
#' containing a mask for variables and dates for which simulated values
#' should be returned. Typically a list containing the observations to which
#' simulations should be compared as provided by SticsRFiles::get_obs
#'
#' @return A list containing simulated values (`sim_list`: a list of tibbles (one
#' element per situation) and an error code (`error`) indicating if at least one
#' simulation ended with an error.
#'
#' @seealso `stics_wrapper_options()` for more informations on how to provide `model_options`.
#'
#' @examples
#'
#' \dontrun{
#'
#' # Specifying the JavaStics folder
#' javastics_path <- "/path/to/javastics"
#'
#' # Setting the input data folder path, root directory of the usms directories
#' data_path <- "/path/to/usms/subdirs/root"
#'
#' # Setting the mandatory simulations options
#' sim_options <- stics_wrapper_options(javastics_path = javastics_path, data_dir = data_path)
#'
#' # Running all the usms that have a corresponding input folder in data_path
#' results <- stics_wrapper(sim_options)
#'
#' # Running a sublist of usm
#' usms_list <- c("wheat", "pea", "maize")
#' results <- stics_wrapper(sim_options, sit_var_dates_mask = usms_list)
#'
#' # Applying a single parameter values vector for the sublist of usms
#' param_values <- c(0.002,50)
#' names(param_values) <- c("dlaimax", "durvieF")
#' results <- stics_wrapper(model_options = sim_options,
#' sit_var_dates_mask = usms_list, param_values = param_values)
#'
#' # Applying different values of the parameters for the usms
#' # Let's run usm wheat with
#' # c(dlaimax=0.001, durvieF=50) and c(dlaimax=0.002, durvieF=50),
#' # usm pea with c(dlaimax=0.001, durvieF=60) and c(dlaimax=0.002, durvieF=60),
#' # and usm maize with c(dlaimax=0.001, durvieF=70)
#' # and c(dlaimax=0.002, durvieF=70)
#' param_values <- array( c(0.001, 0.002, 50, 50,
#'                          0.001, 0.002, 60, 60,
#'                          0.001, 0.002, 70, 70),
#'                        dim=c(2,2,3),
#'                        dimnames=list(NULL,c("dlaimax", "durvieF"),
#'                        c("wheat", "pea", "maize")))
#' # In this case, no need to redefine the usms list in sit_var_dates_mask
#' # argument, it is already given in param_values
#' results <- stics_wrapper(model_options = sim_options,
#' param_values = param_values)
#'
#' }
#'
#' @export
#'
#' @importFrom foreach %dopar%
#' @import parallel
#' @import doParallel
#'
stics_wrapper <- function(model_options,
                          param_values = NULL,
                          sit_names = NULL,
                          var_names = NULL,
                          dates = NULL,
                          stages = NULL,
                          sit_var_dates_mask = NULL){

  # TODO LIST
  #    - handle the case of stages (stages should be specified in the var.mod ...
  #      + handle the case when simulations does not reach the asked stages ...)
  #    - handle the case of intercrop
  #

  # Stopping the cluster when exiting
  on.exit(parallel::stopCluster(cl))


  # Preliminary model checks and initializations -------------------------------

  # Checking model_options content, stopping when mandatory
  # values are not set or not valid
  model_options= do.call(stics_wrapper_options,model_options)
  # NB: splating the arguments given in input so the function checks by itself, no need to
  # explicitely add an argument for checking

  # Getting list values into separated variables
  stics_exe <- model_options$stics_exe
  data_dir <- model_options$data_dir
  parallel <- model_options$parallel
  cores <- model_options$cores
  verbose <- model_options$verbose
  time_display <- model_options$time_display
  successive_usms <- model_options$successive_usms

  # In case of successive USMs, disable parallel run
  if (!is.null(successive_usms)) parallel <- FALSE

  # Checking Stics executable
  check_stics_exe(stics_exe)

  # Activate the stopwatch if required
  if(time_display) start_time <- Sys.time()

  # Managing parallel model simulations
  # Managing cores number to use
  cores_nb <- get_cores_nb( parallel = parallel, required_nb = cores )

  # Launching the cluster
  cl <- parallel::makeCluster(cores_nb)
  doParallel::registerDoParallel(cl)

  parallel::clusterCall(cl, function(x) .libPaths(x), .libPaths())



  # Define the list of USMs to simulate and initialize results -----------------

  # Check the available USMs
  avail_sit <- list.dirs(data_dir, full.names = TRUE)[-1]
  ## Checking existing files
  files_exist <- file.exists(file.path(avail_sit, "new_travail.usm"))
  avail_sit <- basename(avail_sit)[files_exist]
  if(length(avail_sit) == 0){
    stop(paste("Not any Stics directories found in:",data_dir))
  }


  # Define the USMs to simulate from available USMs and user requirements concerning
  # the results to return (sit_names and sit_var_dates_mask arguments)
  # => sit2simulate and required_situations
  required_situations <- union(sit_names, names(sit_var_dates_mask))
  if (!is.null(required_situations)) {
    # If some required situations can not be simulated, warns the user
    if (length(setdiff(required_situations,avail_sit))>0) {
      warning(paste0("No folder(s) found in ",data_dir," for USMs ",
                     paste(setdiff(required_situations,avail_sit),collapse = " "),
                     "\n These USMs will not be simulated."))
    }
    sit2simulate <- intersect(avail_sit,required_situations)
  } else {
    # If neither sit_names nor sit_var_dates_mask are provided, all USMs defined in
    # subfolders of data_dir must be simulated
    sit2simulate <- avail_sit
    required_situations <- avail_sit
  }

  # Case of successive USMs (argument successive_usms)
  ## Check that all successive usms are available
  if (length(setdiff(unlist(successive_usms),avail_sit))>0) {
    warning(paste0("No folder(s) found in ",data_dir," for USMs ",
                   paste(setdiff(unlist(successive_usms),avail_sit),collapse = " "),
                   "\n The corresponding successions of USMs will not be simulated."))
    # Remove successions for which at least one USMs is not available
    idx<-unique(sapply(setdiff(unlist(successive),avail), function(x) which(sapply(successive, function(y) x %in% y))))
    successive_usms[[idx]] <- NULL
  }
  ## Add the successive USMs in the list of USMs to simulate if there are some missing ones and order them
  sit2simulate <- c(unlist(successive_usms), setdiff(sit2simulate,unlist(successive_usms)))


  # Calculating directories list
  run_dirs <- file.path(data_dir,sit2simulate)

  res <- list()
  res$error <- FALSE
  res$sim_list <- setNames(vector("list",length(required_situations)), required_situations)

  # Should all data be returned for each required situation ?
  keep_all_data <- is.null(sit_var_dates_mask) && is.null(var_names) && is.null(dates)



  #################
  #### ATTENTION, si on change l'ordre des boulce doe/situations, cela va poser pb pour les successive USMs ...
  #################


  # Default output data list
  nb_paramValues=1
  if (!is.null(param_values)) {
    nb_paramValues=dim(param_values)[1]
  }







  # Run Stics and store results ------------------------------------------------


  for(ip in 1:nb_paramValues) {

    ## Loops on the USMs that can be simulated
    ## out is a list containing: the list of simulated outputs,
    ##  a flag TRUE if the requested simulation has been not performed (model error),
    ##  a flag FALSE if all the requested dates and variables were not simulated,
    ##  a message in case of warning or error

    # initialization not a global variable !
    i <- 1
    out <- foreach::foreach(i = seq_along(sit2simulate),.export = "run_stics",
                            .packages = c("SticsRFiles")) %dopar% {

                              # Simulation flag status or output data selection status
                              flag_error <- FALSE
                              flag_rqd_res <- TRUE
                              run_dir <- run_dirs[i]
                              situation <- sit2simulate[i]
                              mess <- ""
                              ########################################################################
                              # TODO: make a function dedicated to forcing parameters of the model ?
                              # In that case by using the param.sti mechanism
                              ## Force param values
                              if (is.null(param_values) || ! sit2simulate[i] %in% dimnames(param_values)[[3]]) {
                                # remove param.sti in case of previous run using it ...
                                if (suppressWarnings(file.remove(file.path(run_dir,
                                                                           "param.sti")))) {
                                  SticsRFiles:::set_codeoptim(run_dir,value=0)
                                }

                              } else {
                                ind_non_na<-!is.na(param_values[ip,,sit2simulate[i]])
                                param_values_usm=param_values[ip,ind_non_na,sit2simulate[i]]
                                names(param_values_usm)=colnames(param_values)[ind_non_na]

                                ret <- SticsRFiles::gen_paramsti(run_dir, names(param_values_usm), param_values_usm)

                                # if writing the param.sti fails, treating next situation
                                if ( ! ret ) {
                                  mess <- warning(paste("Error when generating the forcing parameters file for USM",situation,
                                                        ". \n "))
                                  return(list(NULL,TRUE,FALSE, mess))
                                }


                                SticsRFiles:::set_codeoptim(run_dir, value=1)
                              }

                              # Handling successive USMs (if the usm is part of the list and not in first position ...)
                              is_succ <- any(sapply(successive_usms,function(x) match(sit2simulate[i],x))>=2)
                              if (!is.na(is_succ) && is_succ) {
                                # if (file.exists(file.path(run_dirs[iusm-1],"recup.tmp"))) {
                                #   file.copy(from=file.path(run_dirs[iusm-1],"recup.tmp"),
                                #             to=file.path(run_dir,"recup.tmp"),overwrite = TRUE)
                                #
                                # } else {
                                #   mess <- warning(paste("Error running the Stics model for USM",situation,
                                #                         ". \n This USMs is part of a succession but recup.tmp file was not created by the previous USM."))
                                #   return(list(NA,TRUE,FALSE, mess))
                                # }

                                # Checking recup.tmp and snow_variables.txt files

                                f_recup <- c(file.path(run_dirs[i-1],"recup.tmp"), file.path(run_dirs[i-1],"snow_variables.txt"))
                                f_exist <- file.exists(f_recup)

                                if (! all(f_exist) ) {
                                  mess <- warning(paste("Error running the Stics model for USM", situation,
                                                        ". \n This USMs is part of a succession but recup.tmp or snow_variables.txt",
                                                        "file(s) was/were not created by the previous USM."))
                                  return(list(NULL,TRUE,FALSE, mess))
                                }

                                # Copying files and checking return
                                if (!file.copy(from = f_recup, to = run_dir, overwrite = TRUE)) {
                                  mess <- warning(paste("Error copying recup.tmp and/or snow_variables.txt file(s) for ", situation,
                                                        "file(s) was/were not created by the previous USM."))
                                  return(list(NULL,TRUE,FALSE, mess))
                                }

                                # The following could be done only once in case of repeated call to the wrapper (e.g. parameters estimation ...)
                                SticsRFiles::set_usm_txt(filepath = file.path(run_dir,"new_travail.usm"), param="codesuite", value=1)
                              }

                              varmod_modified=FALSE
                              while(TRUE) {

                                # TODO: check or set the flagecriture to 15 to get daily data results !!!

                                ########################################################################
                                # TODO: and call it in/ or integrate parameters forcing in run_system function !
                                ## Run the model & forcing not to check the model executable
                                usm_out <- run_stics(stics_exe, run_dir, check_exe = FALSE)

                                # In case of successive USMs, re-initialize codesuite (to allow next run to be in non-successive mode)
                                if (!is.na(is_succ) && is_succ) {
                                  SticsRFiles::set_usm_txt(filepath = file.path(run_dir,"new_travail.usm"), param="codesuite", value=0)
                                }

                                # if the model returns an error, ... treating next situation
                                if(usm_out[[1]]$error){
                                  mess <- warning(paste("Error running the Stics model for USM",situation,
                                                        ". \n ",usm_out[[1]]$message))
                                  return(list(NULL,TRUE,FALSE, mess))
                                }

                                # Get the number of plants to know whether it is a sole crop or an intercrop:
                                #nbplants= as.numeric(SticsRFiles::get_usm_txt(filepath = file.path(data_dir,situation,"new_travail.usm"))$nbplantes)
                                mixed <- SticsRFiles::get_plants_nb(usm_file_path = file.path(data_dir, situation, "new_travail.usm")) > 1

                                ## Otherwise, getting results
                                sim_tmp= SticsRFiles::get_daily_results(file.path(data_dir, situation),
                                                                        situation, mixed= mixed)[[1]]
                                # Any error reading output file
                                if(is.null(sim_tmp)){
                                  mess <- warning(paste("Error reading outputs for ",situation,
                                                        ". \n "))
                                  return(list(NULL,TRUE,FALSE, mess))
                                }






                                # Keeping all outputs data
                                # - If no sit_var_dates_mask given as input arg
                                # - if only usm names given in sit_var_dates_mask (vector)
                                # - If all output variables are in
                                #   sit_var_dates_mask[[situation]]

                                # Nothing to select, returning all data
                                if(keep_all_data){

                                  return(list(sim_tmp,FALSE, TRUE, mess))

                                } else if(!is.null(sit_var_dates_mask) && is.null(sit_var_dates_mask[[situation]])) {

                                  return(list(NULL,FALSE, TRUE, mess))

                                } else { # Selecting variables from sit_var_dates_mask or var_names

                                  if (!is.null(sit_var_dates_mask)) {
                                    var_list <- colnames(sit_var_dates_mask[[situation]])
                                  } else {
                                    var_list <- c(var_names)
                                  }
                                  # Remove the optional "Plant" variable from observations and add Date, ian, mo, jo, jul if needed
                                  var_list <- var_list[!grepl("Plant",var_list)]
                                  var_list <- unique(c(c("Date","ian","mo","jo","jul"), var_list))

                                  # Keeping only the needed variables in the simulation results
                                  out_var_list <- colnames(sim_tmp)
                                  vars_idx= out_var_list %in% var_list

                                  # Checking variables
                                  # Common variables
                                  inter_vars <- out_var_list[vars_idx]


                                  # In case some variables are not simulated, warn the user, add them in var.mod and re-simulate
                                  if(length(inter_vars) < length(var_list)){
                                    diff_vars= setdiff(var_list,inter_vars)

                                    if(varmod_modified){
                                      mess <- warning(paste("Variable(s)",paste(setdiff(var_list,inter_vars), collapse=", "),
                                                            "not simulated by the Stics model for USM",situation,
                                                            "although added in",file.path(data_dir,situation,"var.mod"),
                                                            "=> these variables may not be Stics variables, please check spelling. \n ",
                                                            "Simulated variables:",paste(out_var_list,collapse=", ")))
                                      flag_error <- FALSE
                                      flag_rqd_res <- FALSE

                                    }else{
                                      # Remove the dummy variables output from STICS but not input:
                                      var_list= var_list[!var_list %in% c("Date","ian","mo","jo","jul","cum_jul")]

                                      if(verbose){
                                        mess <- warning(paste("Variable(s)",paste(setdiff(var_list,inter_vars), collapse=", "),
                                                              "not simulated by the Stics model for USM",situation,
                                                              "=>",file.path(data_dir,situation,"var.mod"),"has been modified and the model re-run."))
                                      }

                                      # For the moment, as we do not provide functions for adding new stics versions,
                                      # we don't check the existence of Stics variables (so that if they are defined in the var.mod
                                      # they can be simulated even if not defined in outputs.csv)
                                      SticsRFiles::gen_varmod(workspace = file.path(data_dir,situation),var_names = var_list, force=TRUE)
                                      varmod_modified=TRUE
                                      next()  # go to the next iteration of while loop now that we have well defined the var.mod file
                                    }

                                  }


                                  # Select the results wrt to the required (and simulated) variables
                                  if(any(vars_idx)){
                                    sim_tmp= sim_tmp[ , vars_idx]

                                  }else{
                                    # If no variable simulated, warn the user and return NULL
                                    mess <- warning(paste("Requested variable(s)",paste(var_list, collapse=", "),
                                                          "for USM",situation," is (are) not valid STICS variable(s)."))
                                    return(list(NULL,TRUE,FALSE, mess))
                                  }


                                  ## Keeping only the required dates in the simulation results
                                  if (!is.null(sit_var_dates_mask)) {
                                    date_list <- sit_var_dates_mask[[situation]]$Date
                                  } else if (!is.null(dates)) {
                                    date_list <- dates
                                  } else {
                                    return(list(sim_tmp,FALSE, TRUE, mess))
                                  }

                                  dates_idx <- sim_tmp$Date %in% date_list
                                  inter_dates <- sim_tmp$Date[dates_idx]

                                  if ( length(inter_dates) < length(date_list) ) {
                                    missing_dates <- date_list[!date_list %in% inter_dates]
                                    if(verbose){
                                      mess <- warning(paste("Requested date(s)",paste(missing_dates, collapse=", "),
                                                            "is(are) not simulated for USM",situation))
                                    }
                                    flag_error <- FALSE
                                    flag_rqd_res <- FALSE
                                  }

                                  # Filtering requested dates
                                  if(any(dates_idx)){
                                    sim_tmp <- sim_tmp[dates_idx, ]
                                  }else{
                                    return(list(NULL,TRUE,FALSE, mess))
                                  }


                                  return(list(sim_tmp, flag_error, flag_rqd_res, mess))


                                }

                              }

                            }



    # Filtering situation names for keeping only the required results
    names(out) <- sit2simulate

    for (isit in required_situations) {
      if (is.null(res$sim_list[[isit]])) res$sim_list[[isit]] <- vector("list",nb_paramValues)
      if (!is.null(out[[isit]][[1]])) {
#        res$sim_list[[isit]][[ip]] <- dplyr::bind_cols(DoE=rep(ip,nrow(out[[isit]][[1]])),out[[isit]][[1]])
        res$sim_list[[isit]][[ip]] <- out[[isit]][[1]]
      }
    }


    ## A VOIR : traitement des erreurs ########################
    res$error <- any(unlist(lapply(out, function(x) return(x[[2]] || !x[[3]]))))

    # displaying warnings
    # If not an empty string
    lapply(out,function(x) stics_display_warnings(x[[4]]))

  }


  # Gather results in one tibble per sit
  for (isit in seq_along(res$sim_list)) {
    res$sim_list[[isit]] <- dplyr::bind_rows(res$sim_list[[isit]])
    if (length(res$sim_list[[isit]])==0) res$sim_list[[isit]] <- NULL
  }
  if (length(res$sim_list)==0) {
    warning("Stics simulations failed for all USMs!!!")
    res$sim_list <- NULL
  }


  # Calculating an printing duration
  if (time_display) {
    duration <- Sys.time() - start_time
    print(duration)
  }

  return(invisible(res))

}




#' @title Getting/setting a stics_wrapper options list with initialized fields
#'
#' @description This function returns a default options list if called with no arguments, or a pre-formated
#' model option list with checks on the inputs.
#'
#' @param javastics_path Path of JavaStics installation directory, needed if `stics_exe` is not provided, or relates to an exe in the `javastics_path` (see details)
#' @param stics_exe The name, executable or path of the stics executable to use (optional, default to "modulostics", see details)
#' @param data_dir Path(s) of the situation(s) input files directorie(s)
#' or the root path of the situation(s) input files directorie(s)
#' @param parallel Boolean. Is the computation to be done in parallel ?
#' @param cores    Number of cores to use for parallel computation.
#' @param time_display Display time
#' @param force    Boolean. Don't check `javastics_path`, `stics_exe` and `data_dir` (default to `FALSE`, see details)
#' @param verbose Logical value (optional), `TRUE` to display informations during execution,
#' `FALSE` otherwise (default)
#' @param successive_usms List of vectors containing the names of the UMSs to consider as successive
#' (e.g. list(c("usm1.1","usm1.2"),c("usm2.1","usm2.2")) defines 2 successions usm1.1->usm1.2 and usm2.1->usm2.2)
#' @param ... Add further arguments set the options list values
#'
#' @details `stics_exe` may be :
#' 1. a model name pointing to a stics executable as done in JavaStics, e.g. "modulostics" for `stics_modulo.exe`, the standard version of the model
#' shipping with JavaStics;
#' 2. a stics executable file available from the bin folder in JavaStics, e.g. "stics_modulo.exe";
#' 3. a path to a stics executable file, eg. "C:/Users/username/Desktop/stics.exe".
#'
#' `javastics_path` must be provided for case (1) and (2).
#'
#' If `force=TRUE`, checks are not done for `javastics_path`, `stics_exe` and `data_dir`. In this case, they are returned as is,
#' and will be checked (and potentially updated to match the right stics executable) only at execution of `stics_wrapper()`. This option
#' is used for portability, when e.g. `stics_wrapper_options` outputs are sent to a remote.
#'
#' @return A list containing Stics model stics_wrapper options
#'
#' @examples
#'
#' \dontrun{
#' # Getting simulations options and defaults values for the stics_wrapper function
#'
#' stics_wrapper_options()
#'
#' > $javastics_path
#' > [1] "unknown"
#' >
#' > $stics_exe
#' > [1] "modulostics"
#' >
#' > $data_dir
#' > [1] "unknown"
#' >
#' > $parallel
#' > [1] FALSE
#' >
#' > $cores
#' > [1] NA
#' >
#' > $time_display
#' > [1] FALSE
#' >
#' > $verbose
#' > [1] TRUE
#'
#' # Setting mandatory simulations options
#' javastics_path= "path/to/javastics"
#' data_path= "path/to/data_directory"
#' sim_options <- stics_wrapper_options(javastics_path = javastics_path, data_dir = data_path)
#'
#' # Changing default values (e.g. parallel):
#' sim_options <- stics_wrapper_options(javastics_path = javastics_path, data_dir = data_path,
#'  parallel = TRUE)
#'
#' > $javastics_path
#' > [1] "path/to/JavaSTICS-v85"
#' >
#' > $stics_exe
#' > [1] "path/to/JavaSTICS-v85/bin/stics_modulo.exe"
#' >
#' > $data_dir
#' > [1] "path/to/data"
#' >
#' > $parallel
#' > [1] TRUE
#' >
#' > $cores
#' > [1] NA
#' >
#' > $time_display
#' > [1] FALSE
#' >
#' > $verbose
#' > [1] TRUE
#'
#'  # Using the `force` argument to keep the inputs as is:
#'  sim_options <- stics_wrapper_options(javastics_path = javastics_path, data_dir = data_path,
#'   force= TRUE)
#'
#' > $javastics_path
#' > [1] "path/to/JavaSTICS-v85"
#' >
#' > $stics_exe
#' > [1] "modulostics"
#' >
#' > $data_dir
#' > [1] "path/to/data"
#' >
#' > $parallel
#' > [1] FALSE
#' >
#' > $cores
#' > [1] NA
#' >
#' > $time_display
#' > [1] FALSE
#' >
#' > $verbose
#' > [1] TRUE
#'
#' # This will be checked and modified by a `do.call()` in `stics_wrapper()`:
#' do.call(stics_wrapper_options,model_options)
#'
#' > $javastics_path
#' > [1] "path/to/JavaSTICS-v85"
#' >
#' > $stics_exe
#' > [1] "path/to/JavaSTICS-v85/bin/stics_modulo.exe"
#' >
#' > $data_dir
#' > [1] "path/to/data"
#' >
#' > $parallel
#' > [1] FALSE
#' >
#' > $cores
#' > [1] NA
#' >
#' > $time_display
#' > [1] FALSE
#' >
#' > $verbose
#' > [1] TRUE
#'
#' # Note the `stics_exe` path that was modified and checked to the path were it was found.
#' }
#'
#' @export
stics_wrapper_options <- function(javastics_path = NULL,
                                  stics_exe= "modulostics",
                                  data_dir = NULL,
                                  parallel= FALSE,
                                  cores= NA,
                                  time_display= FALSE,
                                  verbose= TRUE,
                                  force= FALSE,
                                  successive_usms=NULL,
                                  ... ) {

  options <- list()
  # To get a template, run the function without arguments:
  if(!nargs()){
    # Template list
    options$javastics_path <- "unknown"
    options$stics_exe <- "unknown"
    options$data_dir <- "unknown"
    options$parallel <- FALSE
    options$cores <- NA
    options$time_display <- FALSE
    options$verbose <- TRUE
    options$successive_usms <- NULL
    return(options)
  }

  if(force){
    # Forced, no checks on the arguments.
    options$javastics_path <- javastics_path
    options$stics_exe <- stics_exe
    options$data_dir <- data_dir
    options$parallel <- parallel
    options$cores <- cores
    options$time_display <- time_display
    options$verbose <- verbose
    options$successive_usms <- successive_usms
    return(options)
  }

  if(is.null(data_dir)){
    stop("The data_dir argument is mandatory")
  }

  # Help people that don't remember well the standard name:
  if(stics_exe=="stics_modulo"|stics_exe=="sticsmodulo"){
    stics_exe= "modulostics"
  }

  # Getting right executable name for the platform
  if(stics_exe=="modulostics"){
    # using the exe name instead of the identifier to select the right one for the user's OS
    stics_exe= paste0("stics_modulo",os_suffix())
  }

  if(!is.null(javastics_path)){
    # Checking javastics path if present
    check_java_path(javastics_path)
  }

  # Case 1: stics_exe is a model name present in the preference file:
  if(!is.null(javastics_path) && exist_stics_exe(javastics_path, stics_exe)){
    stics_exe= file.path(javastics_path,list_stics_exe(javastics_path)$stics_list[stics_exe][[1]])
  }else if(!is.null(javastics_path) &&
           check_stics_exe(model_path = file.path(javastics_path, "bin", basename(stics_exe)), stop = FALSE)){
    # Case 2: stics_exe is an executable from the bin directory in JavaStics:
    stics_exe= file.path(javastics_path, "bin", basename(stics_exe))
  }else if(!check_stics_exe(model_path = stics_exe, stop = FALSE)){
    # Case were stics_exe was not found in case 1 and 2, and is not a valid path to an executable either:
    stop("stics_exe was not found as a stics name, executable in the bin path of JavaStics nor executable path: ",
         stics_exe)
    # NB: case 3 (i.e. stics_exe is a full path to an executable) is implicit here: it is the case where
    # check_stics_exe(model_path = stics_exe, stop = FALSE) == TRUE
  }

  if(verbose) cli::cli_alert_success("Using stics: {.val {stics_exe}}")

  # Adding arguments values to the option list:
  if(!is.null(javastics_path)) options$javastics_path <- javastics_path
  if(!is.null(stics_exe)) options$stics_exe <- stics_exe
  if(!is.null(data_dir)) options$data_dir <- data_dir
  if(!is.null(parallel)) options$parallel <- parallel
  if(!is.null(cores)) options$cores <- cores
  if(!is.null(time_display)) options$time_display <- time_display
  if(!is.null(verbose)) options$verbose <- verbose
  if(!is.null(successive_usms)) options$successive_usms <- successive_usms

  # Adding future-proof optional fields:
  dot_args <- list(...)
  options= c(options,dot_args)

  return(options)
}


stics_display_warnings <- function(in_string) {
  # print(in_string)
  # print(length(in_string))
  if (nchar(in_string) ) warning(in_string, call. = FALSE)
}


#' Set the output variables needed
#'
#' @param filepath The path to the "var.mod" file.
#' @param vars     The variables needed
#' @param add      Do we want to add or append the variables to existing variables ?
#'
#' @note This is exactly the same function as `[sticsRFiles::set_out_var_txt()]`, but we add it here
#' to avoid a dependency just for a one-liner function.
#'
#' @return Nothing, write in the "var.mod" file
#' @keywords internal
#'
set_out_var_internal= function(filepath="var.mod",vars=c("lai(n)","masec(n)"),add= F){
  cat(vars,file=filepath, sep="\n",append = add)
}
