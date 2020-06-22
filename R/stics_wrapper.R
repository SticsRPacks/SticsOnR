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
#' @param param_values (optional) either a named vector or a named 3D array.
#' Use a named vector that contains the values and names of the parameters
#' to force the same values of the parameters whatever the simulated
#' situations (usms). If one wants to force the model with different values
#' of parameters for the simulated situations or to simulate the situations
#' several times but with different values of the parameters, use a 3D array
#' containing the value(s) and names of the parameters to force for each
#' situation to simulate. This array contains the different parameters
#' values (first dimension) for the different parameters (second dimension)
#' and for the different situations (third dimension).
#' See examples for more details.
#'
#' @param sit_var_dates_mask (optional) List of situations:
#' may be either a character vector of situation names or a named list
#' containing information about variables and dates for which simulated values
#' should be returned. Typically a list containing the observations to which
#' simulations should be compared as provided by SticsRFiles::get_obs
#'
#' @return A list containing simulated values (`sim_list`: a vector of list (one
#' element per values of parameters) containing usms outputs data.frames) and an
#' error code (`error`) indicating if at least one simulation ended with an
#' error. Inter-crops are not yet taken into account for extracting output
#' data from files.
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
                          sit_var_dates_mask = NULL){

  # TODO LIST
  #    - handle the case of stages (stages should be specified in the var.mod ...
  #      + handle the case when simulations does not reach the asked stages ...)
  #    - handle the case of intercrop
  #

  # Stopping the cluster when exiting
  on.exit(parallel::stopCluster(cl))

  # Preliminary model checks ---------------------------------------------------

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

  if(time_display) start_time <- Sys.time()

  # Managing parallel model simulations
  # Managing cores number to use
  cores_nb <- get_cores_nb( parallel = parallel, required_nb = cores )

  # Launching the cluster
  cl <- parallel::makeCluster(cores_nb)
  doParallel::registerDoParallel(cl)

  parallel::clusterCall(cl, function(x) .libPaths(x), .libPaths())

  # Run Stics and store results ------------------------------------------------

  ## Define the list of usm to simulate
  # This part is a bit complex since stics_wrapper has been designed to be used both by CroptimizR
  # and by the user so different options have been implemented (param_values can be
  # a vector or an array, sit_var_dates_mask can be a list or an array).
  keep_all_data <- FALSE # to specify if all simulated variables and dates must be returned
  if(is.null(sit_var_dates_mask)){
    keep_all_data <- TRUE
  }else if(is.list(sit_var_dates_mask)){
    sit_names_mask <- names(sit_var_dates_mask)
  }else if(is.character(sit_var_dates_mask)){
    sit_names_mask <- sit_var_dates_mask
    keep_all_data <- TRUE
  }

  # Default behavior if param_values or sit_var_dates_mask don't provide situation list
  situation_names <- list.dirs(data_dir, full.names = TRUE)[-1]
  if(length(situation_names) == 0){
    stop(paste("Not any Stics directories found in:",data_dir))
  }

  # Checking existing files
  files_exist <- file.exists(file.path(situation_names, "new_travail.usm"))
  situation_names <- basename(situation_names)[files_exist]

  # may be overwritten here ...
  if(is.null(param_values)){
    if(!is.null(sit_var_dates_mask)){
      situation_names <- sit_names_mask
    }
  }else if(is.vector(param_values)){
    if(!is.null(sit_var_dates_mask)){
      situation_names <- sit_names_mask
    }
    # transform param_values into an 3D-array
    param_values=array(param_values,
                       dim=c(1,length(param_values),length(situation_names)),
                       dimnames=list("NULL",names(param_values),situation_names))
  }else if(is.array(param_values)){
    situation_names <- dimnames(param_values)[[3]]
    if(!is.null(sit_var_dates_mask) && length(setdiff(situation_names, sit_names_mask))>0) {
      warning(paste("Situations in param_values and sit_var_dates_mask are different:",
                    "\n \t Situations param_values:",paste(situation_names,collapse=" "),
                    "\n \t Situations sit_var_dates_mask:",paste(sit_names_mask,collapse=" "),
                    "\n Situations defined in param_values will be simulated."))
    }
  }

  # If successive_usms, check that all usms are in the list, otherwise, add the missing ones
  # and order them
  situation_names <- c(unlist(successive_usms), setdiff(situation_names,unlist(successive_usms)))

  # Default output data list
  nb_paramValues=1
  if (!is.null(param_values)) {
    nb_paramValues=dim(param_values)[1]
  }
  res <- list()
  res$error <- FALSE
  res$sim_list <- vector("list",nb_paramValues)


  # Calculating directories list
  run_dirs <- file.path(data_dir,situation_names)

  # Checking if dirs exist
  dirs_exist <- file.exists(run_dirs)

  # Not any existing dir
  # Exiting the function, returning a list with no data
  if(!any(dirs_exist)){
    warning(paste("Not any existing folders in\n",data_dir,", aborting !"))
    return(res)
  }

  # Some dirs do not exist
  if (!all(dirs_exist)) {
    warning(paste("Folder(s) does(do) not exist",
                  "in data_dir\n",data_dir,":\n => ",
                  paste(situation_names[!dirs_exist], collapse = "\n => ")))
  }

  # Getting existing dir index list
  dirs_idx <- which(dirs_exist)

  for(ip in 1:nb_paramValues) {

    ## Loops on the USMs that can be simulated
    ## out is a list containing: the list of simulated outputs,
    ##  a flag TRUE if the requested simulation has been not performed (model error),
    ##  a flag FALSE if all the requested dates and variables were not simulated,
    ##  a message in case of warning or error

    # initialization not a global variable !
    i <- 1
    out <- foreach::foreach(i = seq_along(dirs_idx),.export = "run_stics",
                            .packages = c("SticsRFiles")) %dopar% {

                              # Simulation flag status or output data selection status
                              flag_error <- FALSE
                              flag_rqd_res <- TRUE
                              iusm <- dirs_idx[i]
                              run_dir <- run_dirs[iusm]
                              situation <- situation_names[iusm]
                              mess <- ""
                              ########################################################################
                              # TODO: make a function dedicated to forcing parameters of the model ?
                              # In that case by using the param.sti mechanism
                              ## Force param values
                              if (is.null(param_values)) {
                                # remove param.sti in case of previous run using it ...
                                if (suppressWarnings(file.remove(file.path(run_dir,
                                                                           "param.sti")))) {
                                  SticsRFiles:::set_codeoptim(run_dir,value=0)
                                }

                              } else {
                                # TODO: handle the case NA for a sublist of parameters (in case one want to force some parameters for some USMs and others for other USMs)
                                param_values_usm=param_values[ip,,situation_names[iusm]]
                                names(param_values_usm)=colnames(param_values)

                                ret <- SticsRFiles::gen_paramsti(run_dir, names(param_values_usm), param_values_usm)

                                # if writing the param.sti fails, treating next situation
                                if ( ! ret ) {
                                  if(verbose) cli::cli_alert_warning("Error when generating the forcing parameters file for USM {.val {situation}}")
                                  return(list(NA,TRUE,FALSE, mess))
                                }


                                SticsRFiles:::set_codeoptim(run_dir, value=1)
                              }

                              # Handling successive USMs (if the usm is part of the list and not in first position ...)
                              is_succ <- any(sapply(successive_usms,function(x) match(situation_names[iusm],x))>=2)
                              if (!is.na(is_succ) && is_succ) {
                                if (file.exists(file.path(run_dirs[iusm-1],"recup.tmp"))) {
                                  file.copy(from=file.path(run_dirs[iusm-1],"recup.tmp"),
                                            to=file.path(run_dir,"recup.tmp"),overwrite = TRUE)
                                } else {
                                  mess <- warning(paste("Error running the Stics model for USM",situation,
                                                        ". \n This USMs is part of a succession but recup.tmp file was not created by the previous USM."))
                                  return(list(NA,TRUE,FALSE, mess))
                                }
                                # The following could be done only once in case of repeated call to the wrapper (e.g. parameters estimation ...)
                                SticsRFiles::set_param_txt(dirpath = run_dir, param="codesuite", value=1)
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
                                  SticsRFiles::set_param_txt(dirpath = run_dir, param="codesuite", value=0)
                                }

                                # if the model returns an error, ... treating next situation
                                if(usm_out[[1]]$error){
                                  mess <- warning(paste("Error running the Stics model for USM",situation,
                                                        ". \n ",usm_out[[1]]$message))
                                  return(list(NA,TRUE,FALSE, mess))
                                }

                                # Get the number of plants to know whether it is a sole crop or an intercrop:
                                nbplants= as.numeric(SticsRFiles::get_usm_txt(filepath = file.path(data_dir,situation,"new_travail.usm"))$nbplantes)
                                mixed <- nbplants > 1

                                ## Otherwise, getting results
                                sim_tmp= SticsRFiles::get_daily_results(file.path(data_dir, situation),
                                                                        situation, mixed= mixed)[[1]]
                                # Any error reading output file
                                if(is.null(sim_tmp)){
                                  if(verbose) cli::cli_alert_warning("Error reading outputs for usm: {.val {situation}}")
                                  return(list(NA,TRUE,FALSE, mess))
                                }


                                # Keeping all outputs data
                                # - If no sit_var_dates_mask given as input arg
                                # - if only usm names given in sit_var_dates_mask (vector)
                                # - If all output variables are in
                                #   sit_var_dates_mask[[situation]]

                                # Nothing to select, returning all data
                                if(keep_all_data){
                                  return(list(sim_tmp,FALSE, TRUE, mess))
                                } else { # Selecting variables from sit_var_dates_mask

                                  var_list= colnames(sit_var_dates_mask[[situation]])
                                  # Remove the optional "Plant" variable from observations
                                  var_list= var_list[!grepl("Plant",var_list)]

                                  out_var_list <- colnames(sim_tmp)

                                  # Keeping only the needed variables in the simulation results
                                  vars_idx= out_var_list %in% var_list

                                  # Checking variables
                                  # Common variables
                                  inter_vars <- out_var_list[vars_idx]

                                  # Indicating that variables are not simulated, adding them and re-simulate
                                  if(length(inter_vars) < length(var_list)){
                                    diff_vars= setdiff(var_list,inter_vars)

                                    if(varmod_modified){
                                      if(verbose) cli::cli_alert_warning(paste("{cli::qty(diff_vars)} Variable{?s} {.val {diff_vars}} found in {.code sit_var_dates_mask}",
                                                                               "not simulated by the Stics model for USM {.val {situation}}"))
                                      flag_error <- FALSE
                                      flag_rqd_res <- FALSE
                                    }else{
                                      # Remove the dummy variables output from STICS but not input:
                                      out_var_list= out_var_list[!out_var_list %in% c("Date","ian","mo","jo","jul","cum_jul")]

                                      # valid STICS vars not simulated yet:
                                      is_diff_vars_valid= SticsRFiles::is_stics_var(diff_vars)

                                      # Using only valid STICS variables:
                                      out_var_list= c(out_var_list,diff_vars[is_diff_vars_valid])

                                      if(!any(is_diff_vars_valid) && verbose){
                                        cli::cli_alert_warning(paste("{cli::qty(diff_vars[!is_diff_vars_valid])} Found variable{?s} not valid for STICS: {.val {diff_vars[!is_diff_vars_valid]}} in {.code sit_var_dates_mask}",
                                                                     "for USM {.val {situation}}. Will not simulate {?it/them}."))
                                      }

                                      if(verbose){
                                        varmod_file= file.path(data_dir,situation,"var.mod")
                                        cli::cli_alert_warning(paste("{cli::qty(diff_vars[is_diff_vars_valid])} Variable{?s} {.val {diff_vars[is_diff_vars_valid]}} found in {.code sit_var_dates_mask}",
                                                                     "but not simulated by STICS in the current config for USM {.val {situation}}"))
                                        cli::cli_alert_success("Updating {.val {varmod_file}} to include the variable as model output, and re-running STICS.")
                                      }

                                      SticsRFiles::gen_varmod(workspace = file.path(data_dir,situation),var_names = out_var_list)
                                      varmod_modified=TRUE
                                      next()
                                    }
                                  }

                                  if(any(vars_idx)){
                                    sim_tmp= sim_tmp[ , vars_idx]
                                  }else{

                                    if(verbose){
                                      cli::cli_alert_warning(paste("{cli::qty(var_list)} Variable{?s} {.val {var_list}} found in {.code sit_var_dates_mask}",
                                                                   "for USM {.val {situation}}, are not valid STICS variables."))
                                    }

                                    return(list(NA,TRUE,FALSE, mess))
                                  }

                                  ## Keeping only the needed dates in the simulation results
                                  date_list= sit_var_dates_mask[[situation]]$Date
                                  dates_idx <- sim_tmp$Date %in% date_list

                                  # Checking dates
                                  # Common dates
                                  inter_dates <- sim_tmp$Date[dates_idx]

                                  if ( length(inter_dates) < length(date_list) ) {
                                    missing_dates <- date_list[!date_list %in% inter_dates]
                                    if(verbose){
                                      cli::cli_alert_warning(paste("{cli::qty(missing_dates)} Requested date{?s} {.val {missing_dates}} found in {.code sit_var_dates_mask}",
                                                                   "for USM {.val {situation}} {?is/are} not simulated by STICS in the current configuration."))
                                    }
                                    flag_error <- FALSE
                                    flag_rqd_res <- FALSE
                                  }

                                  # Filtering needed dates lines
                                  if(any(dates_idx)){
                                    sim_tmp <- sim_tmp[dates_idx, ]
                                  }else{
                                    return(list(NA,TRUE,FALSE, mess))
                                  }
                                  return(list(sim_tmp, flag_error, flag_rqd_res, mess))
                                }

                              }

                            }


    # TODO: optimize res generation without copying out elements !
    # Formatting the output list

    # Filtering situation names, existing dirs !
    names(out) <- situation_names[dirs_exist]

    # for calculating allsim status
    # for selecting output data.frame from the list
    sel_idx <- unlist(lapply(out, function(x) return(!x[[2]])))

    res$sim_list[[ip]] <- lapply(out[sel_idx], function(x) return(x[[1]]))
    res$error <- any(unlist(lapply(out, function(x) return(x[[2]] || !x[[3]]))))

    # displaying warnings
    # If not an empty string
    lapply(out,function(x) stics_display_warnings(x[[4]]))

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
