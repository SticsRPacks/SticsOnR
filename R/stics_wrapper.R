#' @title Running usm(s) from txt input files stored in one directory
#' per `situation`, simulated results are returned in a list
#'
#' @description This function uses Stics directly through a system call, can
#' force Stics input parameters with values given in arguments.
#'
#' @param model_options List containing any information needed by the model.
#' In the case of Stics: `stics_path` the path of Stics executable file and
#' `data_dir` the path of the directory containing the Stics input data
#' for each USM (one folder per USM where Stics input files are stored in txt
#' format)
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
#' simulations should be compared as provided by SticsRFiles::read_obs
#'
#' @return A list containing simulated values (`sim_list`: a vector of list (one
#' element per values of parameters) containing usms outputs data.frames) and an
#' error code (`error`) indicating if at least one simulation ended with an
#' error. Inter-crops are not yet taken into account for extracting output
#' data from files.
#'
#'
#' @examples
#'
#' \dontrun{
#'
#' # Specifying the JavaStics folder
#' javastics_path <- "/path/to/javastics"
#'
#' # Setting the model executable path (windows, linux)
#' stics_path <- file.path(javastics_path, "bin","stics_modulo")
#'
#' # Setting the input data folder path, root directory of the usms directories
#' data_path <- "/path/to/usms/subdirs/root"
#'
#' # Setting the mandatory simulations options
#' sim_options <- stics_wrapper_options(stics_path = stics_path,
#' data_dir = data_path)
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
                          sit_var_dates_mask = NULL) {

  # TODO LIST
  #    - maybe the variables asked will not be simulated (depends on the
  #      var.mod file ...)
  #         => try to simulate, if some variables are not simulated : modify the
  #            var.mod and re-simulate (should be more efficient than checking
  #            everytime the var.mod)
  #           maybe test the size of nb UMS / DOE size to possibly adapt the
  #           strategy (e.g. in case of multisimulation case with millions of
  #           USMs the strategy describe here-before is not very good ...)
  #           Another alternative: build a function that generates the var.mod,
  #           the user can call before main_optim
  #         For the moment, just warn and ask to change var.mod
  #    - handle the case of stages (stages should be specified in the var.mod ...
  #      + handle the case when simulations does not reach the asked stages ...)
  #    - handle the case of intercrop
  #

  # Preliminary model checks ---------------------------------------------------

  # Checking model_options content, stopping when mandatory
  # values are not set or not valid
  stics_wrapper_options(in_options = model_options)

  # Getting list values into separated variables
  stics_path <- model_options$stics_path
  data_dir <- model_options$data_dir
  parallel <- model_options$parallel
  cores <- model_options$cores
  time_display <- model_options$time_display
  warning_display <- model_options$warning_display

  # Checking Stics executable
  check_stics(stics_path)

  if (time_display)   start_time <- Sys.time()

  # Managing parallel model simulations
  # Managing cores number to use
  cores_nb <- get_cores_nb( parallel = parallel, required_nb = cores )

  # Launching the cluster
  cl <- parallel :: makeCluster(cores_nb)
  doParallel::registerDoParallel(cl)


  # Run Stics and store results ------------------------------------------------

  ## Define the list of usm to simulate
  # This part is a bit complex since stics_wrapper has been designed to be used both by CroptimizR
  # and by the user so different options have been implemented (param_values can be
  # a vector or an array, sit_var_dates_mask can be a list or an array).
  keep_all_data <- FALSE # to specify if all simulated variables and dates must be returned
  if (base::is.null(sit_var_dates_mask)) {
    keep_all_data <- TRUE
  } else if (base::is.list(sit_var_dates_mask)) {
    sit_names_mask <- names(sit_var_dates_mask)
  } else if (base::is.character(sit_var_dates_mask)) {
    sit_names_mask <- sit_var_dates_mask
    keep_all_data <- TRUE
  }

  # Default behavior if param_values or sit_var_dates_mask don't provide situation list
  situation_names <- list.dirs(data_dir, full.names = TRUE)[-1]
  if (length(situation_names) == 0) {
    stop(paste("Not any Stics directories found in:",data_dir))
  }

  # Checking existing files
  files_exist <- file.exists(file.path(situation_names, "new_travail.usm"))
  situation_names <- base::basename(situation_names)[files_exist]

  # may be overwritten here ...
  if (is.null(param_values)) {
    if (!base::is.null(sit_var_dates_mask)) { situation_names <- sit_names_mask }
  } else if (is.vector(param_values)) {
    if (!base::is.null(sit_var_dates_mask)) { situation_names <- sit_names_mask }
    # transform param_values into an 3D-array
    param_values=array(param_values,
                       dim=c(1,length(param_values),length(situation_names)),
                       dimnames=list("NULL",names(param_values),situation_names))
  } else if (is.array(param_values)) {
    situation_names <- dimnames(param_values)[[3]]
    if ( !base::is.null(sit_var_dates_mask) &&
         length(setdiff(situation_names, sit_names_mask))>0) {
      base::warning(paste("Situations in param_values and sit_var_dates_mask are different:",
                    "\n \t Situations param_values:",paste(situation_names,collapse=" "),
                    "\n \t Situations sit_var_dates_mask:",paste(sit_names_mask,collapse=" "),
                    "\n Situations defined in param_values will be simulated."))
    }
  }

  # Default output data list
  nb_paramValues=1
  if (!base::is.null(param_values)) {
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
  if (!any(dirs_exist)) {
    base::warning(paste("Not any existing folders in\n",data_dir,", aborting !"))
    return(res)
  }

  # Some dirs do not exist
  if (!all(dirs_exist)) {
    base::warning(paste("Folder(s) does(do) not exist",
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
    out <- foreach::foreach(i = seq_along(dirs_idx),
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
                              if (base::is.null(param_values)) {
                                # remove param.sti in case of previous run using it ...
                                if (base::suppressWarnings(file.remove(file.path(run_dir,
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
                                  mess <- base::warning(paste("Error when generating the forcing parameters file for USM",situation,
                                                        ". \n "))
                                  return(list(NA,TRUE,FALSE,mess))
                                }


                                SticsRFiles:::set_codeoptim(run_dir, value=1)
                              }

                              # TODO: check or set the flagecriture to 15 to get daily data results !!!

                              ########################################################################
                              # TODO: and call it in/ or integrate parameters forcing in run_system function !
                              ## Run the model & forcing not to check the model executable
                              usm_out <- SticsOnR::run_stics(stics_path, run_dir, check_exe = FALSE)

                              # if the model returns an error, ... treating next situation
                              if ( usm_out[[1]]$error ) {

                                mess <- base::warning(paste("Error running the Stics model for USM",situation,
                                                      ". \n ",usm_out[[1]]$message))
                                return(list(NA,TRUE,FALSE,mess))
                              }

                              ## Otherwise, getting results
                              sim_tmp=SticsRFiles::get_daily_results(file.path(data_dir, situation),
                                                                     situation)
                              # Any error reading output file
                              if (base::is.null(sim_tmp)) {
                                mess <- base::warning(paste("Error reading outputs for ",situation,
                                                      ". \n "))
                                return(list(NA, TRUE, FALSE, mess))

                              }


                              # Keeping all outputs data
                              # - If no sit_var_dates_mask given as input arg
                              # - if only usm names given in sit_var_dates_mask (vector)
                              # - If all output variables are in
                              #   sit_var_dates_mask[[situation]]

                              # Nothing to select, returning all data
                              if ( keep_all_data ) {

                                return(list( sim_tmp,FALSE, TRUE, mess))

                              } else { # Selecting variables from sit_var_dates_mask

                                var_list=colnames(sit_var_dates_mask[[situation]])
                                out_var_list <- colnames(sim_tmp)

                                # Keeping only the needed variables in the simulation results
                                vars_idx= out_var_list %in% var_list

                                # Checking variables
                                # Common variables
                                inter_vars <- out_var_list[vars_idx]

                                # Indicating that variables are not simulated, adding them before simulating
                                if (length(inter_vars) < length(var_list)) {
                                  mess <- base::warning(paste("Variable(s)",paste(setdiff(var_list,inter_vars), collapse=", "),
                                                        "not simulated by the Stics model for USM",situation,
                                                        "=> try to add it(them) in",file.path(data_dir,situation,"var.mod")))
                                  flag_error <- FALSE
                                  flag_rqd_res <- FALSE
                                }

                                if (any(vars_idx)) {
                                  sim_tmp=sim_tmp[ , vars_idx]
                                } else {
                                  mess <- base::warning(paste("Not any variable simulated by the Stics model for USM", situation,
                                                        "=> they must be set in",file.path(data_dir,situation,"var.mod")))
                                  return(list(NA, TRUE, FALSE, mess))
                                }

                                ## Keeping only the needed dates in the simulation results
                                date_list=sit_var_dates_mask[[situation]]$Date
                                dates_idx <- sim_tmp$Date %in% date_list

                                # Checking dates
                                # Common dates
                                inter_dates <- sim_tmp$Date[dates_idx]

                                if ( length(inter_dates) < length(date_list) ) {
                                  missing_dates <- date_list[!date_list %in% inter_dates]
                                  mess <- base::warning(paste("Requested date(s)",paste(missing_dates, collapse=", "),
                                                        "is(are) not simulated for USM",situation))
                                  flag_error <- FALSE
                                  flag_rqd_res <- FALSE
                                }

                                # Filtering needed dates lines
                                if ( any(dates_idx) ) {
                                  sim_tmp <- sim_tmp[dates_idx, ]
                                } else {
                                  return(list(NA,TRUE,FALSE, mess))
                                }
                                return(list(sim_tmp, flag_error, flag_rqd_res, mess))

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

  # Stopping the cluster
  parallel :: stopCluster(cl)

  # Calculating an printing duration
  if (time_display) {
    duration <- Sys.time() - start_time
    print(duration)
  }

  return(invisible(res))

}




#' @title Getting/setting a stics_wrapper options list with initialized fields,
#' or validating an existing list
#'
#' @description This function returns a default options list, or allow to set
#' list elements values or validate the input options list elements values
#' (only the mandatories ones)
#'
#' @param stics_path Path of the Stics binary executable file (delivered with
#' JavaStics interface)
#'
#' @param data_dir Path(s) of the situation(s) input files directorie(s)
#' or the root path of the situation(s) input files directorie(s)
#'
#' @param in_options An existing options list, for updating elements
#'
#' @param ... Add further arguments set the options list values
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
#' #> $stics_path
#' #> "/path/to/javastics/bin/stics_modulo"
#' #>
#' #> $data_dir
#' #> "/path/to/usms/subdirs/root"
#' #>
#' #> $parallel
#' #> [1] FALSE
#' #>
#' #> $cores
#' #> [1] NA
#' #>
#' #> $time_display
#' #> [1] FALSE
#' #>
#' #> $warning_display
#' #> [1] TRUE
#'
#' # Setting mandatory simulations options
#' sim_options <- stics_wrapper_options(stics_path = stics_path, data_dir = data_path)
#'
#' # Setting other options using option name as a function argument
#' # Example for activating parallel simulations
#' sim_options <- stics_wrapper_options(stics_path = stics_path,
#' data_dir = data_path, parallel = TRUE)
#'
#' #> $stics_path
#' #> "/path/to/javastics/bin/stics_modulo"
#' #>
#' #> $data_dir
#' #> "/path/to/usms/subdirs/root"
#' #>
#' #> $parallel
#' #> [1] TRUE
#' #>
#' #> $cores
#' #> [1] NA
#' #>
#' #> $time_display
#' #> [1] FALSE
#' #>
#' #> $warning_display
#' #> [1] TRUE
#'
#' }
#'
#' @export
#'
stics_wrapper_options <- function(stics_path = NULL,
                                  data_dir = NULL,
                                  in_options = NULL,
                                  ... ) {

  # Getting the input options list
  options <- in_options

  # Template list
  if (base::is.null(options)) {
    options <- list()
    options$stics_path <- "unknown"
    options$data_dir <- "unknown"
    options$parallel <- FALSE
    options$cores <- NA
    options$time_display <- FALSE
    options$warning_display <- TRUE
  }

  # For getting the template
  # running stics_wrapper_options
  if (! nargs()) return(options)


  # For fixing mandatory fields values
  if (!base::is.null(stics_path)) options$stics_path <- stics_path
  if (!base::is.null(data_dir)) options$data_dir <- data_dir

  # Fixing optional fields,
  # if corresponding to exact field names
  # in options list
  list_names <- names (options)
  add_args <- list(...)

  for (n in names(add_args)) {
    if ( n %in% list_names) {
      options[[n]] <- add_args[[n]]
    }
  }

  # Checking mandatory fields
  missing_opts <- c(options$stics_path, options$data_dir ) == "unknown"
  if (base::any(missing_opts)) {
    stop("Mandatory option(s) is/are missing: ",
         paste(c("stics_path", "data_dir")[missing_opts],
               collapse = ", "))
  }

  # Checking paths
  dirs <- c(options$stics_path, options$data_dir)

  # Checking if dirs is a character vector
  if (!all(base::is.character(dirs))) {
    stop("stics_path and/or data_dir are/is not path(s) !")
  }

  # Checking if paths exist
  exist_dirs <- file.exists(dirs)
  if (!all(exist_dirs)) {
    stop(paste("Mandatory path(s)",c("stics_path", "data_dir")[!exist_dirs],
               collapse = "\n  ")," does(do) not exist: \n",
         paste(dirs[!exist_dirs], collapse = "\n"))
  }

  return(options)
}


stics_display_warnings <- function(in_string) {
  # print(in_string)
  # print(length(in_string))
  if (nchar(in_string) ) base::warning(in_string, call. = FALSE)
}
