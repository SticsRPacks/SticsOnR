#' @title Running usm(s) from txt input files stored in one directory
#' per `situation`, simulated results are returned in a list
#'
#' @description This function uses Stics directly through a system call, can
#' force Stics input parameters with values given in arguments.
#'
#' @param param_values a named vector containing the value(s) and names of the
#' parameters to force (optional). It may contains either unique value for each
#' parameter name or multiple values for each parameter when `prior_information`
#' argument is provided.
#'
#' @param sit_var_dates_mask List of situations:
#' may be either a character vector of situation names or a named list containing
#' information about variables and dates for which simulated values should be returned.
#' Typically a list containing the observations to which simulations should be
#' compared as provided by SticsRFiles::read_obs
#'
#' @param prior_information Prior information on the parameters to estimate.
#' For the moment only uniform distribution are allowed.
#' Either a list containing (named) vectors of upper and lower
#' bounds (`ub` and `lb`), or a named list containing for each
#' parameter the list of situations per group (`sit_list`)
#' and the vector of upper and lower bounds (one value per group) (`ub` and `lb`)
#'
#' @param model_options List containing any information needed by the model.
#' In the case of Stics: `stics_path` the path of Stics executable file and
#' `data_dir` the path of the directory containing the Stics input data
#' for each USM (one folder per USM where Stics input files are stored in txt
#' format)
#'
#' @return A list containing simulated values (`sim_list`) and a flag
#' (`flag_allsim`) indicating if all required situations, variables and
#' dates were simulated.
#'
#' @export
#'
#' @importFrom foreach %dopar%
#'
stics_wrapper <- function( param_values = NULL,
                           sit_var_dates_mask = NULL,
                           prior_information = NULL,
                           model_options ) {

  # TODO LIST
  #    - param_values may be a df (SA case, ...)
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


  ##############################################################################
  # TODO : make a function dedicated to checking model_options
  # Because it may be model dependant, so it could be possible to pass anything
  # usefull in the model running function...
  # Reuse next lines before `Run Stics` block
  ## check presence of mandatory information in model model_options list
  if (base::is.null(model_options$stics_path) || base::is.null(model_options$data_dir)) {
    stop("stics_path and data_dir should be elements of the model_model_options
    list for the Stics model")
  }
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
  cores_nb <- 1
  if ( parallel ) {
    if (is.na(cores)) {
      cores_nb <- parallel :: detectCores() - 1
    } else {
      cores_nb <- cores
    }
  }

  # Launching the cluster
  cl <- parallel :: makeCluster(cores_nb)
  doParallel::registerDoParallel(cl)


  # Run Stics and store results ------------------------------------------------

  # Checking if all data for all situations will be kept or not
  keep_all_data <- FALSE
  if (base::is.null(sit_var_dates_mask)) keep_all_data <- TRUE

  # Getting situations names from list
  # (from dir names or sit_var_dates_mask fields names)
  if (keep_all_data) {
    situation_names <- list.dirs(data_dir, full.names = FALSE)[-1]
    situation_names <- situation_names[sapply(situation_names,
                                              function(x) file.exists(file.path(data_dir,x,"new_travail.usm")))]
  } else {
    if (base::is.list(sit_var_dates_mask)) situation_names <- names(sit_var_dates_mask)
  }

  # Getting situation names from characters vector, no selection in outputs
  if (base::is.character(sit_var_dates_mask)) {
    situation_names <- sit_var_dates_mask
    keep_all_data <- TRUE
  }


  # Calculating directories list
  run_dirs <- file.path(data_dir,situation_names)
  # print(run_dirs)


  ## If data not provided for the required USM
  dirs_exist <- file.exists(run_dirs)
  flag_allsim <- TRUE
  if (!all(dirs_exist)) {
    warning(paste("No folder provided for USM(s)",paste(situation_names[!dirs_exist], collapse = ", "),
                  "in data_dir",data_dir))
    flag_allsim <- FALSE
  }

  dirs_idx <- which(dirs_exist)
  res <- list()
  i <- 1
  ## Loops on the USMs that can be simulated
  out <- foreach::foreach(i = 1:length(dirs_idx),
                          .export = c("run_system"),
                          .packages = c("SticsRFiles","foreach", "CroptimizR")) %dopar% {

                            # Simulation flag status or output data selection status
                            flag_sim <- TRUE
                            select_sim <- TRUE
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
                              if (suppressWarnings(file.remove(file.path(run_dir,
                                                                         "param.sti")))) {
                                SticsRFiles:::set_codeoptim(run_dir,value=0)
                              }

                            } else {
                              # TODO: if the usm name is not in usms groups
                              # param_values_usm == NULL(modify get_params_per_sit)
                              # do not generate the param.sti file and do not set codeoptim to 1
                              param_values_usm= CroptimizR:::get_params_per_sit(prior_information,situation_names[iusm],param_values)

                              ret <- SticsRFiles::gen_paramsti(run_dir, names(param_values_usm), param_values_usm)

                              # if writing the param.sti fails, treating next situation
                              if ( ! ret ) {
                                mess <- warning(paste("Error when generating the forcing parameters file for USM",situation,
                                                      ". \n "))
                                return(list(NA,FALSE,FALSE,mess))
                              }


                              SticsRFiles:::set_codeoptim(run_dir, value=1)
                            }

                            # TODO: check or set the flagecriture to 15 to get daily data results !!!

                            ########################################################################
                            # TODO: and call it in/ or integrate parameters forcing in run_system function !
                            ## Run the model & forcing not to check the model executable
                            usm_out <- run_system(stics_path, run_dir, check_exe = FALSE)

                            # if the model returns an error, ... treating next situation
                            if ( usm_out[[1]]$error ) {

                              mess <- warning(paste("Error running the Stics model for USM",situation,
                                                    ". \n ",usm_out[[1]]$message))
                              return(list(NA,FALSE,FALSE,mess))
                            }

                            ## Otherwise, getting results
                            sim_tmp=SticsRFiles::get_daily_results(file.path(data_dir, situation),
                                                                   situation)
                            # Any error reading output file
                            if (base::is.null(sim_tmp)) {
                              mess <- warning(paste("Error reading outputs for ",situation,
                                                    ". \n "))
                              return(list(NA, FALSE, FALSE, mess))

                            }


                            # Keeping all outputs data
                            # - If no sit_var_dates_mask given as input arg
                            # - if only usm names given in sit_var_dates_mask (vector)
                            # - If all output variables are in
                            #   sit_var_dates_mask[[situation]]

                            # Nothing to select, returning all data
                            if ( keep_all_data ) {
                              return(list( sim_tmp,TRUE, TRUE, mess))
                            }


                            # Selecting variables from sit_var_dates_mask
                            if ( !base::is.null(sit_var_dates_mask) &&
                                 situation %in% situation_names) {
                              var_list=colnames(sit_var_dates_mask[[situation]])
                              out_var_list <- colnames(sim_tmp)
                            }

                            # Keeping only the needed variables in the simulation results
                            vars_idx= out_var_list %in% var_list

                            # Checking variables
                            # Common variables
                            inter_vars <- out_var_list[vars_idx]

                            # Indicating that variables are not simulated, adding them before simulating
                            if (length(inter_vars)<length(var_list)) {
                              mess <- warning(paste("Variable(s)",paste(setdiff(var_list,inter_vars), collapse=", "),
                                                    "not simulated by the Stics model for USM",situation,
                                                    "=> try to add it(them) in",file.path(data_dir,situation,"var.mod")))
                              flag_sim <- FALSE
                              select_sim <- TRUE
                            }

                            if (any(vars_idx)) {
                              sim_tmp=sim_tmp[ , vars_idx]
                            } else {
                              return(list(NA,FALSE,FALSE))
                            }

                            ## Keeping only the needed dates in the simulation results
                            date_list=sit_var_dates_mask[[situation]]$Date
                            dates_idx <- sim_tmp$Date %in% date_list

                            # Checking dates
                            # Common dates
                            inter_dates <- sim_tmp$Date[dates_idx]

                            if ( length(inter_dates) < length(date_list) ) {
                              missing_dates <- date_list[!date_list %in% inter_dates]
                              mess <- warning(paste("Requested date(s)",paste(missing_dates, collapse=", "),
                                                    "is(are) not simulated for USM",situation))
                              flag_sim <- FALSE
                              select_sim <- TRUE
                            }

                            # Filtering needed dates lines
                            if ( any(dates_idx) ) {
                              sim_tmp <- sim_tmp[dates_idx, ]
                            } else {
                              return(list(NA,FALSE,FALSE, mess))
                            }



                            return(list(sim_tmp, flag_sim, select_sim, mess))
                          }

  # TODO: optimize res generation without copying out elements !
  # Formatting output list
  names(out) <- situation_names
  # for calculating allsim status
  sim_idx <- unlist(lapply(out, function(x) return(x[[2]])))
  res$flag_allsim <- all(sim_idx) & flag_allsim
  # for selecting output data.frame from the list
  sel_idx <- unlist(lapply(out, function(x) return(x[[3]])))
  #browser()
  res$sim_list <- lapply(out[sel_idx], function(x) return(x[[1]]))


  # Stopping the cluster
  parallel :: stopCluster(cl)

  # Calculating an printing duration
  if (time_display) {
    duration <- Sys.time() - start_time
    print(duration)
  }

  # displaying warnings
  # If not an empty string
  lapply(out,function(x) stics_display_warnings(x[[4]]))

  return(invisible(res))

}




#' @title Getting a stics_wrapper options list with initialized fields
#'
#' @description This function returns a default options list
#'
#' @param stics_path Path of the Stics binary executable file (delivered with
#' JavaStics interface)
#'
#' @param data_dir Path(s) of the situation(s) input files directorie(s)
#' or the root path of the situation(s) input files directorie(s)
#'
#' @param ... Add further arguments to the options
#'
#' @return A list containing Stics model stics_wrapper options
#'
#' @export
#'
stics_wrapper_options <- function(stics_path,
                                  data_dir, ... ) {

  # TODO: add an input options list to be modified,
  # or completed by the arguments content

  # Template list
  options <- list()
  options$stics_path <- character(0)
  options$data_dir <- character(0)
  options$parallel <- FALSE
  options$cores <- NA
  options$time_display <- FALSE
  options$warning_display <- TRUE


  # For getting the template
  # running stics_wrapper_options
  if (! nargs()) return(options)


  # For fixing mandatory fields values
  options$stics_path <- stics_path
  options$data_dir <- data_dir

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

  return(options)
}


stics_display_warnings <- function(in_string) {
  # print(in_string)
  # print(length(in_string))
  if (nchar(in_string) ) warning(in_string, call. = FALSE)
}
