
#' @title Running usm(s) from txt input files stored in one directory
#' per `situation`, simulated results are returned in a list
#'
#' @description This function uses Stics directly through a system call, can
#' force Stics input parameters with values given in arguments.
#'
#' @param param_values named vector containing the value(s) and names of the
#' parameters to force (optional)
#'
#' @param sit_var_dates_mask List of situations, variables and dates for which
#' simulated values should be returned. Typically a list containing the
#' observations to which simulations should be compared as provided by
#' SticsRFiles::read_obs_to_list
#'
#' @param prior_information Prior information on the parameters to estimate.
#' For the moment only uniform distribution are allowed.
#' Either a list containing (named) vectors of upper and lower
#' bounds (\code{ub} and \code{lb}), or a named list containing for each
#' parameter the list of situations per group (\code{sit_list})
#' and the vector of upper and lower bounds (one value per group) (\code{ub} and \code{lb})
#'
#' @param model_options List containing any information needed by the model.
#' In the case of Stics: \code{stics_path} the path of Stics executable file and
#' \code{data_dir} the path of the directory containing the Stics input data
#' for each USM (one folder per USM where Stics input files are stored in txt
#' format)
#'
#' @return A list containing simulated values (\code{sim_list}) and a flag
#' (\code{flag_allsim}) indicating if all required situations, variables and
#' dates were simulated.
#'
#' @examples
#'
#' @export
#'
#
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


stics_wrapper <- function( param_values=NULL, sit_var_dates_mask=NULL,
                           prior_information=NULL, model_options ) {


  library("doParallel")
  library("SticsRFiles")

  # Preliminary model checks ---------------------------------------------------

  ##############################################################################
  # TODO : make a function dedicated to checking model_options
  # Because it may be model dependant, so it could be possible to pass anything
  # usefull in the model running function...
  # Reuse next lines before `Run Stics` block
  ## check presence of mandatory information in model model_options list
  if (is.null(model_options$stics_path) || is.null(model_options$data_dir)) {
    stop("stics_path and data_dir should be elements of the model_model_options
    list for the Stics model")
  }
  stics_path <- model_options$stics_path
  data_dir <- model_options$data_dir
  parallel <- model_options$parallel
  cores <- model_options$cores
  time_display <- model_options$time_display
  warning_display <- options$warning_display

  ## testing if the model executable file exist and if it is executable
  if (!file.exists(stics_path)){
    stop(paste("Stics executable file doesn't exist !",stics_path))
  }
  val <- try(system(paste(stics_path,'--version'),
                    intern = FALSE,
                    ignore.stdout = TRUE),
             silent = TRUE)

  if (val != 0) {
    stop(paste(stics_path,"is not executable or is not a Stics executable !"))
  }
  ##############################################################################


  if (time_display)   start_time <- Sys.time()

  # Managing parallel model simulations
  # Managing cores number to use
  cores_nb <- 1
  if ( parallel ) {
    if (is.na(cores)) {
      cores_nb <- detectCores() - 1
    } else {
      cores_nb <- cores
    }
  }
  # Launching the cluster
  cl <- makeCluster(cores_nb)
  registerDoParallel(cl)






  # Run Stics and store results ------------------------------------------------

  # Checking if all data for all situations will be kept or not
  keep_all_data <- FALSE
  if (is.null(sit_var_dates_mask)) keep_all_data <- TRUE

  # Getting situations names list
  # (from dir names or sit_var_dates_mask fields names)
  if (keep_all_data) {
    situation_names=list.dirs(data_dir, full.names = FALSE)[-1]
    situation_names=situation_names[sapply(situation_names,
                                           function(x) file.exists(file.path(data_dir,x,"new_travail.usm")))]
  } else {
    situation_names=names(sit_var_dates_mask)
  }

  # Calculating directories list
  run_dirs=file.path(data_dir,situation_names)
  # res=list(sim_list=vector("list", length(situation_names)),flag_allsim=TRUE)
  # names(res$sim_list)=situation_names

  ## If data not provided for the required USM
  dirs_exist <- file.exists(run_dirs)
  flag_allsim <- TRUE
  if (!all(dirs_exist)) {
    warning(paste("No folder provided for USM(s)",paste(situation_names[!dirs_exist], collapse=", "),
                  "in data_dir",data_dir))
    #res$flag_allsim=FALSE
    flag_allsim <- FALSE
  }

  dirs_idx <- which(dirs_exist)
  res <- list()
  ## Loops on the USMs that can be simulated
  #for (iusm in which(dirs_exist)) {
  out <- foreach(iusm = dirs_idx,
                 .export = c("get_daily_results",
                             "set_codeoptim",
                             "run_system",
                             "gen_param_sti",
                             "get_params_per_sit")) %dopar% {

                               # Simulation flag status or output data selection status
                               flag_sim <- TRUE
                               select_sim <- TRUE
                               #iusm <- dirs_idx[i]
                               run_dir <- run_dirs[iusm]
                               situation <- situation_names[iusm]
                               keep_all_data <- TRUE
                               mess <- ""
                               ########################################################################
                               # TODO: make a function dedicated to forcing parameters of the model ?
                               # In that case by using the param.sti mechanism
                               ## Force param values
                               if (is.null(param_values)) {
                                 # remove param.sti in case of previous run using it ...
                                 if (suppressWarnings(file.remove(file.path(run_dir,
                                                                            "param.sti")))) {
                                   set_codeoptim(run_dir,value=0)
                                 }

                               } else {
                                 param_values_usm=get_params_per_sit(prior_information,situation_names[iusm],param_values)

                                 gen_param_sti(run_dir, names(param_values_usm), param_values_usm)
                                 set_codeoptim(run_dir,value=1)
                               }
                               ########################################################################
                               # TODO: and call it in/ or integrate parameters forcing in run_system function !
                               ## Run the model
                               usm_out=run_system(stics_path, run_dir)

                               # if the model returns an error, ... treating next situation
                               if (usm_out[[1]]$error > 0) {

                                 mess <- warning(paste("Error running the Stics model for USM",situation,
                                               ". \n ",usm_out[[1]]$message))
                                 #res$sim_list[[situation]]=NA
                                 #res$flag_allsim=FALSE


                                 return(list(NA,FALSE,FALSE,mess))
                               } #else {


                               ## Otherwise, getting results

                               sim_tmp=get_daily_results(file.path(data_dir, situation),
                                                         situation)

                               # Any error reading output file
                               if (is.null(sim_tmp)) {
                                 #res$sim_list[[situation]]=NA
                                 #res$flag_allsim=FALSE
                                  mess <- warning(paste("Error reading outputs for ",situation,
                                                       ". \n "))
                                 return(list(NA, FALSE, FALSE, mess))

                               }

                               # Integrated in get_daily results
                               # Adding the column date in the simulation results tibble
                               #Date= data.frame(Date=as.POSIXct(x = paste(sim_tmp$ian,sim_tmp$mo,sim_tmp$jo, sep="-"),
                               #                                 format = "%Y-%m-%d", tz="UTC"))
                               # Integrated in get_daily results calling new function var_to_col_names
                               #sim_tmp= cbind(Date,sim_tmp)
                               ## change the .n. by _n in the varname to be homogeneous with read_obs outputs
                               # tmp=sub("\\.$","",colnames(sim_tmp))
                               # colnames(sim_tmp)=sub("\\.","_",tmp)


                               if ( !is.null(sit_var_dates_mask) &&
                                    situation %in% situation_names) {
                                 keep_all_data <- FALSE
                                 var_list=colnames(sit_var_dates_mask[[situation]])
                                 out_var_list = colnames(sim_tmp)
                               }

                               # Keeping all outputs data
                               # - If no sit_var_dates_mask given as input arg
                               # - If all output variables are in
                               #   sit_var_dates_mask[[situation]]
                               if ( keep_all_data ) {
                                 #res$sim_list[[situation]] <- sim_tmp

                                 return(list( sim_tmp,TRUE, TRUE, mess))
                               } #else {

                               ## Keeping only the needed variables in the simulation results
                               #inter_vars=intersect(out_var_list,var_list)
                               vars_idx= out_var_list %in% var_list
                               inter_vars <- out_var_list[vars_idx]

                               #if (length(inter_vars)>0) {
                               if (any(vars_idx)) {
                                 #sim_tmp=as.data.frame(sim_tmp[,inter_vars])
                                 sim_tmp=sim_tmp[ , vars_idx]
                               } else {
                                 #res$sim_list[[situation]]=NA
                                 #res$flag_allsim=FALSE

                                 return(list(NA,FALSE,FALSE))
                               }
                               if (length(inter_vars)<length(var_list)) {
                               #if ( sum(vars_idx) < length(var_list)) {
                                 mess <- warning(paste("Variable(s)",paste(setdiff(var_list,inter_vars), collapse=", "),
                                               "not simulated by the Stics model for USM",situation,
                                               "=> try to add it(them) in",file.path(data_dir,situation,"var.mod")))
                                 #res$flag_allsim=FALSE
                                 flag_sim <- FALSE
                                 select_sim <- TRUE
                               }

                               ## Keeping only the needed dates in the simulation results
                               date_list=sit_var_dates_mask[[situation]]$Date
                               dates_idx <- sim_tmp$Date %in% date_list
                               inter_dates <- sim_tmp$Date[dates_idx]
                               #inter_dates=intersect(sim_tmp$Date,date_list)
                               #if (length(inter_dates)>0) {
                               if ( any(dates_idx) ) {
                                 #res$sim_list[[situation]]=
                                 # as.data.frame(sim_tmp[match(inter_dates,sim_tmp$Date),])
                                 #sim_tmp <- as.data.frame(sim_tmp[match(inter_dates,sim_tmp$Date),])
                                 sim_tmp <- sim_tmp[dates_idx, ]
                               } else {
                                 #res$sim_list[[situation]]=NA
                                 #res$flag_allsim=FALSE

                                 return(list(NA,FALSE,FALSE, mess))
                               }
                               if (length(inter_dates)<length(date_list)) {
                               #if ( sum(dates_idx) < length(date_list) ) {
                                 mess <- warning(paste("Requested date(s)",paste(date_list[match(setdiff(date_list,inter_dates),date_list)], collapse=", "),
                                               "is(are) not simulated for USM",situation))
                                 #res$flag_allsim=FALSE
                                 flag_sim <- FALSE
                                 select_sim <- TRUE
                               }

                               #}
                               #}

                               #return(list(sim_tmp,res$flag_allsim))
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
  stopCluster(cl)

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
#' @param parallel Logical value for specifying to perform parallel runs (TRUE)
#' or not (FALSE)
#'
#' @param cores Number of cores to be used
#'
#' @param time_display Logical value used to display (TRUE) or not (FALSE)
#' simulations duration
#'
#' @return A list containing Stics model stics_wrapper options
#'
#' @examples
#'
#' @export
#'
stics_wrapper_options <- function(stics_path,
                                  data_dir, ... ) {

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
