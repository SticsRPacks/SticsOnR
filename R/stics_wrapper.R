
#' @title Running usm(s) from txt input files stored in one rep. per USM and return simulated results in a list
#'
#' @description This function uses Stics directly through a system call, can
#' force Stics input parameters with values given in arguments.
#'
#' @param model_options List containing any information needed by the model.
#' In the case of Stics: \code{stics_path} the path of Stics executable file and
#' \code{data_dir} the path of the directory containing the Stics input data
#' for each USM (one folder per USM where Stics input files are stored in txt
#' format)
#'
#' @param site_var_dates List of situations, variables and dates for which
#' simulated values should be returned. Typically a list containing the
#' observations to which simulations should be compared as provided by
#' SticsRFiles::read_obs_to_list
#'
#' @param param_values named vector containing the value(s) and names of the
#' parameters to force (optional)
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


stics_wrapper <- function(model_options, param_values=NULL, site_var_dates=NULL) {

  # Preliminary checks ---------------------------------------------------------

  # TODO : make a function dedicated to checking model_options
  # Because it may be model dependant, so it could be possible to pass anything
  # usefull in the model running function...
  # Reuse next lines before `Run Stics` block
  ## check presence of mandatory information in model model_options list
  if (is.null(model_options$stics_path) || is.null(model_options$data_dir)) {
    stop("stics_path and data_dir should be elements of the model_model_options list
         for the Stics model")
  }
  stics_path=model_options$stics_path
  data_dir=model_options$data_dir

  ## testing if the model executable file exist and if it is executable
  if (!file.exists(stics_path)){
    stop(paste("Stics executable file doesn't exist !",stics_path))
  }
  val <- try(system(paste(stics_path,'--version'),intern = FALSE,ignore.stdout = TRUE), silent = TRUE)
  if (val != 0) {
    stop(paste(stics_path,"is not executable or is not a Stics executable !"))
  }


  # Run Stics and store results ------------------------------------------------

  # Checking if all data for all situations will be kept or not
  keep_all_data <- FALSE
  if (is.null(site_var_dates)) keep_all_data <- TRUE

  # Getting situations names list (from dir names or site_var_dates fields names)
  if (keep_all_data) {
    situation_names=list.dirs(data_dir, full.names = FALSE)[-1]
  } else {
    situation_names=names(site_var_dates)
  }

  # Calculating dirs list and results list
  run_dirs=file.path(data_dir,situation_names)
  res=list(sim_list=vector("list", length(situation_names)),flag_allsim=TRUE)
  names(res$sim_list)=situation_names

  ## If data not provided for the required USM
  dirs_exist <- file.exists(run_dirs)
  if (!all(dirs_exist)) {
    warning(paste("No folder provided for USM(s)",paste(situation_names[!dirs_exist], collapse=", "),
                  "in data_dir",data_dir))
    res$flag_allsim=FALSE
  }

  ## Loops on the USMs that can be simulated
  for (iusm in which(dirs_exist)) {

    # TODO: make a function dedicated to forcing parameters of the model ?
    # In that case by using the param.sti mechanism
    ## Force param values
    if (is.null(param_values)) {
      # remove param.sti in case of previous run using it ...
      if (suppressWarnings(file.remove(file.path(data_dir,situation_names[iusm],
                                                 "param.sti")))) {
        set_codeoptim(run_dirs[iusm],value=0)
      }

    } else {
      gen_param_sti(run_dirs[iusm], param_values, names(param_values))
      set_codeoptim(run_dirs[iusm],value=1)
    }

    ## Run the model
    usm_out=run_system(stics_path, data_dir, situation_names[iusm])

    # if the model returns an error, ... treating next situation
    if (usm_out[[1]]$error > 0) {

      warning(paste("Error running the Stics model for USM",situation_names[iusm],". \n ",usm_out[[1]]$message))
      res$sim_list[[situation_names[iusm]]]=NULL
      res$flag_allsim=FALSE
      next

    } # else {

    ## Otherwise get the results

    sim_tmp=get_daily_results(file.path(data_dir, situation_names[iusm]), situation_names[iusm])

    # Integrated in get_daily results
    # Adding the column date in the simulation results tibble
    #Date= data.frame(Date=as.POSIXct(x = paste(sim_tmp$ian,sim_tmp$mo,sim_tmp$jo, sep="-"),
    #                                 format = "%Y-%m-%d", tz="UTC"))
    # Integrated in get_daily results calling new function var_to_col_names
    #sim_tmp= cbind(Date,sim_tmp)
    ## change the .n. by _n in the varname to be homogeneous with read_obs outputs
    # tmp=sub("\\.$","",colnames(sim_tmp))
    # colnames(sim_tmp)=sub("\\.","_",tmp)


    var_list=colnames(site_var_dates[[situation_names[iusm]]])
    out_var_list = colnames(sim_tmp)

    # Keeping all outputs data
    # - If no site_var_dates given as input arg
    # - If all output variables are in
    #   site_var_dates[[situation_names[iusm]]]
    if (keep_all_data || ( all(var_list %in% out_var_list) &&
                           all(out_var_list %in% var_list)) ) {
      res$sim_list[[situation_names[iusm]]] <- sim_tmp
      next
    }


    ## Keeping only the needed variables in the simulation results
    inter_vars=intersect(out_var_list,var_list)

    if (length(inter_vars)>0) {
      sim_tmp=as.data.frame(sim_tmp[,inter_vars])
    } else {
      res$sim_list[[situation_names[iusm]]]=NULL
      res$flag_allsim=FALSE
      next
    }
    if (length(inter_vars)<length(var_list)) {
      warning(paste("Variable(s)",paste(setdiff(var_list,inter_vars), collapse=", "),
                    "not simulated by the Stics model for USM",situation_names[iusm],
                    "=> try to add it(them) in",file.path(data_dir,situation_names[iusm],"var.mod")))
      res$flag_allsim=FALSE
    }

    ## Keeping only the needed dates in the simulation results
    date_list=site_var_dates[[situation_names[iusm]]]$Date
    inter_dates=intersect(sim_tmp$Date,date_list)
    if (length(inter_dates)>0) {
      res$sim_list[[situation_names[iusm]]]=
        as.data.frame(sim_tmp[match(inter_dates,sim_tmp$Date),])
    } else {
      res$sim_list[[situation_names[iusm]]]=NULL
      res$flag_allsim=FALSE
      next
    }
    if (length(inter_dates)<length(date_list)) {
      warning(paste("Requested date(s)",paste(date_list[match(setdiff(date_list,inter_dates),date_list)], collapse=", "),
                    "is(are) not simulated for USM",situation_names[iusm]))
      res$flag_allsim=FALSE
    }

    #}

  }

  return(res)

}
