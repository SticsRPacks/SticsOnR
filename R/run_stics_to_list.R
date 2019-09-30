run_stics_to_list <- function(model_path,data_dir,usm_names,param_names=NULL,param_values=NULL,varnames=NULL,dates=NULL,var_and_dates=NULL) {
  #' @title Running usm(s) from txt input files stored in one rep. per USM and return simulated results in a list
  #'
  #' @description This function uses Stics directly through a system call, can force Stics input parameters with values given in arguments.
  #'
  #' @param model_path Path of Stics executable file
  #' @param data_dir Path of a directory containing one folder per USM where Stics input files are stored in txt format
  #' @param usm_names Name(s) of the USMs to simulate (and of the corresponding sub-directory(ies) of data_dir)
  #' @param param_names Name(s) of parameters to force the value (optional)
  #' @param param_values Value(s) of the parameters to force (optional)
  #' @param varnames List of variables for which simulated values should be returned (optional)
  #' @param dates List of dates (%Y-%m-%d) for which simulated values should be returned (optional)
  #' @param var_and_dates List of variables and dates for which simulated values should be returned. Typically a list containing the observations to which simulations should be compared as provided by SticsRFiles::read_obs_to_list (optional)
  #'
  #' @details If \code{var_and_dates} is not provided, \code{varnames} must be. If neither \code{var_and_dates} and \code{dates} is provided simulated values will be returned for all dates.
  #'
  #' @return A list containing simulated values
  #'
  #' @examples
  #'
  #' @export
  #'


  #
  # TO DO LIST
  #    - maybe the variables asked will not be simulated (depends on the var.mod file ...)
  #         => try to simulate, if some variables are not simulated : modify the var.mod and re-simulate (should be more efficient
  #             than checking everytime the var.mod)
  #         For the moment, exit with an error and ask to change var.mod
  #    - handle the case of stages (stages should be specified in the var.mod ... + handle the case when simulations does not reach the asked stages ...)
  #

  run_dir=file.path(data_dir,usm_names)

  # Preliminary checks

  ## testing id dirs exist
  dirs_exist <- file.exists(run_dir)

  if ( !all(dirs_exist) ) {
    print(paste0(run_dir[!dirs_exist],collapse = ", "))
    stop("One or more USM directories does/do not exist !")
  }

  if (!file.exists(model_path)){
    stop(paste("Stics executable file doesn't exist !",model_path))
  }

  ## testing if the model is executable
  val <- try(system(paste(model_path,'--version'),intern = FALSE,ignore.stdout = TRUE), silent = TRUE)
  if (val != 0) {
    stop("The file is not executable or is not a Stics executable !")
  }


  # run Stics and store results

  sim_list=vector("list", length(usm_names))
  names(sim_list)=usm_names

  for (iusm in 1:length(usm_names)) {

    # if the usm has to be simulated
    if(is.null(var_and_dates) || (!is.null(var_and_dates) && !is.null(var_and_dates[[usm_names[iusm]]])) ) {

      if (is.null(param_names) || is.null(param_values)) {
        # remove param.sti in case of previous run using it ...
        if (suppressWarnings(file.remove(file.path(data_dir,usm_names[iusm],"param.sti")))) {
            set_codeoptim(run_dir[iusm],value=0)
        }

      } else {
        gen_param_sti(run_dir[iusm], param_names, param_values)
        set_codeoptim(run_dir[iusm],value=1)
      }

      usm_out=run_system(model_path, data_dir, usm_names[iusm])

      if (usm_out[[1]]$error > 0) {

        warning(paste("Error running the Stics model for USM",usm_names[iusm],". \n ",usm_out[[1]]$message))
        sim_list[[usm_names[iusm]]]=NULL

      } else {

        sim_tmp=get_daily_results(file.path(data_dir, usm_names[iusm]), usm_names[iusm])

        # add the column date in the simulation results tibble
        Date= data.frame(Date=as.POSIXct(x = paste(sim_tmp$ian,sim_tmp$mo,sim_tmp$jo, sep="-"),
                                         format = "%Y-%m-%d", tz="UTC"))
        sim_tmp= cbind(Date,sim_tmp)

        # change the .n. by _n in the varname to be homogeneous with read_obs outputs
        tmp=sub("\\.$","",colnames(sim_tmp))
        colnames(sim_tmp)=sub("\\.","_",tmp)

        if(!is.null(dates)) {
          date_list=dates
        } else {
          date_list=sim_tmp$Date
        }

        if(!is.null(varnames)) {
          var_list=c("Date",varnames)
        } else if(!is.null(var_and_dates)) {
          var_list=colnames(var_and_dates[[usm_names[iusm]]])
          date_list=var_and_dates[[usm_names[iusm]]]$Date
        } else {
          stop("Either varnames or var_and_dates have to be provided in argument.")
        }

        # keep only the needed variables in the simulation results
        if (!all(var_list %in% colnames(sim_tmp))) {
          stop(paste("Variable(s)",setdiff(var_list,colnames(sim_tmp)),
                     "not simulated by the Stics model for USM",usm_names[iusm],
                     "=> try to add it(them) in",file.path(data_dir,usm_names[iusm],"var.mod")))
        }
        sim_tmp=sim_tmp[,var_list]

        # keep only the needed dates in the simulation results
        inter_dates=intersect(sim_tmp$Date,date_list)
        if (length(inter_dates)>0) {
          sim_list[[usm_names[iusm]]]=
            as.data.frame(sim_tmp[match(inter_dates,sim_tmp$Date),])
        } else {
          sim_list[[usm_names[iusm]]]=NULL
        }

        if (length(inter_dates)<length(date_list)) {
          warning(paste("Requested date(s)",paste(date_list[match(setdiff(date_list,inter_dates),date_list)], collapse=", "),
                        "is(are) not simulated for USM",usm_names[iusm]))
        }

      }

    }

  }

  return(sim_list)

}
