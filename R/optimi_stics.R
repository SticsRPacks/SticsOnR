#' STICS parameter optimization
#'
#' @description Optimize STICS parameter values according to measurements
#'
#' @param dir.orig   Vector or named list of paths to the directory from which to copy the USMs files.
#' @param dir.targ   Path to the target directory for evaluation. Created if missing.
#' @param stics      STICS executable path
#' @param obs_name   A `data.frame` of observation file name(s) of the form
#'                   `data.frame(Principal= c(obs1.obs), Dominated= c(obs2.obs)` for mixed crops (simply remove the `Dominated` column for sole crops.
#' @param Parameters A data.frame with parameter name, starting, min and max values (see details and example)
#' @param Vars       Output variables on which the optimization is performed
#' @param weight     The weight used for each variable (see details)
#' @param method     The optimization method to use, see \pkg{dfoptim} package. For the moment, only [dfoptim::nmkb()]
#' @param Plant      A vector for the plant (*i.e.* Principal or associated) for which the parameters
#'                   will be set (only for plant or technical parameters in mixed crop simulations)
#'                   Set to `NULL` if using STICS in sole crop
#' @param ...        Further parameters passed to the optimization function called
#'                   (see \pkg{dfoptim} package)
#'
#' @details The function uses [stats::optimize()] for univariate optimization, and the \pkg{dfoptim} package functions for multivariate.
#' Currently only the Nelder-Mead algorithm is implemented from \pkg{dfoptim}.
#' The `Parameters` argument should be formated as a a data.frame (see example).
#' The start values should exclude the min and max values (they are exclusive bounds).
#' If the start is `NULL`, then the mean value between the min and max values is taken.
#' If weight is not provided by the user, the selection criteria is computed using the equation
#' 5 from Wallach et al. (2011). If they are provided, the equation 6 is used instead.
#'
#' @references Wallach, D., Buis, S., Lecharpentier, P., Bourges, J., Clastre, P., Launay, M., … Justes, E. (2011).
#'  A package of parameter estimation methods and implementation for the STICS crop-soil model. Environmental Modelling & Software, 26(4), 386–394. doi:10.1016/j.envsoft.2010.09.004
#'
#'
#' @return A list of three :
#' * gg_objects: A list of ggplot objects to plot the final STICS simulation with optimized parameter values
#'   compared to original parameter values.
#' * opti_output: differs if the optimisation is uni or multi-dimensional:
#'     + Univariate: A list of two: `minimum` -> the optimized parameter(s) value(s), and
#'       `objective` -> the last value of the objective function.
#'     + Multivariate: A list of six: `par` -> the optimized parameter(s) value(s),
#'       `value` -> the last value of the optimization function, `feval`-> the number of times the objective function
#'        was evaluated, `restarts` -> the number of times the algorithm had to be restarted when it stagnated,
#'        `convergence` -> 0= convergence, 1 no convergence, and `message` some information about the convergence.
#' * last_sim_data: the STICS output from the last simulation with the optimized parameter
#'   values. This data is given using [eval_output()].
#'
#' @importFrom dfoptim nmkb
#' @importFrom dplyr group_by summarise summarise_all select
#' @importFrom magrittr "%<>%"
#'
#' @seealso [dfoptim::nmkb()]
#'
#' @examples
#'\dontrun{
#' library(sticRs)
#'
#' Parameters= data.frame(parameter= c('hautK1','hautK2'),
#'                        start= c(0.2,0.2),
#'                        min= c(0,0),
#'                        max= c(1,1))
#'
#' # On a single USM:
#' optimi_stics(dir.orig = "0-DATA/dummy/Year_2005_2006/IC_Wheat_Pea",
#'              dir.targ = "2-Simulations/param_optim",
#'              stics = "0-DATA/stics_executable/19-new/Stics.exe",
#'              obs_name = c("6_IC_Wheat_N0.obs","6_IC_Pea_N0.obs"),
#'              Parameters= Parameters,
#'              Vars = c('lai(n)','masec(n)','hauteur'),
#'              method= "nmkb",Plant=1)
#'
#' # On a series of USMs:
#' optimi_stics(dir.orig = list(Y2005= "0-DATA/dummy/Year_2005_2006/IC_Wheat_Pea",
#'                              Y2006= "0-DATA/dummy/Year_2006_2007/IC_Wheat_Pea"),
#'              dir.targ = "2-Simulations/param_optim",
#'              stics = "0-DATA/stics_executable/19-new/Stics.exe",
#'              obs_name = data.frame(Principal= rep("6_IC_Wheat_N0.obs",2),
#'                                    Associated= rep("6_IC_Pea_N0.obs",2)),
#'              Parameters = Parameters, weight= 1, Vars = c('hauteur'),
#'              method= "nmkb",Plant=c(1,1))
#'}
#'
#' @export
#'
optimi_stics= function(dir.orig, dir.targ=getwd(),stics,obs_name,Parameters,
                       Vars,weight=NULL,method=c("nmkb"),Plant=1,...){
  .= NULL # to avoid CRAN checks errors

  method= match.arg(method,c("nmkb")) # add new methods here

  if(!is.list(dir.orig)){
    dir.orig= as.list(dir.orig)
  }

  if(length(Plant)!=length(dir.orig)){
    Plant= rep(Plant, length(dir.orig))
    warning("Single value of the Plant argument used for all USMs")
  }

  if(!is.data.frame(obs_name)){
    obs_name= data.frame(as.list(obs_name))
  }

  if(nrow(obs_name)>0&&length(dir.orig)!=nrow(obs_name)){
    stop("Each USM from dir.orig should have its own obs_name")
  }

  Vars_R= gsub("\\(","_",Vars)%>%gsub("\\)","",.)

  if(!is.null(weight)){
    if(length(weight)!=length(Vars_R)){
      stop("weight length should be the same as the Vars length")
    }
    weight= data.frame(variable= Vars_R, weight= weight, stringsAsFactors = FALSE)
  }

  if(!is.data.frame(Parameters)){
    stop("Parameters should be a data.frame")
  }

  # if start values not given:
  if(!all(c("parameter","min","max")%in%colnames(Parameters))){
    stop("Parameters should be a data.frame with columns name: parameter, (start), min, max")
  }else if(!c("start")%in%colnames(Parameters)){
    warning("Parameters$start not set, taking mean value between min and max")
    Parameters$start= (Parameters$max+Parameters$min)/2
  }
  # Or if any start value not given:
  na_start= is.na(Parameters$start)
  if(any(na_start)){
    Parameters$start[na_start]=
      (Parameters$max[na_start]+
         Parameters$min[na_start])/2
  }

  # Testing start values regarding min and max:
  if(any(Parameters$start<=Parameters$min)){
    stop("Wrong parameters argument: start value for ",
         paste(Parameters$parameter[Parameters$start<Parameters$min], collapse=", "),
         " below or at its min value")
  }

  if(any(Parameters$start>=Parameters$max)){
    stop("Wrong parameters argument: start value for ",
         paste(Parameters$parameter[Parameters$start>Parameters$max], collapse=", "),
         " above or at its max value")
  }
  # NB: we don't use stics_eval dircectly because we want to copy the usm only once for
  # performance.

  usm_name= paste0("optim_",1:length(dir.orig))

  if(is.null(names(dir.orig))){
    names(dir.orig)= usm_name
  }

  USM_path= file.path(dir.targ,usm_name)

  # Reference values:
  NbCores= parallel::detectCores()-1
  cl= parallel::makeCluster(min(NbCores,length(dir.orig)))
  parallel::clusterExport(cl=cl,
                          varlist=c("dir.orig","dir.targ","usm_name","stics",
                                    "obs_name","Vars","stics_eval","Plant"),
                          envir=environment())

  outputs=
    parallel::parLapply(
      cl,
      1:length(dir.orig),
      function(x,dir.orig,dir.targ,Vars,stics,obs_name,Plant){
        sim_name= list(stics)
        names(sim_name)= usm_name[x]
        stics_eval(dir.orig = dir.orig[[x]], dir.targ = dir.targ,
                   stics = sim_name, obs_name = obs_name[x,],
                   Out_var = Vars, plot_it = FALSE, Erase = FALSE,
                   Parallel = FALSE)
      },dir.orig,dir.targ,Vars,stics,obs_name,Plant)

  names(outputs)= usm_name
  parallel::stopCluster(cl)

  if(length(Parameters$parameter)==1){
    # univariate optimization:
    opti= stats::optimize(f= stics_eval_opti,
                          interval= c(Parameters$min,Parameters$max),
                          USM_path= USM_path,
                          param= as.character(Parameters$parameter),
                          Plant= Plant,
                          weight= weight,
                          obs_name= obs_name)
    params= as.list(opti$minimum)
    names(params)= Parameters$parameter
  }else{
    # multivariate optimization:
    opti= dfoptim::nmkb(fn= stics_eval_opti,
                        par= Parameters$start,
                        lower= Parameters$min,
                        upper= Parameters$max,
                        USM_path= USM_path,
                        param= as.character(Parameters$parameter),
                        Plant= Plant,
                        weight= weight,
                        obs_name= obs_name)
    params= as.list(opti$par)
    names(params)= Parameters$parameter
  }


  # Re-make the best simulation for output:
  NbCores= parallel::detectCores()-1
  cl= parallel::makeCluster(min(NbCores,length(dir.orig)))
  parallel::clusterExport(cl=cl,
                          varlist=c("dir.orig","dir.targ","usm_name","stics",
                                    "obs_name","Vars","stics_eval","Plant","params"),
                          envir=environment())
  parallel::clusterEvalQ(cl, {library(dplyr)})

  outputs_new=
    parallel::parLapply(
      cl,
      1:length(dir.orig),
      function(x,dir.orig,dir.targ,Vars,stics,obs_name,Plant){
        sim_name= list(stics)
        names(sim_name)= usm_name[x]

        tmp= stics_eval(dir.orig = dir.orig[[x]], dir.targ = dir.targ,
                        stics = sim_name, obs_name = obs_name[x,],
                        Out_var = Vars, plot_it = FALSE, Erase = FALSE,
                        Parallel = FALSE,Parameter= params,Plant= Plant[x])
        tmp$outputs[[usm_name[x]]]=
          tmp$outputs[[usm_name[x]]]%>%
          dplyr::mutate(usm= names(dir.orig[x]))
        tmp
      },dir.orig,dir.targ,Vars,stics,obs_name,Plant)

  parallel::stopCluster(cl)

  output_opti=
    lapply(outputs_new, function(x){x$outputs[[1]]})%>%
    data.table::rbindlist(fill = TRUE)%>%as.data.frame()

  opti_plot=
    lapply(1:length(dir.orig),
           function(x){
             plot_output(original= outputs[[x]]$outputs[[1]],
                         optimized= outputs_new[[x]]$outputs[[1]]%>%select(-.data$usm),
                         plot_it = FALSE,
                         Title = paste("Simulations for USM",names(dir.orig)[x]))
           })
  names(opti_plot)= names(dir.orig)

  unlink(x = dir.targ, recursive = T, force = T)

  invisible(list(gg_objects= opti_plot, opti_output= opti, last_sim_data= output_opti))
}




#' Objective function to optimize
#'
#' @description Function evaluated for parameter optimization. This function is only provided
#' for informative value, but should not be used by the user. It is called by [optimi_stics()].
#'
#' @param x        The starting parameter values
#' @param USM_path The path to the USM
#' @param obs_name The observation file name
#' @param param    The parameter names (in same order than x)
#' @param weight   The weight used for each variable (see details)
#' @param Plant      The plant (*i.e.* Principal or associated) for which the parameters
#'                   will be set (only for plant or technical parameters in mixed crop simulations)
#'                   Set to `NULL` if using STICS in sole crop
#'
#' @details If weight is not provided by the user, the selection criteria is computed using the equation
#' 5 from Wallach et al. (2011). If they are provided, the equation 6 is used instead.
#'
#' @references Wallach, D., Buis, S., Lecharpentier, P., Bourges, J., Clastre, P., Launay, M., … Justes, E. (2011).
#'  A package of parameter e00stimation methods and implementation for the STICS crop-soil model. Environmental Modelling & Software, 26(4), 386–394. doi:10.1016/j.envsoft.2010.09.004
#'
#' @importFrom rlang .data
#'
#' @return The weighted product of squares (selection criteria)
#' @export
#'
stics_eval_opti= function(x,USM_path,obs_name,param,weight=NULL,Plant){
  names(x)= param

  output= stics_eval_no_copy(USM_path,x,obs_name,Plant)

  out_stats=
    output%>%
    dplyr::select(-.data$Plant)%>% # removing the Plant column to avoid any issue in the next line
    # Selecting only the plant the user need:
    dplyr::filter(ifelse(.data$Dominance=="Sole crop"|(.data$Dominance=="Principal"&Plant==1)|
                           (.data$Dominance=="Associated"&Plant==2),TRUE,FALSE))%>%
    dplyr::select(-.data$usm)%>%
    stati_stics()%>%dplyr::ungroup()

  if(is.null(weight)){
    crit=
      out_stats%>%
      dplyr::mutate(crit= (.data$SS_res/.data$n_obs)^(.data$n_obs/2))%>%
      dplyr::summarise(crit= prod(.data$crit))
  }else{
    crit=
      dplyr::left_join(weight,out_stats%>%
                         dplyr::mutate(variable= as.character(.data$variable)),
                       by= "variable")%>%
      dplyr::mutate(crit= .data$SS_res*.data$weight)%>%
      dplyr::summarise(crit= sum(.data$crit))
  }
  crit$crit
}


#' Make a simulation
#'
#' @description Make a STICS simulation such as for using [stics_eval()] but without
#' importing the files, and without making plots. This function is parallelized over USMs.
#'
#' @param USM_path The path to the USM folder
#' @param param    The parameter values
#' @param obs_name The observation file names
#' @param Plant    The plant (*i.e.* Principal or associated) for which the parameters
#'                 will be set (only for plant or technical parameters in mixed crop simulations)
#'                 Set to `NULL` if using STICS in sole crop#'
#' @return The output of [eval_output()]
#'
stics_eval_no_copy= function(USM_path,param,obs_name,Plant){

  if(length(USM_path)==1){
    outputs=
      lapply(1:length(USM_path),
             function(x){
               lapply(names(param), function(pa){
                 set_param(dirpath = USM_path[x],
                           param = pa,
                           value = param[pa],
                           plant = Plant)
               })
               run_stics(dirpath = USM_path[x])
               eval_output(USM_path[x], obs_name= obs_name[x,])%>%
                 dplyr::mutate(usm= USM_path[x])
             })%>%
      data.table::rbindlist(fill = TRUE)%>%
      as.data.frame()
  }else{
    # Make the simulation in parallel if there are several USMs
    NbCores= parallel::detectCores()-1
    cl= parallel::makeCluster(min(NbCores,length(USM_path)))
    parallel::clusterExport(cl=cl,
                            varlist=c("USM_path","param","obs_name","set_param",
                                      "run_stics","eval_output","Plant"),
                            envir=environment())

    parallel::clusterEvalQ(cl, library("dplyr"))

    outputs=
      parallel::parLapply(
        cl,
        1:length(USM_path),
        function(x,USM_path,param,obs_name,Plant){
          lapply(names(param), function(pa){
            set_param(dirpath = USM_path[x],
                      param = pa,
                      value = param[pa],
                      plant = Plant[x])
          })
          run_stics(dirpath = USM_path[x])
          eval_output(USM_path[x], obs_name= obs_name[x,])%>%
            dplyr::mutate(usm= USM_path[x])
        },USM_path,param,obs_name,Plant)%>%
      data.table::rbindlist(fill = TRUE)%>%
      as.data.frame()
    parallel::stopCluster(cl)
  }

  outputs
}
