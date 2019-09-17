#' Evaluate STICS version
#'
#' @description Wrapper function to run STICS simulations and evaluate
#'  outputs using observations and/or other model versions.
#'
#' @param dir.orig   Path to the directory from which to copy the simulation files. If
#'                   `NULL` (the default), uses the package dummy USM.
#' @param dir.targ   Path to the target directory for evaluation. Created if missing.
#' @param stics      STICS executable path named list (see details).
#' @param Parameter  STICS input parameter named list (see details).
#' @param Plant      Integer value of the plant on which to set the parameter if STICS
#'                   is run on mixed crop (1= Principal, 2= Associated)
#' @param obs_name   A vector of observation file name(s). It must have the form
#'                   `c(Dominant,Dominated)` for mixed crops.
#'                   See [read_obs()] `filename` parameter for more details.
#' @param Out_var    The variables needed as output
#' @param plot_it    Boolean. Do the plot has to be pinted ?
#' @param Parallel   Are the simulations to be executed in parallel ?
#' @param mixed      (optional) Is the simulation made on mixed species (boolean)
#' @param Title      A title for the evaluation. This will be used on the ggplot object.
#' @param Erase      Should the simulations data be erased upon import (see details)?
#'
#' @details The function evaluate STICS outputs either along different model versions \strong{OR}
#' parameter values, not both at the same time. The method is automatically chosen using the
#' `stics` and `Parameter` length. The parameter with a length > 1 will be evaluated.
#' The names of the `stics` or the `Parameter` list are used for reference in the outputs
#' (data.frames and plots). The names are mandatory for `Parameter`, but are optionnal for
#' `stics`. If no names are provided for `stics`, the function give a dummy name for each
#' model evaluation.
#' The format of both `stics` or `Parameter` parameters is the same: a named list of either
#' STICS executable path or a named list of parameters value(s).
#' Please set the `Parameter` argument to `NULL` (the default) for no parameter changes.
#' The function run STICS executable in parrallel using all cores from the machine but one.
#'
#' @return A list of three objects:
#' \describe{
#'   \item{outputs}{A list of `data.frame` objects corresponding
#'   to the simulation for each value in the `stics` or `Parameter` arguments. The
#'   data.frame is made by a call to [eval_output()].}
#'   \item{gg_object}{A summary plot of the simulations outputs returned as a ggplot object.
#'    Possibly a comparison between simulations if several values are given to the `stics`
#'    or `Parameter` arguments.}
#'    \item{stats}{A `data.frame` of a set of summary statistics for each simulation, computed
#'    by a call to [stati_stics()].}
#' }
#'
#'
#' @seealso [sensitive_stics()] to evaluate STICS sensitivity to parameter(s), and other
#' functions used under the hood: [eval_output()], [import_usm()],
#' [run_stics()], [stati_stics()], and [set_out_var()].
#'
#' @examples
#' \dontrun{
#' library(sticRs)
#' # Evaluating a change in a parameter:
#'
#' Eval_parameter=
#'  stics_eval(dir.orig = "0-DATA/dummy/IC_Wheat_Pea",
#'             dir.targ = "2-Simulations/Parameterization",
#'             stics = list(EquivDens= "0-DATA/stics_executable/EquDens/stics.exe"),
#'             Parameter =  list(interrang= c(0.05, 0.25)),
#'             obs_name =  c("6_IC_Wheat_N02.obs","6_IC_Pea_N02.obs"),
#'             Out_var = c("raint", "trg(n)", "rsoleil", "lai(n)", "masec(n)",
#'             "hauteur", "cumraint", "fapar", "eai"),
#'             Title = "Wheat-Pea Auzeville 2005-2006 N0", plot_it = T)
#'
#'
#' # Evaluating a change in the executable:
#' Eval_stics=
#' stics_eval(dir.orig = "0-DATA/dummy/IC_Wheat_Pea",
#'            dir.targ = "2-Simulations/Parameterization",
#'            stics = list(EquivDens= "0-DATA/stics_executable/EquDens/stics.exe"),
#'            obs_name =  c("6_IC_Wheat_N02.obs","6_IC_Pea_N02.obs"),
#'            Out_var = c("raint", "trg(n)", "rsoleil", "lai(n)", "masec(n)",
#'            "hauteur", "cumraint", "fapar", "eai"),
#'            Title = "Wheat-Pea Auzeville 2005-2006 N0", plot_it = T)
#' }
#' @export
stics_eval= function(dir.orig=NULL, dir.targ= getwd(),stics,Parameter=NULL,
                     Plant=1,obs_name= NULL,
                     Out_var=c("lai(n)","masec(n)","hauteur"), plot_it=T,
                     Parallel=T,mixed= NULL,Title=NULL,Erase=TRUE){

  if(!dir.exists(dir.targ)&Erase){erase_dir=T}else{erase_dir=F}
  Param_val= Parameter
  if(length(stics)>1&any(lapply(Param_val, length)%>%unlist > 1)){
    stop("stics_eval only evaluate several STICS executables OR parameter values",
         " not both at a time.", "\nPlease provide only one with several values")
  }

  # Setting usm name to either parameter values or stics names
  if(any(lapply(Param_val, length)%>%unlist > 1)){
    if(is.list(Parameter)){
      usm_name= paste("Set",seq_along(Param_val[[1]]), sep="_")
      method= "Parameter"
    }else{
      stop("The Parameter parameter must be a list")
    }
  }else{
    if(is.list(stics)){
      usm_name= names(stics)
      if(is.null(usm_name)){
        usm_name= paste0("STICS_",seq_along(stics))
      }
      if(length(stics)>1){
        method= "stics"
      }else{
        method= "Parameter" # Default method
      }

    }else{
      stop("The stics parameter must be a list")
    }
  }


  if(Parallel&length(usm_name)>1){
    NbCores= parallel::detectCores()-1
    cl= parallel::makeCluster(min(NbCores,length(usm_name)))
    parallel::clusterExport(cl=cl,
                            varlist=c("dir.orig","dir.targ","usm_name","stics",
                                      "obs_name","Out_var","import_usm",
                                      "set_out_var","Plant","run_stics",
                                      "eval_output","Parameter","Erase","method",
                                      "set_param","Param_val"),
                            envir=environment())
    # The apply function runs either along parameter values or stics exe.
    outputs=
      parallel::parLapply(
        cl,
        seq_along(usm_name),
        function(x,dir.orig,dir.targ,usm_name,stics,
                 obs_name,Parameter,Plant,Erase,method,
                 Param_val){
          tryCatch(expr = {
            USM_path= file.path(dir.targ,usm_name[x])
            import_usm(dir.orig = dir.orig, dir.targ = dir.targ,
                       usm_name = usm_name[x], overwrite = T,
                       stics = ifelse(method=="stics",stics[[x]],
                                      stics[[1]]))
            set_out_var(filepath= file.path(USM_path,"var.mod"),
                        vars=Out_var, add=F)
            if(method=="Parameter"){
              # set parameter values using mapply:
              Param_val_x= lapply(Param_val, function(y){y[x]})
              mapply(function(y,z){
                set_param(dirpath = USM_path,
                          param = y,plant = Plant,
                          value = z)
              },names(Param_val_x),Param_val_x)
            }
            run_stics(dirpath = USM_path)
            output= eval_output(dirpath= USM_path, obs_name= obs_name)
            if(Erase){
              unlink(x = USM_path, recursive = T, force = T)
            }
            output
          },
          error=function(cond) {
            message(paste("Error during simulation setup for USM:",usm_name[x]))
            message("Here's the original error message:")
            message(cond)
            unlink(x = USM_path, recursive = T, force = T)
            return(NULL)
          },
          warning=function(cond) {
            message(paste("Simulation setup caused a warning for USM:",usm_name[x]))
            message("Here's the original warning message:")
            message(cond)
            return(NULL)
          },
          finally={
            if(Erase){
              unlink(x = USM_path, recursive = T, force = T)
            }
          }
          )
        },dir.orig,dir.targ,usm_name,stics,obs_name,
        Parameter,Plant,Erase,method,Param_val)
    parallel::stopCluster(cl)
  }else{
    outputs=
      lapply(seq_along(usm_name),
             function(x){
               USM_path= file.path(dir.targ,usm_name[x])
               import_usm(dir.orig = dir.orig, dir.targ = dir.targ,
                          usm_name = usm_name[x], overwrite = T,
                          stics = ifelse(method=="stics",
                                         stics[[x]],stics[[1]]))
               set_out_var(filepath= file.path(USM_path,"var.mod"),
                           vars=Out_var, add=F)
               if(method=="Parameter"){
                 # set parameter values using mapply:
                 Param_val_x= lapply(Param_val, function(y){y[x]})
                 mapply(function(y,z){
                   set_param(dirpath = USM_path,
                             param = y,plant = Plant,
                             value = z)
                 },names(Param_val_x),Param_val_x)
               }
               run_stics(dirpath = USM_path)
               output= eval_output(dirpath= USM_path, obs_name= obs_name)
               if(Erase){
                 unlink(x = USM_path, recursive = T, force = T)
               }
               output
             })
  }

  if(erase_dir){
    unlink(x = dir.targ, recursive = T, force = T)
  }

  names(outputs)= usm_name
  stats_out= do.call(function(...)try(stati_stics(...),silent = T),outputs)
  if(inherits(stats_out,"try-error")){
    warning("Can't find ",crayon::red("ANY"),
            " valid observation to perform evaluation")
  }
  outputs[["plot_it"]]= plot_it
  outputs[["Vars"]]= Out_var
  outputs[["Title"]]= Title
  gg_stics= do.call(plot_output,outputs)

  return(list(outputs= outputs[-grep("plot_it|Vars|Title",names(outputs))],
              gg_object= gg_stics,
              stats= stats_out))
}


#' Makes one stics_eval simulation list from several
#'
#' @description Uses [data.table::rbindlist()] but keep the original
#' structure of the list within the list. To use for row binding
#' [stics_eval()] outputs.
#'
#' @param ...   Input simulations lists from [stics_eval()].
#'
#' @details The function keep all columns from the simulations as in `data.table::rbindlist(l,fill= TRUE)`.
#'
#' @seealso [stics_eval()]
#'
#' @importFrom data.table rbindlist
#'
#' @examples
#' \dontrun{
#'
#' Simulation= rbind_sim(Sim_2017, Sim_2018)
#'
#' }
#' @export
#'
rbind_sim= function(...){
  dot_args= list(...)
  bind_s=
    lapply(names(dot_args[[1]]$outputs), function(x){
      data.table::rbindlist(
        lapply(dot_args, function(dot_x,y,x){dot_x[[y]][[x]]}, "outputs",x),
        fill = TRUE)%>%
        as.data.frame()
    })
  names(bind_s)= names(dot_args[[1]]$outputs)
  return(bind_s)
}

