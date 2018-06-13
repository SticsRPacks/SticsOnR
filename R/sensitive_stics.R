#' Make sensitivity analysis of STICS output
#'
#' @description Make sensitivity analysis on a particular output given STICS input
#'
#' @param dir.orig   Path to the directory from which to copy the simulation files. If
#'                   \code{NULL} (the default), uses the package dummy USM.
#' @param dir.targ   Path to the target directory for evaluation. Created if missing.
#' @param stics      STICS executable path
#' @param obs_name   A vector of observation file name(s). It must have the form
#'                   \code{c(Dominant,Dominated)} for mixed crops.
#'                   See \code{\link{read_obs}} \code{filename} parameter for more details.
#' @param Parameters A list of list of min and max values for each parameters, named after
#'                   them (see details and example)
#' @param Vars       Output variables on which the sensitivity is performed
#' @param method     The sensitivity method to use, see \pkg{sensitivity} package
#' @param n          Sample size for simulation (must an even number for `sobol` alike
#'                   methods)
#' @param q          A vector of quantile functions names corresponding to wanted factors
#'                   distributions (see \code{\link[sensitivity]{fast99}} and details).
#'                   \code{\link[sensitivity]{fast99}} and details).
#' @param Plant      On which plant (\emph{i.e.} Principal or associated) the parameters
#'                   have to be set (only for mixed simulations, using plant parameters)
#'                   Set to \code{NULL} if using STICS in sole crop
#' @param ...        Further parameters to give to the sensitivity function used
#'                   (see \pkg{sensitivity} package)
#'
#' @details The function uses the \pkg{sensitivity} package functions under the hood to
#'  create the DOE (design of experiment) and then to compute the sensitivity index. For
#'  `sobol` alike methods, the DOE is first computed using the
#'  \code{\link[sensitivity]{fast99}} function using the `q` and `Parameters` (for
#'  `q.arg`) parameters. The DOE is then splitted in two to fill the `X1`` and `X2`
#'  parameters.
#'  The `Parameters` should take the form of a list of arguments to pass to the `q`
#'  function for each parameter, named after the parameter of interest (see example).
#'
#'
#' @return A list of two :
#' \itemize{
#'   \item A list of ggplot objects to plot the sensitivity of each variable to the
#'   parameter(s) along the rotation
#'   \item A list of the output from the method function, *e.g.* a list of class
#'   `fast99` for the `fast99` method. sensitivity analysis output
#' }
#'
#' @importFrom sensitivity fast99 sobol tell
#' @importFrom dplyr group_by summarise
#' @importFrom caret createDataPartition
#'
#' @seealso \code{\link[stats]{Distributions}} if you use \code{\link[sensitivity]{fast99}}.
#'
#' @examples
#'\dontrun{
#' library(sticRs)
#' # Example using the fast99 method for a sensitivity analysis on the
#' # "interrang" and "P_densitesem" parameters:
#'
#' sensitive_stics(dir.orig = "0-DATA/dummy/SC_Wheat",
#'                 dir.targ = "2-Simulations/Sensitivity2",
#'                 stics = "0-DATA/stics_executable/EquDens_trg/stics.exe",
#'                 obs_name = "Wheat_N0.obs",
#'                 Parameters= list(interrang= list(min=0.05, max=0.25),
#'                                  P_densitesem= list(min=140, max=280)),
#'                 Vars = c("raint", "lai(n)", "masec(n)"),
#'                 method= "fast99", n= 10,
#'                 q= "qunif",q.arg = list(list(min=0.05, max=0.25),
#'                 list(min=140, max=280)))
#'
#' # Example using the sobol method for a sensitivity analysis on the
#' # "interrang" and "P_densitesem" parameters with different quantile functions:
#'
#' sensitive_stics(dir.orig = "0-DATA/dummy/SC_Wheat",
#'                 dir.targ = "2-Simulations/Sensitivity2",
#'                 stics = "0-DATA/stics_executable/EquDens_trg/stics.exe",
#'                 obs_name = "Wheat_N0.obs",
#'                 Parameters= list(interrang= list(min=0.05, max=0.25),
#'                                  P_densitesem= list(mean=210, sd=30)),
#'                 Vars = c("raint", "lai(n)", "masec(n)"),
#'                 method= "fast99", n= 10,
#'                 q= c("qunif","qnorm"),q.arg = list(list(min=0.05, max=0.25),
#'                 list(min=140, max=280)))
#'}
#'
#' @export
#'
sensitive_stics= function(dir.orig, dir.targ=getwd(),stics,obs_name,Parameters,
                          Vars,method=c("fast99","sobol"),n=10*length(Vars),
                          q="qunif",Plant=1,...){
  .=Date=Dominance=S_Max=S_Mean=S_Min=Sim=meas=plant=sd_meas=NULL
  method= match.arg(method,c("fast99","sobol"))

  Design_experiment= sensitivity::fast99(model= NULL,factors = names(Parameters),
                                         n=n,q=q,q.arg=Parameters)
  DOE= Design_experiment$X
  if(any(is.na(DOE))){
    stop("Sample size too little for evaluation, please increase the n parameter")
  }

  if(method=="sobol"){
    if(n%%2==1){stop("n must be an even number to partition equally the input for sobol")}
    Sample_index= sample(x= 1:nrow(DOE),size= floor(nrow(DOE)/2),replace= F)
    X1= DOE[Sample_index,,drop=F]
    X2= DOE[-Sample_index,,drop=F]
    Design_experiment= sensitivity::sobol(model= NULL,X1= X1,X2= X2, ...)
  }

  Vars_R= gsub("\\(","_",Vars)%>%gsub("\\)","",.)

  NbCores= parallel::detectCores()-1
  cl= parallel::makeCluster(min(NbCores,nrow(DOE)))
  parallel::clusterExport(cl=cl,
                          varlist=c("dir.orig","dir.targ","stics",
                                    "obs_name","Vars","import_usm",
                                    "set_out_var","DOE","Parameters",
                                    "run_stics","eval_output",
                                    "set_param","plant"),
                          envir=environment())
  outputs=
    parallel::parLapply(cl,seq_len(nrow(DOE)),
                        function(x,dir.orig,dir.targ,stics,obs_name,
                                 DOE,Parameters,plant){
                          usm_name= paste0("stics_usm","_",x)
                          USM_path= file.path(dir.targ,usm_name)
                          import_usm(dir.orig = dir.orig, dir.targ = dir.targ,
                                     usm_name = usm_name, overwrite = T,
                                     stics = stics)
                          set_out_var(filepath= file.path(USM_path,"var.mod"),
                                      vars=Vars, app=F)

                          lapply(names(Parameters), function(pa){
                            set_param(dirpath = USM_path, param = pa,
                                      value = DOE[x,pa],plant = Plant)
                          })
                          run_stics(dirpath = USM_path)
                          output= eval_output(dirpath= USM_path,
                                              obs_name= obs_name)
                          output$Design= x
                          output
                        },dir.orig,dir.targ,stics,obs_name,DOE,
                        Parameters,plant)
  parallel::stopCluster(cl)
  outputs= as.data.frame(data.table::rbindlist(outputs))
  Vars_R= gsub("\\(","_",Vars)%>%gsub("\\)","",.)


  # Making the plots --------------------------------------------------------

  gg_output=
    lapply(seq_along(Vars),
           function(x){
             output_Var= outputs[,grep(Vars_R[x],colnames(outputs)), drop=F]
             is_meas= any(grepl("_meas",colnames(output_Var)))
             is_sd= any(grepl("_sd",colnames(output_Var)))
             output_Var_sim=
               data.frame(Date= outputs$Date,
                          Dominance= outputs$Dominance_sim,
                          Sim= output_Var[,grep("_sim",colnames(output_Var))],
                          Design= outputs$Design)%>%
               dplyr::group_by(Date,Dominance)%>%
               dplyr::summarise(S_Min= min(Sim),S_Mean= mean(Sim),S_Max= max(Sim))

             if(is_meas){
               output_Var_meas=
                 output_Var[outputs$Design==1,
                            grep("_meas$",colnames(output_Var))]
               colnames(output_Var_meas)=
                 gsub(paste0(Vars_R[x],"_"),"",colnames(output_Var_meas))
               output_Var_sim= data.frame(output_Var_sim,output_Var_meas)
             }

             gg_sens= ggplot2::ggplot(output_Var_sim,aes(x= Date))+
               ggplot2::facet_grid(Dominance~., scales='free_y') +
               ggplot2::geom_line(aes(y=S_Mean, color= "Mean simulation"))+
               ggplot2::geom_ribbon(aes(ymin= S_Min, ymax=S_Max,
                                        color= "Max/Min simulation"),
                                    alpha=0.3)+
               ggplot2::labs(colour="Simulation",
                             title=paste("Sensitivity analysis for the",
                                         Vars_R[x],"variable"),
                             subtitle= paste("Parameter(s) tested:",
                                             paste(names(Parameters),
                                                   collapse = ", ")))
             if(is_meas){
               gg_sens=
                 gg_sens+
                 ggplot2::geom_point(aes(y=meas, shape= "Measured +/- Sd"))+
                 ggplot2::geom_errorbar(aes(ymin= meas+sd_meas,
                                            ymax= meas-sd_meas,
                                            shape= "Measured +/- Sd"))+
                 ggplot2::labs(shape="Measurement")
             }
             gg_sens
           }
    )
  names(gg_output)= Vars

  sensitivity_stics=
    lapply(seq_along(Vars),
           function(x){
             output_Var= outputs[,grep(Vars_R[x],colnames(outputs)), drop=F]
             output_Var= output_Var[,!grepl('meas',colnames(output_Var))]
             sensitivity::tell(Design_experiment,output_Var)
           }
    )
  names(sensitivity_stics)= Vars

  return(list(gg_objects=gg_output, sensi_objects=sensitivity_stics))

}



