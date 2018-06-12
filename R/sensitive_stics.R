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
#' @param Parameters A vector of input parameter names
#' @param Vars       Output variables on which the sensitivity is performed
#' @param method     The sensitivity method to use, see \pkg{sensitivity} package
#' @param Plant      On which plant (\emph{i.e.} Principal or associated) the parameters
#'                   have to be set (only for mixed simulations, using plant parameters)
#' @param ...        Further parameters to give to the sensitivity function used
#'                   (see \pkg{sensitivity} package)
#'
#' @return The sensitivity analysis output
#'
#' @importFrom sensitivity fast99 sobol tell
#' @importFrom dplyr group_by summarise
#'
#' @seealso \code{\link[stats]{Distributions}} if you use \code{\link[sensitivity]{fast99}}.
#'
#' @examples
#'\dontrun{
#' library(sticRs)
#' # Example using the fast99 method:
#' sensitive_stics(dir.orig = "0-DATA/dummy/SC_Wheat",
#'                 dir.targ = "2-Simulations/Sensitivity2",
#'                 stics = "0-DATA/stics_executable/EquDens_trg/stics.exe",
#'                 obs_name = "Wheat_N0.obs",Parameters = "interrang",
#'                 Vars = c("raint", "lai(n)", "masec(n)"),
#'                 method= "fast99", n= 10,
#'                 q= "qunif",q.arg = list(list(min=0.05, max=0.25),
#'                 list(min=140, max=280)))
#'}
#'
#' @export
#'
sensitive_stics= function(dir.orig, dir.targ=getwd(),stics,obs_name,Parameters,
                          Vars,method=c("fast99","sobol"),Plant=1,...){
  .=Date=Dominance=S_Max=S_Mean=S_Min=Sim=meas=plant=sd_meas=NULL
  method= match.arg(method,c("fast99"))

  if(method=="fast99"){
    Design_experiment= sensitivity::fast99(model= NULL,
                                           factors = Parameters, ...)
  }else if(method=="sobol"){
    Design_experiment= sensitivity::sobol(model = NULL, ...)
  }

  Vars_R= gsub("\\(","_",Vars)%>%gsub("\\)","",.)

  DOE= Design_experiment$X
  if(method=="fast99"){
    if(any(is.na(DOE))){"Please increase the n parameter of the fast99 function"}
  }

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

                          lapply(Parameters, function(pa){
                            set_param(dirpath = USM_path, param = pa,
                                      value = DOE[x,pa],plant = plant)
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
                                             paste(Parameters,
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



