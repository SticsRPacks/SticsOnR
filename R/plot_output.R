#' Plot STICS outputs
#'
#' @description Plot stics outputs, compare between USMs, and compare simulations
#' to observations when available.
#'
#' @param ...      Either a file path or the output from \code{\link{eval_output}}
#' If several objects are detected, make a comparison between them.
#' @param Vars     Character vector of variables required for plotting.
#' @param obs_name A vector of observation file name(s). It must have the form
#'                 \code{c(Dominant,Dominated)} for mixed crops. See details.
#' @param Title    A title for the plot (optional)
#' @param plot_it  Boolean. Do the plot as to be pinted ?
#'
#' @details if \code{Vars} is NULL (the default), the function plots all variables
#' from the simulation. The output variables from simulations can be set using
#' \code{\link{set_out_var}}.If \code{obs_name} is not provided, the function tries
#' to guess it using the built-in algorithm from \code{\link{read_obs}}. Idem for
#' the \code{mixed} argument. See respective documentation for more details.
#'
#' @return A ggplot object, and print a plot if \code{plot_it} is set to \code{TRUE}.
#'
#' @importFrom ggplot2 aes geom_line geom_point ggplot labs facet_grid ggtitle geom_errorbar guides
#' @importFrom reshape2 melt
#' @importFrom parallel parLapply stopCluster
#' @importFrom dplyr ungroup group_by summarise "%>%"
#'
#' @examples
#'\dontrun{
#' library(sticRs)
#' # Example 1, uing paths as inputs:
#' plot_output(Sim1= "dummy_path/simulation_1",Sim2= "dummy_path/simulation_2",
#'             obs_name = c("Wheat.obs","Pea.obs"),
#'             Title = "Model comparison for Wheat-Pea in Intercrop")
#'
#' # Example 2, using do.call :
#' Simulations_path= list(Sim1= "dummy_path/simulation_1",Sim2= "dummy_path/simulation_2")
#' # Add arguments passed to plot_output() in the "Simulations_path" list:
#' Simulations_path$Title= "Model comparison for Wheat-Pea in Intercrop"
#' do.call(plot_output,Simulations_path)
#'
#' # Example 3, using simulation outputs:
#' sim1= read_output("dummy_path/simulation_1")
#' sim2= read_output("dummy_path/simulation_2")
#' plot_output(sim1=sim1,sim2=sim2)
#'
#'}
#' @export
#'
plot_output= function(..., Vars=NULL,obs_name=NULL,Title=NULL,plot_it=T){
  Date= Dominance= value= Equ= variable= Version= .= value_min= value_max= NULL
  dot_args= list(...)
  if(any((sapply(dot_args,length)>1)&!(sapply(dot_args,is.data.frame)))){
    stop('Wrong "..." argument format. \nDid you provide outputs in a list or a vector? ',
         "If so, provide them as function arguments instead.")
  }
  Isdf= all(lapply(dot_args, is.data.frame)%>%unlist)
  Ispath= all(lapply(dot_args, is.character)%>%unlist)
  if(!Isdf&Ispath){
    dot_args=
      lapply(dot_args, function(x)eval_output(dirpath = x,obs_name = obs_name))
  }

  V_names= names(dot_args)
  if(is.null(V_names)|length(V_names)<length(dot_args)){
    V_names= paste0("Version_", seq_along(dot_args))
  }
  if(is.null(Vars)){
    Vars= lapply(dot_args, function(x){
      Names= colnames(x)
      Names= gsub("_sim","",Names)
      Names= Names[-grep("Date|Dominance|Plant|ian|mo|jo|jul|_meas",
                         Names)]
      }
    )%>%unlist%>%unique
  }else{
    Vars= gsub("\\(","_",Vars)%>%gsub("\\)","",.)
  }

  x_sim_= x_meas_= .= NULL
  for(i in seq_along(dot_args)){
    x= dot_args[[i]]
    x_sim=
      x[,-grep("_meas|ian|mo|jo|jul",colnames(x))]
    x_sim$Version= V_names[i]
    colnames(x_sim)= gsub("_sim","",colnames(x_sim))
    Vars= paste("Date|Dominance|Version",paste(Vars,collapse = "|"),sep="|")
    x_sim_=
      x_sim[,grep(Vars,colnames(x_sim))]%>%
      reshape2::melt(id.vars = c("Date","Dominance","Version"))%>%
      rbind(x_sim_,.)

    x_meas=
      x[,-grep("_sim|ian|mo|jo|jul",colnames(x))]
    # If x_meas has more than Dominance, Date and Plant columns:
    if(ncol(x_meas)>3){
      x_meas[,-grep("_meas|Date|Dominance|Plant",colnames(x_meas))]= NA
      x_meas$Version= V_names[i]
      colnames(x_meas)= gsub("_meas","",colnames(x_meas))
      x_meas_=
        x_meas[,grep(Vars,colnames(x_meas))]%>%
        reshape2::melt(id.vars = c("Date","Dominance","Version"))%>%
        rbind(x_meas_,.)
    }

  }

  levels(x_sim_$variable)= gsub("_n","",levels(x_sim_$variable))

  ggstics=
    ggplot(x_sim_,aes(x=Date))+
    facet_grid(variable~., scales='free_y') +
    geom_line(aes(y= value, colour= Dominance,linetype= Version))+
    labs(linetype='Simulation',colour='Plant dominance')+
    ggtitle(Title)

  if(!is.null(x_meas_)&
     length(colnames(x_meas_)[
       -grep("Date|Dominance|Version",colnames(x_meas_))])>0&
     !all(is.na(x_meas_$value))){
    # If the observations come from the same source (all equal), make one
    # legend only:
    Same_Obs=
      x_meas_%>%
      group_by(Date,Dominance,variable)%>%
      summarise(Equ= identical_vals(variable))%>%
      group_by(variable)%>%
      summarise(Equ= identical_vals(variable))%>%
      ungroup()%>%
      summarise(all(Equ))%>%unlist()
    if(Same_Obs){
      x_meas_$Version= "Obs."
    }
    levels(x_meas_$variable)= gsub("_n","",levels(x_meas_$variable))
    x_meas_no_sd= x_meas_[!grepl("_sd",x_meas_$variable),]
    ggstics= ggstics+
      geom_point(data= x_meas_no_sd,
                 aes(y= value, colour= Dominance,pch= Version))+
      labs(pch='Observations')
    # If there are sd values in the observation file:
    if(any(grepl("_sd",x_meas_$variable))){

      x_meas_sd_tmp= x_meas_[grepl("_sd",x_meas_$variable),]
      x_meas_sd= x_meas_no_sd
      x_meas_sd$value_min= NA
      x_meas_sd$value_max= NA
      sdvars= unique(x_meas_$variable[grep("_sd",x_meas_$variable)])

      for(i in seq_along(sdvars)){
        x_meas_sd$value_min[grep(gsub("_sd","",sdvars[i]),x_meas_sd$variable)]=
          x_meas_sd$value[grep(gsub("_sd","",sdvars[i]),x_meas_sd$variable)]-
          x_meas_sd_tmp$value[grep(sdvars[i],x_meas_sd_tmp$variable)]
        x_meas_sd$value_max[grep(gsub("_sd","",sdvars[i]),x_meas_sd$variable)]=
          x_meas_sd$value[grep(gsub("_sd","",sdvars[i]),x_meas_sd$variable)]+
          x_meas_sd_tmp$value[grep(sdvars[i],x_meas_sd_tmp$variable)]

      }
      ggstics= ggstics+
        geom_errorbar(data= x_meas_sd,aes(ymin= value_min, ymax= value_max,
                                          colour= Dominance,pch= Version))
    }

  }

  if(plot_it){print(ggstics)}
  invisible(ggstics)
}


identical_vals= function(x){length(unique(x))==1}

