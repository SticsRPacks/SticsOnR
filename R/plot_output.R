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
#'
#' @details if \code{Vars} is NULL (the default), the function plots all variables
#' from the simulation. The output variables from simulations can be set using
#' \code{\link{set_out_var}}.If \code{obs_name} is not provided, the function try
#' to guess it using the built-in algorithm from \code{\link{read_obs}}. Idem for
#' the \code{mixed} argument. See documentation for more details.
#'
#' @importFrom ggplot2 aes geom_line geom_point ggplot labs facet_grid
#' @importFrom reshape2 melt
#'
#' @examples
#'\dontrun{
#' library(sticRs)
#' plot_output()
#'}
#' @export
#'
plot_output= function(..., Vars=NULL,obs_name=NULL){
  Date= Dominance= value= Version= NULL
  dot_args= list(...)

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
  }

  x_sim_= x_meas_= .= NULL
  for(i in 1:length(dot_args)){
    x= dot_args[[i]]
    x_sim=
      x[,-grep("_meas|ian|mo|jo|jul",colnames(x))]
    x_sim$Version= x_meas$Version= V_names[i]
    colnames(x_sim)= gsub("_sim","",colnames(x_sim))
    Vars= paste("Date|Dominance|Version",paste(Vars,collapse = "|"),sep="|")
    x_sim_=
      x_sim[,grep(Vars,colnames(x_sim))]%>%
      reshape2::melt(id.vars = c("Date","Dominance","Version"))%>%
      rbind(x_sim_,.)

    x_meas=
      x[,-grep("_sim|ian|mo|jo|jul",colnames(x))]
    # If x_meas as more than Dominance, Date and Plant columns:
    if(ncol(x_meas)>3){
      x_meas[,-grep("_meas|Date|Dominance|Plant",colnames(x_meas))]= NA
      colnames(x_meas)= gsub("_meas","",colnames(x_meas))
      x_meas_=
        x_meas[,grep(Vars,colnames(x_meas))]%>%
        reshape2::melt(id.vars = c("Date","Dominance","Version"))%>%
        rbind(x_meas_,.)
    }

  }

  ggstics=
    ggplot(x_sim_,aes(x=Date))+
    facet_grid(variable~., scale='free_y') +
    geom_line(aes(y= value, colour= Dominance,linetype= Version))+
    labs(linetype='Model Version',colour='Plant dominance')

  if(!is.null(x_meas_)){
    ggstics= ggstics+geom_point(data= x_meas_,aes(y= value, colour= Dominance,
                                                  pch= Version))+
      labs(pch='Observation source')
  }

  print(ggstics)
  invisible(ggstics)
}
