#' Plot STICS outputs
#'
#' @description Plot stics outputs, compare between USMs, and compare simulations
#' to observations when available.
#'
#' @param ... output data.frame(s), generally the output from \code{\link{eval_output}}
#' to compare with observations. If several objects are detected, make a comparison between
#' them.
#'
#' @examples
#'\dontrun{
#' library(sticRs)
#' plot_output()
#'}
#' @export
#'
plot_output= function(..., Vars=NULL){
  dot_args= list(...)
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
    x_meas=
      x[,-grep("_sim|ian|mo|jo|jul",colnames(x))]
    x_meas[,-grep("_meas|Date|Dominance|Plant",colnames(x_meas))]= NA
    x_sim$Version= x_meas$Version= V_names[i]
    colnames(x_sim)= gsub("_sim","",colnames(x_sim))
    colnames(x_meas)= gsub("_meas","",colnames(x_meas))

    Vars= paste("Date|Dominance|Version",paste(Vars,collapse = "|"),sep="|")
    x_sim_=
      x_sim[,grep(Vars,colnames(x_sim))]%>%
      reshape2::melt(id.vars = c("Date","Dominance","Version"))%>%
      rbind(x_sim_,.)
    x_meas_=
      x_meas[,grep(Vars,colnames(x_meas))]%>%
      reshape2::melt(id.vars = c("Date","Dominance","Version"))%>%
      rbind(x_meas_,.)
  }

  ggstics=
    ggplot(x_sim_,aes(x=Date))+
    facet_grid(variable~., scale='free_y') +
    geom_line(aes(y= value, colour= Dominance,linetype= Version))+
    geom_point(data= x_meas_,aes(y= value, colour= Dominance,
                                 pch= Version))+
    labs(pch='Observation source',linetype='Model Version',colour='Plant dominance')

  print(ggstics)
  invisible(ggstics)
}
