#' Compare STICS simulation outputs (mod_s) to observations (*.obs)
#'
#' @description Compare STICS simulation outputs to observations for sole or mixed crops.
#'
#' @param dirpath  Directory path
#' @param obs_name A vector of observation file name(s). It must have the form
#'                 \code{c(Principal,Associated)} for mixed crops. See details.
#' @param mixed    (optional) Is the simulation made on mixed species (boolean)
#'
#' @details For mixed crops, the \code{obs_name} argument should have the principal plant
#'          observations first in the character vector, and then the associated plant.
#'          If \code{obs_name} is not provided, the function try to guess it using the
#'          built-in algorithm from \code{\link{read_obs}}. Idem for the \code{mixed}
#'          argument. See documentation for more details.
#'
#' @return A data.frame (sole crop) or a list of two data.frames (mixed crops) with
#'         simulated and observed data. Simulated and measured data have the \code{_sim}
#'         and \code{_meas} suffix respectively.
#'
#' @seealso \code{\link{read_output}} and \code{\link{read_obs}}
#'
#' @examples
#'\dontrun{
#' library(sticRs)
#' Table_compare= eval_output()
#'}
#'
#' @export
#'
eval_output= function(dirpath= getwd(), obs_name= NULL, mixed= NULL){
  .= NULL
  sim= read_output(dirpath = dirpath, mixed = mixed)
  meas= read_obs(dirpath = dirpath, filename = obs_name, mixed = mixed)
  colnames(sim)[-grep("Date|Dominance",colnames(sim))]=
    paste0(colnames(sim[-grep("Date|Dominance",colnames(sim))]),"_sim")

  mixed= length(unique(sim$Dominance))>1

  if(!is.null(meas)){
    colnames(meas)[-grep("Date",colnames(meas))]=
      paste0(colnames(meas[-grep("Date",colnames(meas))]),"_meas")
    if(mixed){
      meas$Dominance= "Principal"
      meas$Dominance[meas$Plant==unique(meas$Plant)[2]]= "Associated"
    }else{
      meas$Dominance= "Sole crop"
    }
    Equiv= data.frame(Dominance= unique(meas$Dominance), Measurement= unique(meas$Plant),
                      Simulation= unique(sim$Plant))

  }else{
    if(mixed){
      Equiv= data.frame(Dominance= c("Principal","Associated"), Measurement= rep(NA,2),
                        Simulation= unique(sim$Plant))
      meas= data.frame(Date= sim$Date,
                       Dominance= rep(c("Principal","Associated"),each=nrow(sim)/2))
      attr(meas, "file")= rep(NA,2)
    }else{
      Equiv= data.frame(Dominance= c("Sole crop"), Measurement= NA,
                        Simulation= unique(sim$Plant))
      meas= data.frame(Date= sim$Date,
                       Dominance= rep(c("Sole crop"),each=nrow(sim)))
      attr(meas, "file")= NA
    }
  }

  cat("Input/Output files used for simulation:\n")
  print(Equiv)

  if(mixed){
    Table_comp= merge(sim,meas,by = c("Dominance","Date"),
                      suffixes = c('_sim','_meas'),all.x = T, all.y = F)
    Table_comp[,grep("ian_meas|mo_meas|jo_meas|jul_meas|Plant_meas",
                     colnames(Table_comp))]= NULL
    colnames(Table_comp)[grep("ian_sim|mo_sim|jo_sim|jul_sim|Plant_sim",
                              colnames(Table_comp))]=
      c("ian","mo","jo","jul","Plant")
    attr(Table_comp, 'files')=
      data.frame(sim_Plant= attr(sim, "file")$Plant,
                 sim_file= attr(sim, "file")$file,
                 obs_file= attr(meas, "file"))
  }else{
    Table_comp= merge(sim,meas,by = c("Date"),
                      suffixes = c('_sim','_meas'),all.x = T, all.y = F)
    Table_comp[,grep("ian_meas|mo_meas|jo_meas|jul_meas|Plant_meas",
                     colnames(Table_comp))]= NULL
    colnames(Table_comp)[grep("ian_sim|mo_sim|jo_sim|jul_sim|Plant_sim",
                              colnames(Table_comp))]=
      c("ian","mo","jo","jul","Plant")
    attr(Table_comp, 'files')=
      data.frame(sim_Plant= attr(sim, "file"),
                 sim_file= attr(sim, "file"),
                 obs_file= attr(meas, "file"))
  }

  return(Table_comp)
}
