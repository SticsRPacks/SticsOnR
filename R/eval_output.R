#' Compare STICS simulation outputs (mod_s) to observations (*.obs)
#'
#' @description Compare STICS simulation outputs to observations for sole or mixed crops.
#'
#' @param dirpath  Directory path
#' @param obs_name A vector of observation file name(s). It must have the form
#'                 \code{c(Dominant,Dominated)} for mixed crops. See details.
#' @param mixed    (optional) Is the simulation made on mixed species (boolean)
#'
#' @details For mixed crops, the \code{obs_name} argument should have the dominant plant
#'          observations first in the character vector, and then the dominated plant.
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
  meas$Dominance= "Dominant"
  meas$Dominance[meas$Plant==unique(meas$Plant)[2]]= "Dominated"
  Equiv= data.frame(Dominance= c("Dominant","Dominated"), Measurement= unique(meas$Plant),
                    Simulation= unique(sim$Plant))
  cat("Equivalence dominant/dominated plants between measurement and simulation files:\n")
  print(Equiv)

  if(is.list(sim)&length(sim)>1){
    # (1) The simulations were made on mixed species
    Table_comp= merge(sim,meas,by = c("Dominance","Date"),
                      suffixes = c('_sim','_meas'),all.x = T)%>%
      .[,-grep("ian_meas|mo_meas|jo_meas|jul_meas",colnames(.))]
    colnames(Table_comp)[grep("ian_sim|mo_sim|jo_sim|jul_sim",colnames(Table_comp))]=
      c("ian","mo","jo","jul")
    attr(Table_comp, 'files')=
      data.frame(sim_Plant= attr(sim, "file")$Plant,
                 sim_file= attr(sim, "file")$file,
                 obs_file= attr(meas, "file"))
  }else{
    # (2) The simulations were made on a sole crop
    Table_comp= merge(sim,meas,by = c("Date"),
                      suffixes = c('_sim','_meas'))%>%
      .[,-grep("mo_sim|jo_sim",colnames(.))]
    colnames(Table_comp)[grep("ian_sim|mo_sim|jo_sim|jul_sim",colnames(Table_comp))]=
      c("ian","mo","jo","jul")
    attr(Table_comp, 'files')=
      data.frame(sim_Plant= attr(sim, "file")$Plant,
                 sim_file= attr(sim, "file")$file,
                 obs_file= attr(meas, "file"))
  }

  return(Table_comp)
}
