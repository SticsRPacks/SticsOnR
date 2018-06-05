#' Compare STICS simulation outputs (mod_s) to observations (*.obs)
#'
#' @description Compare STICS simulation outputs to observations for sole or mixed crops.
#'
#' @param dirpath  Directory path
#' @param obs_name A vector of observation file name(s). Optional, see details.
#' @param mixed    (optional) Is the simulation made on mixed species (boolean)
#'
#' @details If \code{obs_name} is not provided, the function try to guess it using the
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


  if(is.list(sim)&length(sim)>1){
    # (1) The simulations were made on mixed species
    Table_comp= lapply(1:length(names(sim)), function(x){
      Out= merge(sim[[x]],meas[[x]],by = c("ian","jul"),
                 suffixes = c('_sim','_meas'))%>%
        .[,-grep("mo_sim|jo_sim",colnames(.))]
      colnames(Out)[grep("mo_meas|jo_meas",colnames(Out))]=
        c("mo","jo")
      Out
    })
    names(Table_comp)= names(sim)
    attr(Table_comp, 'files')= data.frame(sim= names(sim), obs= names(meas))
  }else{
    # (2) The simulations were made on a sole crop
    Table_comp= merge(sim,meas,by = c("ian","jul"),
                      suffixes = c('_sim','_meas'))%>%
      .[,-grep("mo_sim|jo_sim",colnames(.))]
    colnames(Out)[grep("mo_meas|jo_meas",colnames(Out))]=
      c("mo","jo")
    names(Table_comp)= names(sim)
    attr(Table_comp, 'files')= data.frame(sim= names(sim), obs= names(meas))
  }

  return(Table_comp)
}
