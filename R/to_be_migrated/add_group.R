
#' Add a group to a simulation output
#'
#' @description Add a group variable to a simulation from [stics_eval()].
#' @details This is usefull when comparing several simulations, to class outputs per simulations
#'
#' @param stics_eval_out The output of a stics_eval simulation
#' @param y              A vector of the group value, either of length 1 or nrow(stics_eval_out$outputs)
#'
#' @seealso [plot_output()] to use this group variable
#' @examples
#'\dontrun{
#' library(SticsOnR)
#' library(magrittr)
#'
#' # Make two simulations on two different years, and compare them in the same plot:
#' # Simulation from 2018 (note the add_group function call at the end)
#' Eval_stics_1=
#'    SticsOnR::stics_eval(dir.orig = "0-DATA/dummy/IC_Wheat_Pea_2018",
#'              dir.targ = "2-Simulations/Parameterization",
#'              stics = list(EquivDens= "0-DATA/stics_executable/EquDens/stics.exe"),
#'              obs_name =  c("6_IC_Wheat_N02.obs","6_IC_Pea_N02.obs"),
#'              Out_var = c("lai(n)", "masec(n)","hauteur"),
#'              Title = "Wheat-Pea Auzeville 2005-2006 N0", plot_it = T)%>%
#'              add_group("2017-2018")
#'
#' # Simulation from 2017 (note the add_group function call at the end):
#' Eval_stics_2=
#'    SticsOnR::stics_eval(dir.orig = "0-DATA/dummy/IC_Wheat_Pea_2017",
#'              dir.targ = "2-Simulations/Parameterization",
#'              stics = list(EquivDens= "0-DATA/stics_executable/EquDens/stics.exe"),
#'              obs_name =  c("6_IC_Wheat_N02.obs","6_IC_Pea_N02.obs"),
#'              Out_var = c("lai(n)", "masec(n)","hauteur"),
#'              Title = "Wheat-Pea Auzeville 2005-2006 N0", plot_it = T)%>%
#'              add_group("2016-2017")
#'
#' # Binding the two simulations:
#' Eval_stics= SticsOnR::rbind_sim(Eval_stics_1, Eval_stics_2)
#'
#' # plotting the results:
#' plot_output(Eval_stics$outputs)
#'
#'}
#' @export
#'
add_group= function(stics_eval_out,y){
  stics_eval_out$outputs=
    lapply(stics_eval_out$outputs, function(x){
      x$group= y
      x
    })
  stics_eval_out
}
