#' Save and Read sensitivity analysis of STICS
#'
#' @description Helper functions to conveniently read or save sensitivity outputs from
#'              the \code{\link{sensitive_stics}} function
#'
#' @param x       The output from the \code{\link{sensitive_stics}} function
#' @param dirpath Path to the directory where to save the plots and objects
#' @param device  Device to use for the \code{\link[ggplot2]{ggsave}} function
#'                that is used under the hood. From its documentation: Can be either
#'                a device function (e.g. png()), or one of "eps", "ps", "tex" (pictex),
#'                "pdf", "jpeg", "tiff", "png", "bmp", "svg" or "wmf" (windows only)
#' @param width,height   Plot dimensions in \code{units} parameter units
#' @param units   Units to use for the plot size parameters
#'                them (see details and example)
#' @param ...     Further parameters to pass to \code{\link[ggplot2]{ggsave}}
#'
#' @details The function first save all the `gg_objects` from the output of
#'          \code{\link{sensitive_stics}} as images, save the full `x`object using
#'          \code{\link[base]{saveRDS}}, and write a readme file for convenience.
#'
#'
#' @importFrom ggplot2 ggsave
#'
#' @seealso \code{\link[ggplot2]{ggsave}}, \code{\link[base]{saveRDS}} and
#'          \code{\link[base]{readRDS}}
#'
#' @examples
#'\dontrun{
#' library(sticRs)
#'
#' # Making a sensitivity analysis on two parameters:
#'
#' sens=
#'   sensitive_stics(dir.orig = "0-DATA/dummy/SC_Wheat",
#'                   dir.targ = "Sensitivity",
#'                   stics = "0-DATA/stics_executable/original/stics.exe",
#'                   obs_name = "Wheat_N0.obs",
#'                   Parameters= list(interrang= list(min=0.05, max=0.25),
#'                                    P_densitesem= list(min=140, max=280)),
#'                   Vars = c("raint", "lai(n)", "masec(n)"),
#'                   method= "fast99", n= 10,
#'                   q= "qunif",q.arg = list(list(min=0.05, max=0.25),
#'                   list(min=140, max=280)))
#'
#' # Saving the output :
#'
#' save_sensi(x = sens,dirpath = "3-Results/Sensitivity",
#'            width = 16,height = 9,units = "in")
#'
#' sens= load_sensi(dirpath = "3-Results/Sensitivity")
#'}
#' @aliases load_sensi
#'
#' @export
#'
save_sensi= function(x,dirpath= getwd(), device="png",width=NULL,
                     height=NULL, units=c("in", "cm", "mm"),...){

  units= match.arg(units,c("in", "cm", "mm"))
  if(!dir.exists(dirpath)){dir.create(dirpath,recursive= T)}
  if(!is.null(width)&!is.null(height)){
    lapply(seq_along(x$gg_objects), function(i){
      ggplot2::ggsave(filename= paste0(names(x$gg_objects)[i],".",device),
                      plot= x$gg_objects[[i]],device= device,path= dirpath,
                      width= width, height= height, units= units,...)
    })
  }
  saveRDS(object = x, file = file.path(dirpath,"sensitivity.rds"))
  writeLines(
    text= paste0("Sensitivity analysis on the STICS model using the ",
                "sensitive_stics() and save_sensi() functions from the ",
                "sticRs package (https://github.com/VEZY/sticRs).\n",
                "Parameters used:\n",
                paste(names(x$sensi_objects[[1]]$X), collapse = ", "),
                "\nOutput variables on which the effect of the parameters ",
                "where tested:\n",
                paste(names(x$sensi_objects), collapse = ", "),
                "\nPlease use the sticRs::load_sensi() function to ",
                "load the sensitivity analysis back to R."),
    con= file.path(dirpath,"readme.txt"))
}

#' @rdname save_sensi
#' @export
load_sensi= function(dirpath= getwd()){
  readRDS(file.path(dirpath,"sensitivity.rds"))
}
