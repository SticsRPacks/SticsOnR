#' Compute simulated/observed statistics
#'
#' @description Compute statistics for evaluation of the STICS outputs against observations. This function can
#'              be used for one USM or to compare outputs from different model versions or parameter values.
#'
#' @param ...      Either a folder path or the output from [eval_output()]. If several objects are
#'                 detected, make a comparison between them.
#' @param obs_name A vector of observation file name(s). Optionnal, required only if `...` is a link.
#'                 Must have the form `c(Dominant,Dominated)` for mixed crops. See details.
#'
#' @details If `obs_name` is not provided, the function tries to guess it using the built-in algorithm
#'          from [read_obs()]. See respective documentation for more details.
#'
#' @note Because this function has the purpose to assess model quality, all statistics
#'       are computed on dates were observations are present only. So the simulation mean
#'       is only the mean on dates with observations, not the overall simulation mean.
#'
#' @return A data.frame with statistics for each simulation. The data.frame has a
#'         `description` attribute that gives the description of the column names
#'
#' @seealso This function is largely inspired from the `evaluate()` function from the
#'          `SticsEvalR` package
#'
#' @importFrom reshape2 melt
#' @importFrom parallel parLapply stopCluster
#' @importFrom dplyr ungroup group_by summarise "%>%" filter
#' @importFrom stats sd
#' @examples
#'\dontrun{
#' library(sticRs)
#' # Exemple 1:
#' tests= stati_stics("dummy/path/simulation",
#'                    obs_name = c("Wheat.obs","Pea.obs"))
#' attr(tests,which = "description")
#'
#' # Exemple 2, stats for a comparison between simulations:
#' stati_stics(Simulation_1,Simulation_2,
#'             obs_name = c("Wheat.obs","Pea.obs"))
#' # Equivalent to:
#' stati_stics("dummy/path/simulation_1","dummy/path/simulation_2",
#'             obs_name = c("Wheat.obs","Pea.obs"))
#'
#'}
#'
#' @export
#'
stati_stics= function(...,obs_name=NULL){
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

  Vars= lapply(dot_args, function(x){
    Names= colnames(x)
    Names= gsub("_sim","",Names)
    Names= Names[-grep("Date|Dominance|Plant|ian|mo|jo|jul|_meas",
                       Names)]
  }
  )%>%unlist%>%unique

  x_sim_= x_meas_= Dominance=Version= mean_obs= mean_sim= n= obs=
    sd_obs= sd_sim= sim= .= variable= NULL

  for(i in seq_along(dot_args)){
    x= dot_args[[i]]
    x_sim=
      x[,-grep("_meas|ian|mo|jo|jul",colnames(x))]
    x_sim$Version= V_names[i]
    colnames(x_sim)= gsub("_sim","",colnames(x_sim))
    Vars= paste("Date|Dominance|Version",paste(Vars,collapse = "|"),sep="|")
    x_sim_=
      x_sim[,grep(Vars,colnames(x_sim))]%>%
      reshape2::melt(id.vars = c("Date","Dominance","Version"),value.name = "sim")%>%
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
        reshape2::melt(id.vars = c("Date","Dominance","Version"),value.name = "obs")%>%
        rbind(x_meas_,.)
    }
  }

  if(!is.null(x_meas_)&
     length(colnames(x_meas_)[
       -grep("Date|Dominance|Version",colnames(x_meas_))])>0&
     !all(is.na(x_meas_$obs))){
    x=
      merge(x_meas_,x_sim_)%>%
      dplyr::filter(!is.na(obs))%>%
      dplyr::group_by(variable,Dominance,Version)%>%
      dplyr::summarise(n_obs= n(),
                       mean_obs= mean(obs, na.rm = T),
                       mean_sim= mean(sim, na.rm = T),
                       sd_obs= sd(obs, na.rm = T),
                       sd_sim= sd(sim, na.rm = T),
                       CV_obs= (sd_obs/mean_obs)*100,
                       CV_sim= (sd_sim/mean_sim)*100,
                       R2= R2(sim = sim, obs = obs),
                       SS_res= SS_res(sim = sim, obs = obs),
                       RMSE= RMSE(sim = sim, obs = obs),
                       nRMSE= nRMSE(sim = sim, obs = obs),
                       MAE= MAE(sim = sim, obs = obs),
                       FVU= FVU(sim = sim, obs = obs),
                       MSE= MSE(sim = sim, obs = obs),
                       EF= EF(sim = sim, obs = obs),
                       Bias= Bias(sim = sim, obs = obs),
                       ABS= ABS(sim = sim, obs = obs),
                       MAPE= MAPE(sim = sim, obs = obs),
                       RME= RME(sim = sim, obs = obs)
      )

    attr(x, "description")=
      data.frame(n_obs= "Number of observations",
                 mean_obs= "Mean of the observations",
                 mean_sim= "Mean of the simulation",
                 sd_obs= "Standard deviation of the observations",
                 sd_sim= "Standard deviation of the simulation",
                 CV_obs= "Coefficient of variation of the observations",
                 CV_sim= "Coefficient of variation of the simulation",
                 R2= "coefficient of determination for obs~sim",
                 SS_res= "Residual sum of squares",
                 RMSE= "Root Mean Squared Error",
                 nRMSE= "Normalized Root Mean Squared Error, CV(RMSE)",
                 MAE= "Mean Absolute Error",
                 FVU= "Fraction of variance unexplained",
                 MSE= "Mean squared Error",
                 EF= "Model efficiency",
                 Bias= "Bias",
                 ABS= "Mean Absolute Bias",
                 MAPE= "Mean Absolute Percentage Error",
                 RME= "Relative mean error (%)"
      )
  }else{
    stop("Can't find ",crayon::red("ANY")," valid observation for ",
         crayon::red("ANY")," model version")
  }
  return(x)
}

#' Model quality assessment
#'
#' @description
#' Provide several metrics to assess the quality of the predictions of a model (see note) against
#' observations.
#'
#' @param obs       Observed values
#' @param sim       Simulated values
#' @param na.rm     Boolean. Remove `NA` values if `TRUE` (default)
#' @param na.action A function which indicates what should happen when the data contain NAs.
#'
#' @details The statistics for model quality can differ between sources. Here is a
#'          short description of each statistic and its equation (see html version
#'          for `LATEX`):
#' \itemize{
#'   \item `R2()`: coefficient of determination, computed using [stats::lm()] on obs~sim.
#'   \item `SS_res()`: residual sum of squares (see notes).
#'   \item `RMSE()`: Root Mean Squared Error, computed as
#'             \deqn{RMSE = \sqrt{\frac{\sum_1^n(\hat{y_i}-y_i)^2}{n}}}{RMSE = sqrt(mean((sim-obs)^2)}
#'   \item `NSE()`: Nash-Sutcliffe Efficiency, alias of EF, provided for user convenience.
#'   \item `nRMSE()`: Normalized Root Mean Squared Error, also denoted as CV(RMSE), and computed as:
#'              \deqn{nRMSE = \frac{RMSE}{\hat{y}}\cdot100}{nRMSE = (RMSE/mean(obs))*100}
#'   \item `MAE()`: Mean Absolute Error, computed as:
#'            \deqn{MAE = \frac{\sum_1^n(\left|\hat{y_i}-y_i\right|)}{n}}{MAE = mean(abs(sim-obs))}
#'   \item `ABS()`: Mean Absolute Bias, which is an alias of `MAE()`
#'   \item `FVU()`: Fraction of variance unexplained, computed as:
#'            \deqn{FVU = \frac{SS_{res}}{SS_{tot}}}{FVU = SS_res/SS_tot}
#'   \item `MSE()`: Mean squared Error, computed as:
#'            \deqn{MSE = \frac{1}{n}\sum_{i=1}^n(Y_i-\hat{Y_i})^2}{MSE = mean((sim-obs)^2)}
#'   \item `EF()`: Model efficiency, also called Nash-Sutcliffe efficiency (NSE). This statistic is
#'           related to the FVU as \eqn{EF= 1-FVU}. It is also related to the \eqn{R^2}{R2}
#'           because they share the same equation, except SStot is applied relative to the
#'           identity function (*i.e.* 1:1 line) instead of the regression line. It is computed
#'           as: \deqn{EF = 1-\frac{SS_{res}}{SS_{tot}}}{EF = 1-SS_res/SS_tot}
#'   \item `Bias()`: Modelling bias, simply computed as:
#'             \deqn{Bias = \frac{\sum_1^n(\hat{y_i}-y_i)}{n}}{Bias = mean(sim-obs)}
#'   \item `MAPE()`: Mean Absolute Percent Error, computed as:
#'            \deqn{MAPE = \frac{\sum_1^n(\frac{\left|\hat{y_i}-y_i\right|}{y_i})}{n}}{
#'            MAPE = mean(abs(obs-sim)/obs)}
#'   \item `RME()`: Relative mean error (\%), computed as:
#'            \deqn{RME = \frac{\sum_1^n(\frac{\hat{y_i}-y_i}{y_i})}{n}}{RME = mean((sim-obs)/obs)}
#' }
#'
#' @note \eqn{SS_{res}}{SS_res} is the residual sum of squares and \eqn{SS_{tot}}{SS_tot} the total
#'       sum of squares. They are computed as:
#'       \deqn{SS_{res} = \sum_{i=1}^n (y_i - \hat{y_i})^2}{SS_res= sum((obs-sim)^2)}
#'       \deqn{SS_{tot} = \sum_{i=1}^{n}\left(y_{i}-\bar{y}\right)^2}{SS_tot= sum((obs-mean(obs))^2}
#'       Also, it should be noted that \eqn{y_i} refers to the observed values and \eqn{\hat{y_i}} to
#'       the predicted values, and \eqn{\bar{y}} to the mean value of observations.
#'
#' @return A statistic depending on the function used.
#'
#' @seealso This function was inspired from the `evaluate()` function
#'          from the `SticsEvalR` package. This function is used by [stics_eval()]
#'
#' @name predictor_assessment
#'
#' @importFrom dplyr "%>%"
#' @importFrom stats lm sd var na.omit
#'
#' @examples
#' library(sticRs)
#' sim= rnorm(n = 5,mean = 1,sd = 1)
#' obs= rnorm(n = 5,mean = 1,sd = 1)
#' RMSE(sim,obs)
NULL


#' @export
#' @rdname predictor_assessment
R2= function(sim,obs, na.action= stats::na.omit){
  .= NULL
  stats::lm(formula = obs~sim, na.action= na.action)%>%summary(.)%>%.$adj.r.squared
}

#' @export
#' @rdname predictor_assessment
SS_res= function(sim,obs,na.rm= T){
  sum((obs-sim)^2, na.rm = na.rm) # residual sum of squares
}

#' @export
#' @rdname predictor_assessment
RMSE= function(sim,obs,na.rm= T){
  sqrt(mean((sim-obs)^2, na.rm = na.rm))
}

#' @export
#' @rdname predictor_assessment
nRMSE= function(sim,obs,na.rm= T){
  (RMSE(sim = sim, obs = obs, na.rm= na.rm)/
     mean(obs, na.rm = na.rm))*100
}

#' @export
#' @rdname predictor_assessment
MAE= function(sim,obs,na.rm= T){
  mean(abs(sim-obs), na.rm = na.rm)
}

#' @export
#' @rdname predictor_assessment
ABS= function(sim,obs,na.rm= T){
  MAE(sim,obs,na.rm)
}

#' @export
#' @rdname predictor_assessment
MSE= function(sim,obs,na.rm= T){
  mean((sim-obs)^2,na.rm = na.rm)
}

#' @export
#' @rdname predictor_assessment
EF=  function(sim,obs,na.rm= T){
  # Modeling efficiency
  SStot= sum((obs-mean(obs,na.rm= na.rm))^2, na.rm = na.rm) # total sum of squares
  # SSreg= sum((sim-mean(obs))^2) # explained sum of squares
  1-SS_res(sim = sim, obs = obs, na.rm = na.rm)/SStot
}

#' @export
#' @rdname predictor_assessment
NSE= function(sim,obs,na.rm= T){
  EF(sim, obs, na.rm = na.rm)
}

#' @export
#' @rdname predictor_assessment
Bias= function(sim,obs,na.rm= T){
  mean(sim-obs,na.rm = na.rm)
}

#' @export
#' @rdname predictor_assessment
MAPE= function(sim,obs,na.rm= T){
  mean(abs(sim-obs)/obs,na.rm = na.rm)
}

#' @export
#' @rdname predictor_assessment
FVU=  function(sim,obs,na.rm= T){
  var(obs-sim,na.rm = na.rm)/var(obs,na.rm = na.rm)
}

#' @export
#' @rdname predictor_assessment
RME=  function(sim,obs,na.rm= T){
  mean((sim-obs)/obs, na.rm = na.rm)
}
