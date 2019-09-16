compute_doy_cumul <- function(doys_vec, years_vec) {
  #' @title Calculating cumulative day of year from days of year over 2 years
  #' @description For a vector of days of year and a vector of corresponding year
  #' for each one, calculation of cumulative days of years
  #' @param doys_vec vector of days of year
  #' @param years_vec vector of years corresponding to each day of year from doys_vec
  #' @return A vector of cumulative days of year over a 2 years period
  #' @examples
  #' compute_doy_cumul(c(350,360,10,20,30),c(1990,1990,1991,1991,1991))
  #' @export
  # ----------------------------------------------------------------------
  #  MODIFICATIONS (last commit)
  #  $Date: 2019-06-03 11:56:53 +0200 (lun. 03 juin 2019) $
  #  $Author: plecharpent $
  #  $Revision: 1429 $
  # ----------------------------------------------------------------------
  #' @importFrom lubridate leap_year

  if (!all(c(class(doys_vec),class(years_vec)) %in% c("numeric","integer"))) {
    stop("Vectors are not all numeric/integer ones, aborting !")
  }

  # verify vectors length
  if ( ! length(doys_vec) == length(years_vec) ) {
    stop("Vectors are not of same dimensions, aborting !")
  }

  # check if doys_vec contains days of year
  if( ! max(doys_vec) < 367 ) {
    stop("Vector of julian date (doy) contains value(s) over 366, aborting !")
  }

  # checking iof years are contiguous
  u_years = unique(years_vec)

  nb_years <- length(u_years)

  # nothing to do for one year !
  if ( nb_years == 1) {
    return(doys_vec)
  }

  # over 2 years
  if (length(u_years) > 2) {
    warning("Years number is greater than 2, aborting (treatment for 2 years only) !")
    return(NULL)
  }

  # years not contiguous
  if ( u_years[2] != u_years[1] + 1 ) {
    warning("The 2 years are not continuous, aborting !")
    return(NULL)
  }

  cum_vec <- doys_vec
  cum_vec[ years_vec == u_years[2] ] <-
  cum_vec[ years_vec == u_years[2] ]+ 365 + leap_year(u_years[1])

  return(cum_vec)

}
