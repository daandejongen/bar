#' Estimating a buffered autoregressive model with Least Squares estimation.
#'
#' @param y A numeric vector representing the outcome time series.
#' @param z A numeric vector representing the threshold time series.
#' @param d A vector with one or more values for the delay parameter.
#' @param p0 The order of regime 0.
#' @param p1 The order of regime 1.
#' @param r A numeric vector of length 2 with (boundary) threshold values
#' @param search What type of boundary values of r are specified to find
#' optimal threshold values: "quantile" for quantile values, "scale" for
#' values on the scale of z, "none" if r is fixed
#'
#'
#' @return An object of class bar.
#' @export
#'
#' @examples
#' a <- 1
barfit <- function(y, z = y, d = 1, p0 = 1, p1 = 1,
                   r = c(.1, .9), search = "quantile") {
  # Checks ---------------------------------------------------------------
  # Check AND return match.arg() for `search`
  search <- check_search(search)
  # Checks for correct arguments that throw an error
  check_data(y, z)
  check_dp(d, p0, p1)
  check_r(r)

  # Preps ----------------------------------------------------------------
  x     <- create_x(y, d, p0, p1)
  eff   <- time_eff(y, d, p0, p1)
  y_eff <- y[eff]
  grid  <- create_grid_r(z, r, search)
  grid  <- add_d(d, grid)

  # Optimization of d, r0, r1, and  starting regime ----------------------
  optim <- optim_grid(y_eff, eff, x, z, p0, p1, grid)
  optim$est <- select_min_d(optim$est)

  # Final solution -------------------------------------------------------
  bar <- new_bar(optim, eff, y_eff, x, z, p0, p1)

  return(bar)
}
