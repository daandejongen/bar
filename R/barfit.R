#' Estimating a buffered autoregressive model with Least Squares estimation.
#'
#' @param y A numeric vector representing the outcome time series.
#' @param z A numeric vector representing the threshold time series.
#' @param d A vector with one or more values for the delay parameter.
#' @param p0 The order of regime 0.
#' @param p1 The order of regime 1.
#' @param r A numeric vector of length 2 with (boundary) threshold values
#' @param r_search What type of boundary values of r are specified to find
#' optimal threshold values: "quantile" for quantile values, "scale" for
#' values on the scale of z, "none" if r is fixed.
#' @param r_select When multiple pairs of thresholds yield the same residual
#' sums of squares, which one should be selected? Options: "widest" and "smallest".
#'
#'
#' @return An object of class bar.
#' @export
#'
#' @examples
#' a <- 1
barfit <- function(y, z = y, d = 1, p0 = 1, p1 = 1,
                   r = c(.1, .9), r_search = "quantile", r_select = "widest") {

  search <- check_search_r(r_search, r)
  select <- check_r_select(r_select)
  check_data(y, z)
  check_dp(d, p0, p1)

  x     <- create_x(y, d, p0, p1)
  eff   <- time_eff(y, d, p0, p1)
  grid  <- gridmaker(z, r, search, d, eff)
  n_search <- nrow(grid)

  optim <- optim_grid(y[eff], eff, x, z, p0, p1, grid)

  bar   <- new_bar(y, eff, x, z, p0, p1, optim, select, n_search)

  return(bar)
}
