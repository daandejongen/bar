#' Estimating the hysTAR model using least squares estimation
#'
#' The hysTAR model
#'
#' @param y A numeric vector representing the outcome time series.
#' @param z A numeric vector representing the threshold time series.
#'     If `z` = `y` (default), the model is a hysSETAR
#'     (hysteretic self-exciting threshold autoregressive) model.
#' @param d A numeric vector with one or more values for the search space
#'     of the delay parameter.
#' @param p0 A numeric vector with one or more values for the search space
#'     of the autoregressive order of Regime 0.
#' @param p1 A numeric vector with one or more values for the search space
#'     of the autoregressive order of Regime 1. By default equal to p0.
#' @param r A vector or a matrix. If `r` is a vector, its length must be 2,
#'     such that it represents the interval in which the threshold value
#'     should be searched. If `r` is a matrix, it must have two columns,
#'     such that each row represents a pair \eqn{r_0 \le r_1} to test.
#'     You can use a matrix with one row if you don't want to estimate
#'     the thresholds.
#' @param r_type What type of values are gievn in `r`.
#'     `"quantile"` for quantile values and
#'     `"scale"` for values on the scale of `z`.
#' @param r_select When multiple pairs of thresholds yield the same residual
#'     sums of squares, which one should be selected?
#'     Options: `"widest"` or `"smallest"` (default).
#'
#' @return An object of class bar.
#' @export
#'
#' @examples
#' model <- barfit(y = y, z = z, d = c(1, 3), p0 = 1:4, p1 = 2)
#' summary(model)
#' plot(model$data)
hystar_fit <- function(y, z = y, d = 1, p0 = 1, p1 = p1,
                       r = c(.1, .9), r_type = "quantile", r_select = "smallest") {

  check_input <- check_hystar_fit_input(y, z, d, p0, p1, r, r_type, r_select)
  r_type      <- check_input[1]
  r_select    <- check_input[2]

  eff         <- time_eff(y, max(d), max(p0), max(p1))
  x           <- create_x(y, eff, max(p0), max(p1))
  grid        <- create_grid(z, r, r_type, d, eff)
  optim       <- optim_grid(y[eff], eff, x, z, p0, p1, grid)$est

  bar         <- new_bar(y, eff, x, z, p0, p1, optim, r_select)

  return(bar)
}
