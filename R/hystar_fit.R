#' Estimating the hysTAR model using least squares estimation
#'
#' @description
#' The hysTAR model is defined as:
#'
#' \eqn{
#'     y_t =
#'     \begin{cases}
#'     \phi_0^{(0)} + \phi_1^{(0)} y_{t-1} + \cdots + \phi_{p_0}^{(0)}
#'     y_{t-p_0} + \sigma_{(0)} \varepsilon_t & \text{if}~R_{t} = 0 \\
#'     \phi_0^{(1)} + \phi_1^{(1)} y_{t-1} + \cdots + \phi_{p_1}^{(1)}
#'     y_{t-p_1} + \sigma_{(1)} \varepsilon_t & \text{if}~R_{t} = 1, \\
#'     \end{cases}
#' }
#'
#' where
#' \eqn{
#'     R_t = \begin{cases}
#'     0 & \mathrm{if} \, z_{t-d} \in (-\infty, r_{0}] \\
#'     R_{t-1} & \mathrm{if} \, z_{t-d} \in (r_0, r_1] \\
#'     1 & \mathrm{if} \, z_{t-d} \in (r_1, \infty), \\
#'     \end{cases}
#' }
#'
#' @details
#' Coefficient estimates are offered in various ways:
#'     as the element of the `hystar` (list) object: a named vector.
#'     in `coef(hystar)`: as a 2 x max(order)+1 matrix, only estimates.
#'     in `summary(hystar)`: a matrix with the SEs and p-values.
#'     in `print(hystar)`: in the model equations.
#'
#' @param y A numeric vector representing the outcome time series.
#' @param z A numeric vector representing the threshold time series.
#'     If `z` = `y` (default), the model is a hysSETAR
#'     (hysteretic self-exciting threshold autoregressive) model.
#' @param d A numeric vector with one or more values for the search space
#'     of the delay parameter. Defaults to 1.
#' @param p0 A numeric vector with one or more values for the search space
#'     of the autoregressive order of Regime 0. Defaults to 1.
#' @param p1 Same as `p0`, but for regime 1.
#' @param r A vector or a matrix. If `r` is a vector, its length must be 2,
#'     such that it represents the interval in which the threshold value
#'     should be searched. If `r` is a matrix, it must have two columns,
#'     such that each row represents a pair \eqn{r_0 \le r_1} to test.
#'     You can use a matrix with one row if you don't want to estimate
#'     the thresholds. Defaults to \eqn{(0.1, 0.9)}.
#' @param r_type What type of values are given in `r`:
#'
#' * `"quantile"` (default) for quantile values and
#' * `"scale"` for values on the scale of `z`.
#'
#' @param r_select When multiple pairs of thresholds yield the same residual
#'     sums of squares, which one should be selected? Options:
#'
#' * `"smallest"` (default): \eqn{\mathrm{argmin}_{r_0, r_1} \{|r_0 - r_1|\} },
#' * `"widest"`: \eqn{\mathrm{argmax}_{r_0, r_1} \{|r_0 - r_1|\} }.
#'
#' @return An object of class [bar].
#' @export
#'
#'
#' @examples
#' model <- barfit(y = y, z = z, d = c(1, 3), p0 = 1:4, p1 = 2)
#' summary(model)
#' plot(model$data)
hystar_fit <- function(y, z = y, d = 1, p0 = 1, p1 = 1,
                       r = c(.1, .9), r_type = "quantile", r_select = "smallest") {

  check_input <- check_hystar_fit_input(y, z, d, p0, p1, r, r_type, r_select)
  r_type      <- check_input[1]
  r_select    <- check_input[2]

  eff         <- time_eff(y, max(d), max(p0), max(p1))
  x           <- create_x(y, eff, max(p0), max(p1))
  grid        <- create_grid(z, r, r_type, d, eff)
  optim       <- optim_grid(y[eff], eff, x, z, p0, p1, grid)$est

  bar         <- new_hystar(y, eff, x, z, p0, p1, optim, r_select)

  return(bar)
}
