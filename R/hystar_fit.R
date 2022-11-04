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
#' @param p_select Which information criterion should be minimized to select
#' the orders p0 and p1. Choices:
#' * "aic" (Akaike Information Criterion)
#' * "aicc" (Corrected Akaike Information Criterion)
#' * "bic" (default, Bayesian Information Criterion)
#' @param r A vector or a matrix. If `r` is a vector, its length must be 2,
#'     such that it represents the interval in which the threshold value
#'     should be searched. If `r` is a matrix, it must have two columns,
#'     such that each row represents a pair \eqn{r_0 \le r_1} to test.
#'     You can use a matrix with one row if you don't want to estimate
#'     the thresholds. Defaults to `c(.1, .9)`.
#' @param r_type What type of values are given in `r`:
#'
#' * `"quantile"` (default) for quantile values and
#' * `"scale"` for values on the scale of `z`.
#'
#' @param r_select When multiple pairs of thresholds yield the same residual
#'     sums of squares, which one should be selected? Options:
#' * `"smallest"` (default): \eqn{\mathrm{argmin}_{r_0, r_1} \{|r_0 - r_1|\} },
#' * `"widest"`: \eqn{\mathrm{argmax}_{r_0, r_1} \{|r_0 - r_1|\} }.
#'
#' @return An object of S3 class `hystar`, which is a list containing the following
#' items:
#' * `$data`. A `data.frame` of class `hystar_data`, containing
#'     * `y`, the outcome variable
#'     * `z`, the threshold variable
#'     * `H`, a logical vector that indicates at which time points the hysteresis
#' effect is happening. Note that this vector starts with `NA`(s), since the first
#' \eqn{\max\{p_0, p_1, d\}} values are not predicted in the HysTAR model.
#'     * `R`, the regime indicator vector. (Also starts with `NA`(s).)
#'
#' * `$residuals`. Also accessible with the `residuals()` S3 method.
#' * `$coefficients`, a named vector with the estimated coefficients.
#' With the `residuals()` S3 method, the residuals are represented in a matrix.
#' Use the `confint()` method to get the confidence intervals of the estimates.
#' * `$delay`, a named scalar with the estimate for the delay parameter.
#' * `$thresholds`, a named vector with the estimates of the tresholds.
#' * `$orders`, a named vector with the estimates of the orders.
#' * `$resvar`, a named vector with the estimates of the residual variances.
#' * `$rss`, the minimized residual sum of squares.
#' * `$ic`, a named vector with the aic, the corrected aic and the bic.
#' * `$n`, a named vector with the total effective observations and the
#' effective obeservations in regime 0 and regime 1.
#' * `$eff`, a vector with the time indicators of the effective observations.
#' * `$equiv`, a matrix containing equivalent estimates for \eqn{d, p_0} and \eqn{p_1},
#' i.e., estimates that imply exactly the same regime indicator vector, and
#' as a result the same minimal residual sum of squares.
#'
#' S3 methods:
#' *  The `hystar_data` class has a `plot()` method.
#' * `summary()`, this also provides the p-values and standard errors for the
#' estimates of
#' \eqn{\phi_0^{(0)}, \dots, \phi_{p_0}^{(0)}, \phi_0^{(1)}, \dots, \phi_{p_1}^{(1)}}.
#' * `print()`
#' * `coef()`
#' * `confint()`
#' * `residuals()`
#' * `fitted()`
#' * `nobs()`
#'
#' @export
#'
#' @examples
#' model <- hystar_fit(y = y, z = z, d = c(1, 3), p0 = 1:4, p1 = 2)
#' summary(model)
#' plot(model$data)
hystar_fit <- function(y, z = y, d = 1,
                       p0 = 1, p1 = 1, p_select = "bic",
                       r = c(.1, .9), r_type = "quantile", r_select = "smallest",
                       thin = TRUE) {

  check_input <- check_hystar_fit_input(y, z, d,
                                        p0, p1, p_select,
                                        r, r_type, r_select)
  r_type <- check_input[1]
  r_select <- check_input[2]
  ic_method <- check_input[3]

  eff <- time_eff(y, max(d), max(p0), max(p1))
  x <- create_x(y, eff, max(p0), max(p1))
  grid <- if (is.matrix(r)) r else create_grid(z, r, r_type, d, eff, thin)
  p_options <- create_p_options(p0, p1)

  OPT <- optim_p(y, x, z, eff, grid, p_options, r_select, ic_method)
  est <- OPT$est
  equiv <- OPT$equiv
  model <- run_model(y, x, z, eff, est["p0"], est["p1"],
                   est["d"], est["r0"], est["r1"], est["s"],
                   return_HR = TRUE)
  hystar <- new_hystar(y, x, z, eff, est, model, equiv)

  return(hystar)
}
