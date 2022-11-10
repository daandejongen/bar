#' Estimate the hysTAR model using conditional least squares estimation
#'
#' @description
#' This function allows you to estimate the parameters of the hysteretic threshold
#' autoregressive (hysTAR) model.
#'
#' @inherit hystar_sim author
#' @inheritSection hystar_sim The hysTAR model
#' @inherit hystar_sim references
#' @inherit hystar_sim examples
#'
#' @param y A numeric vector representing the outcome time series. Can be simulated
#'     with [hystar_sim()].
#' @param z A numeric vector representing the threshold time series.
#' @param d A numeric vector with one or more values for the search space
#'     of the delay parameter. Defaults to 1.
#' @param p0 A numeric vector with one or more values for the search space
#'     of the autoregressive order of Regime 0. Defaults to 1.
#' @param p1 Same as `p0`, but for regime 1.
#' @param p_select Which information criterion should be minimized to select
#'     the orders \eqn{p_0} and \eqn{p_1} Choices:
#'     * "aic" (Akaike Information Criterion)
#'     * "aicc" (Corrected Akaike Information Criterion)
#'     * "bic" (default, Bayesian Information Criterion)
#' @param r A vector or a matrix with search values for \eqn{\hat{r}_0, \hat{r}_1}.
#'     Defaults to `c(.1, .9)`.
#'     * If `r` is a vector, its length must be 2, such that it represents two quantiles
#'     within which the threshold value should be searched.
#'     * If `r` is a matrix, it must have two columns,
#'     such that each row represents a pair \eqn{r_0 \le r_1} to test.
#'     You can use a matrix with one row if you don't want to estimate
#'     the thresholds. Note that the values in these matrix are not quantiles,
#'     but directly values on the scale of `z`.
#' @param thin Logical, only relevant when `r` is a vector.
#'     If `TRUE` (default), the search values for \eqn{r} are
#'     `quantile(z, seq(r[1], r[2], by = .01))`. This drastically improves the speed
#'     without much cost of estimation precision. Note that this is a purely practical
#'     choice with no theoretical justification (yet).
#'     If `FALSE`, all observed unique values of `z` between
#'     `quantile(z, r[1])` and `quantile(z, r[2])` will be considered.
#'
#' @returns An object of S3 class `hystar_fit`, which is a `list` containing the following
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
#' With the `coef()` S3 method, the coefficients are represented in a matrix.
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
#' * `$equiv`, a matrix containing equivalent estimates for the delay and thresholds,
#' i.e., estimates that imply exactly the same regime indicator vector, and
#' as a result the same minimal residual sum of squares.
#'
#' Implemented generics for the `hystar_fit` class:
#'   * `plot()` plots the `z` variable and the `y` variable above one another.
#'   Shading of the background visualizes the regimes. Thresholds are drawn as
#'   horizontal lines in the `z` plot.
#' * `summary()`, this also provides the p-values and standard errors for the
#' estimates of the coefficients.
#' * `print()` prints the estimates within the mathematical representation of the model.
#' Note that the scalar multiplied with `e[t]` is the standard deviation
#' of the residuals, *not* the variance. See also the model definition above.
#' * `coef()`
#' * `confint()`
#' * `residuals()`
#' * `fitted()`
#' * `nobs()`
#'
#' @export
hystar_fit <- function(y, z, d = 1L, p0 = 1L, p1 = 1L, p_select = "bic",
                       r = c(.1, .9), thin = TRUE) {
  check_input <- check_hystar_fit_input(y, z, d, p0, p1, p_select, r, thin)
  p_select <- check_input
  eff <- time_eff(y, max(d), max(p0), max(p1))
  x <- create_x(y, eff, max(p0), max(p1))
  grid <- create_grid(z, r, d, eff, thin)
  p_options <- create_p_options(p0, p1)
  OPT <- optim_p(y, x, z, eff, grid, p_options, p_select)
  est <- OPT$est
  # We can discard the 4th column, "starts", because this will always have
  # the same value. A different start value would always result in a different
  # residual sum of squared residuals.
  equiv <- OPT$equiv[, 1:3]
  model <- run_model(y, x, z, eff, est["p0"], est["p1"],
                   est["d"], est["r0"], est["r1"], est["s"],
                   return_HR = TRUE)
  hystar <- new_hystar_fit(y, x, z, eff, est, model, equiv)

  return(hystar)
}
