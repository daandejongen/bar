#' Estimate the HysTAR model using conditional least squares estimation
#'
#' @description
#' This function allows you to estimate the parameters of the hysteretic threshold
#' autoregressive (HysTAR) model.
#'
#' @inherit hystar_sim author
#' @inheritSection hystar_sim The HysTAR model
#' @inherit hystar_sim references
#' @inherit hystar_sim examples
#'
#' @details In regime 0, \eqn{y_{t}} is predicted by values up to \eqn{y_{t - p_0}}.
#' This implies that the first \eqn{p_0} time points can not be predicted. E.g.,
#' if \eqn{p_0 = 2}, \eqn{y_1} would miss a value from \eqn{y_{-1}}. Similarly, the
#' value of the delay parameter implies that the regime is unknown for the first
#' \eqn{d} time points. To ensure that the same data are used on all options for
#' `d`, `p0` and `p1`, the first `max(d, p0, p1)` observations are discarded for
#' estimation of the parameters.
#'
#' @param data a vector, matrix or data.frame containing the outcome variable
#' \eqn{y} in the first column and the threshold variable \eqn{z} in the second.
#' Other columns are ignored.
#' A vector, is taken to be both the outcome and control variable,
#' so, in that case, a self-exciting HysTAR is fitted.
#' @param r A vector or a matrix with search values for \eqn{\hat{r}_0, \hat{r}_1}.
#'     Defaults to `c(.1, .9)`.
#' * A vector `r` must contain two values \eqn{a} and \eqn{b} in \eqn{[0, 1]}.
#'     The search space for the thresholds will be observed values of `z`
#'     between its \eqn{100a\%} and \eqn{100b\%} percentiles.
#' * A matrix `r` allows for a custom search. It must have two columns,
#'     such that each row represents a pair \eqn{r_0 \le r_1} to test.
#'     You can use a matrix with one row if you don't want to estimate
#'     the thresholds. Note that the values in these matrix should be
#'     on the scale of `z`.
#' @param d A numeric vector with one or more values for the search space
#'     of the delay parameter. Defaults to 1. Typically, d is not very large, so
#'     a reasonable search space might be 0, 1, 2, ..., 5.
#' @param p0 A numeric vector with one or more values for the search space
#'     of the autoregressive order of Regime 0. Defaults to 1.
#' @param p1 Same as `p0`, but for regime 1. Note that it does not need to be
#'     equal to `p0`.
#' @param p_select The information criterion that should be minimized to select
#'     the orders \eqn{p_0} and \eqn{p_1}. Choices:
#' * `"aic"` (Akaike Information Criterion)
#' * `"aicc"` (Corrected Akaike Information Criterion)
#' * `"bic"` (default, Bayesian Information Criterion)
#' @param thin `TRUE` (default) or `FALSE`. Only relevant when `r` is a vector.
#' * If `TRUE` (default), the search space for the thresholds are the
#'     \eqn{100a\%, 100(a+0.01)\%, \dots, 100b\%} percentiles of `z`.
#'     This drastically reduces computation costs while keeping a reasonably large
#'     search space for the thresholds. Note that this is a purely practical
#'     choice with no theoretical justification.
#' * If `FALSE`, all observed unique values of `z` between
#'     the \eqn{100a\%} and \eqn{100b\%} percentiles of `z` will be considered.
#' @param tar `TRUE` or `FALSE` (default). Choose `TRUE` if you want to fit a traditional
#'     2-regime threshold autoregressive (TAR) model. In this model,
#'     there is only one threshold (or equivalently, a HysTAR model with \eqn{r_0 = r_1}).
#'
#' @returns An object of S3 class `hystar_fit`, which is a `list` containing the following
#' items:
#' * `$data`. A `data.frame` containing
#'     * `y`, the outcome variable
#'     * `z`, the threshold variable
#'     * `H`, a logical vector that indicates at which time points the hysteresis
#' effect is happening. Note that this vector starts with `NA`(s), since not all
#' values can be predicted in the HysTAR model. See Details.
#'     * `R`, the regime indicator vector. (Also starts with `NA`(s).)
#'
#' * `$residuals`. Also accessible with the `residuals()` S3 method.
#' * `$coefficients`, a vector with the estimated coefficients.
#' With the `coef()` S3 method, the coefficients are represented in a matrix.
#' Use the `confint()` method to get the confidence intervals of the estimates.
#' * `$delay`, a scalar with the estimate for the delay parameter.
#' * `$thresholds`, a vector with the estimates of the thresholds.
#' * `$orders`, a vector with the estimates of the orders.
#' * `$resvar`, a vector with the estimates of the residual variances.
#' * `$rss`, the minimized residual sum of squares.
#' * `$ic`, a vector with the aic, the corrected aic and the bic.
#' * `$n`, a vector with the total effective observations and the
#' effective obeservations in regime 0 and regime 1.
#' * `$eff`, a vector with the time indicators of the effective observations.
#' * `$equiv`, a matrix containing equivalent estimates for the delay and thresholds,
#' i.e., estimates that imply exactly the same regime indicator vector, and
#' as a result the same minimal residual sum of squares.
#' * `$r_search`, a vector with the \eqn{r}-values that were considered.
#' * `$tar`, Logical: `TRUE` if a TAR model was fitted.
#'
#' Implemented generics for the `hystar_fit` class:
#'   * `plot()` plots the `z` variable and the `y` variable above one another.
#'   Shading of the background visualizes the regimes. Thresholds are drawn as
#'   horizontal lines in the `z` plot. You can provide regime_names (char vector of 2),
#'   main (char vector of 1), xlab (char vector of 1) and ylab (char vector of 2).
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
hystar_fit <- function(data, r = c(.1, .9), d = 0L, p0 = 1L, p1 = 1L, p_select = "bic",
                       thin = FALSE, tar = FALSE) {
  check_data(data)
  if (is.vector(data)) {
    y <- z <- data
  } else {
    y <- data[, 1]
    z <- data[, 2]
  }
  p_select <- check_hystar_fit_input(z, d, p0, p1, p_select, r, thin, tar)
  eff <- time_eff(y, max(d), max(p0), max(p1))
  x <- create_x(y, eff, max(p0), max(p1))
  grid <- create_grid(z, r, d, eff, thin, tar)
  r_search <- unique(as.vector(grid[, c("r0", "r1")]))
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
  hystar <- new_hystar_fit(y, x, z, eff, est, model, equiv, tar, r_search)

  return(hystar)
}
