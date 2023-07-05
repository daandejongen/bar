#' Simulate data from the HysTAR model
#'
#' @description With this function, you can simulate observations from the
#'   HysTAR model, given its parameter values.
#'
#' # The HysTAR model
#' The HysTAR model is defined as:
#'
#'   \eqn{ y_t = \begin{cases} \phi_{00} + \phi_{01} y_{t-1} + \cdots +
#'   \phi_{0 p_0} y_{t-p_0} + \sigma_{0} \epsilon_{t} \quad \mathrm{if}~R_{t} = 0 \\
#'   \phi_{10} + \phi_{11} y_{t-1} + \cdots + \phi_{1 p_1} y_{t-p_1} + \sigma_{1} \epsilon_{t}
#'   \quad \mathrm{if}~R_{t} = 1, \\ \end{cases} }
#'
#'   with \eqn{ R_t = \begin{cases} 0 \quad \quad \mathrm{if} \, z_{t-d} \in (-\infty, r_{0}] \\
#'   R_{t-1} \quad \mathrm{if} \, z_{t-d} \in (r_0, r_1] \\ 1 \quad \quad \mathrm{if} \, z_{t-d}
#'   \in (r_1, \infty), \\ \end{cases} }
#'
#'   where \eqn{p_j} denotes the order of regime \eqn{j \in \{0,1\}} with
#'   coefficients \eqn{\phi_{j0}, \dots, \phi_{j p_j \in (-1, 1)}},
#'   \eqn{\sigma_{j}} is the standard deviation of the residuals, and \eqn{d \in
#'   \{0, 1, 2, \dots\}} is a delay parameter. The parameters of primary interest are
#'   the thresholds \eqn{r_0 \le r_1}. We let \eqn{t = 0, 1, 2, ..., T}, where \eqn{T}
#'   is the number of observations.
#'
#' @author Daan de Jong.
#'
#' @references Li, Guodong, Bo Guan, Wai Keung Li, en Philip L. H. Yu.
#' ‘Hysteretic Autoregressive Time Series Models’. Biometrika 102, nr. 3
#' (september 2015): 717–23.
#'
#' Zhu, Ke, Philip L H Yu, en Wai Keung Li. ‘Testing for the Buffered
#' Autoregressive Process’. Munich Personal RePEc Archive, (november 2013).
#'
#' @details Some details:
#'   * To simulate `y`, 50 burn-in samples according the starting regime are used.
#'   * The coefficients imply a stationary process of \eqn{y_t} if
#'     \eqn{\sum_{i=1}^{p_0} \phi_i^{(0)} < 1} and
#'     \eqn{\sum_{i=1}^{p_1} \phi_i^{(1)} < 1}. See Zhu, Yu and Li (2013), p5.
#'
#' @param z A numeric vector representing the observed threshold variable.
#'   You can simulate `z` with [z_sim()]. Can not have missing values.
#' @param r A numeric vector of length 2, representing the threshold values
#'   \eqn{r_0} and \eqn{r_1}. The values must be inside the range of z, that is,
#'   larger than `min(z)` and smaller than `max(z)`. Otherwise, only one regime will
#'   be active so you might as well simulate an AR process, e.g. with [arima.sim()].
#'   If you simulated `z` with `z_sim()` and `start_hyst = TRUE`, make sure to
#'   set the threshold values around the middle of the range of `z`, otherwise,
#'   the start will not be hysteretic.
#' @param d A positive whole number representing the value of the
#'   delay parameter. It must be smaller than `length(z)`.
#' @param phi_R0 A vector containing the constant and autoregressive parameters
#'   \eqn{(\phi_0^{(0)}, \phi_1^{(0)}, \dots, \phi_{p_0}^{(0)})} of Regime 0.
#'   Note that the first value of this vector is *always* interpreted as the
#'   constant, so for an AR(1) process with no constant, you must use
#'   `phi_R0 = c(0, .5)`, for example. Both orders must be smaller than `length(z)`.
#'   For valid standard errors of the estimates in [hystar_fit()], the coefficients
#'   should imply that `y` is stationary, see Details.
#' @param phi_R1 The same as `phi_R0`, but for Regime 1.
#' @param resvar A numeric vector of length 2 representing the variances of the
#'   residuals \eqn{\sigma_{(0)}^2} and \eqn{\sigma_{(1)}^2}. The residuals are
#'   sampled from a normal distribution in the current implementation, but note
#'   that the model is defined for any i.i.d. vector of residuals with zero mean and
#'   finite variance.
#' @param start_regime Optionally, a 0 or 1 that indicates which regime should be the
#'   first, in case the `z` variable starts in the hysteresis zone.
#'   This is only necessary when you use your 'own' `z` variable
#'   AND `z` starts in the hysteresis zone.
#'   A vector `z` simulated with [z_sim()] will contain information about if
#'   the start is hysteretic and what the starting regime is supposed to be
#'   (in the `attributes()` of `z`).
#'
#' @returns A list of class `hystar_sim` with elements
#'
#'   * `$data`, a `data.frame` with `length(z)` rows and 4 columns:
#'        * `y`, the outcome variable
#'        * `z`, the threshold variable
#'        * `H`, a logical vector that indicates at which time
#'        points the hysteresis effect is happening. Note that this vector starts
#'        with `NA`(s), since the first \eqn{d} time points have no values observed
#'        for \eqn{z_{t-d}}.
#'        * `R`, the regime indicator vector.
#'   * `$r`, a numeric vector with the two threshold values,
#'   * `$d`, the delay parameter,
#'   * `$phi`, a numeric vector containing the coefficients. The names are such
#'   that `phi_R1_2` represents \eqn{\phi_{2}^{(1)}}, the second lag
#'   autoregressive coefficient in Regime 1,
#'   * `$orders`, a numeric vector containing the two orders, and
#'   * `$resvar`, a numeric vector with the residual variances of both regimes.
#'
#'   Implemented generics for the `hystar_sim` class:
#'   * `plot()` plots the `z` variable and the `y` variable above one another.
#'   Shading of the background visualizes the regimes. Thresholds are drawn as
#'   horizontal lines in the `z` plot. You can provide regime_names (char vector of 2),
#'   main (char vector of 1), xlab (char vector of 1) and ylab (char vector of 2).
#'   * `summary()` gives an overview of the true parameter values that were used.
#'   * `print()` prints the parameter values within the mathematical representation
#'   of the model. Note that the scalar multiplied with `e[t]` is the standard deviation
#'   of the residuals, *not* the variance. See also the model definition above.
#'
#' @export
#'
#' @examples
#' z <- z_sim(n_t = 200, n_switches = 5, start_regime = 1)
#' sim <- hystar_sim(z = z, r = c(-.5, .5), d = 2, phi_R0 = c(0, .6), phi_R1 = 1)
#' plot(sim)
#' fit <- hystar_fit(sim$data)
#' summary(fit)
hystar_sim <- function(z, r, d, phi_R0, phi_R1, resvar = c(1, 1), start_regime = NULL) {

  temp <- check_hystar_sim_input(z, r, d, phi_R0, phi_R1, resvar, start_regime)
  z <- temp$z
  start_regime <- temp$start

  phi_R0 <- remove_trailing_zeros(phi_R0)
  phi_R1 <- remove_trailing_zeros(phi_R1)
  p0 <- get_order(phi_R0)
  p1 <- get_order(phi_R1)
  k <- max(p0, p1, d)

  z_del <- z[(k + 1 - d):(length(z) - d)]
  H <- ts_hys(z_del, r0 = r[1], r1 = r[2])
  R <- c(rep(start_regime, times = k), ts_reg(H, start = start_regime))
  y <- y_sim(R, phi_R0, phi_R1, resvar)

  out <- new_hystar_sim(y, z, H, R, phi_R0, phi_R1, r, d, resvar, k)

  return(out)
}



