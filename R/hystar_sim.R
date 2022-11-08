#' Simulate data from the hysTAR model
#'
#' @description With this function, you can simulate observations from the
#'   hysTAR model, given its parameter values. See Section "The hysTAR model" below.
#'
#' # The hysTAR model
#' The hysTAR model is defined as:
#'
#'   \eqn{ y_t = \begin{cases} \phi_0^{(0)} + \phi_1^{(0)} y_{t-1} + \cdots +
#'   \phi_{p_0}^{(0)} y_{t-p_0} + \sigma_{(0)} \varepsilon_t & \text{if}~R_{t} = 0
#'   \\ \phi_0^{(1)} + \phi_1^{(1)} y_{t-1} + \cdots + \phi_{p_1}^{(1)} y_{t-p_1}
#'   + \sigma_{(1)} \varepsilon_t & \text{if}~R_{t} = 1, \\ \end{cases} }
#'
#'   with \eqn{ R_t = \begin{cases} 0 & \mathrm{if} \, z_{t-d} \in (-\infty,
#'   r_{0}] \\ R_{t-1} & \mathrm{if} \, z_{t-d} \in (r_0, r_1] \\ 1 & \mathrm{if}
#'   \, z_{t-d} \in (r_1, \infty), \\ \end{cases} }
#'
#'   where \eqn{p_i} denotes the order of regime \eqn{i \in \{0,1\}} with
#'   coefficients \eqn{\phi_0^{(i)}, \dots, \phi_{p_i}^{(i)} \in (-1, 1)},
#'   \eqn{\sigma_{(i)}} is the standard deviation of the residuals, and \eqn{d \in
#'   \{1, 2, \dots\}} is a delay parameter. The parameters of primary interest are
#'   the thresholds \eqn{r_0 \le r_1}.
#'
#' @author Daan de Jong.
#'
#' @references Li, Guodong, Bo Guan, Wai Keung Li, en Philip L. H. Yu.
#' ‘Hysteretic Autoregressive Time Series Models’. Biometrika 102, nr. 3
#' (september 2015): 717–23.
#'
#' @details Simulation of z. Why initial values are needed. [hystar_fit()]
#'
#' @param z A numeric vector representing the observed threshold variable.
#'   You can simulate `z` with [z_sim()].
#' @param r A numeric vector of length 2, representing the threshold values
#'   \eqn{r_0} and \eqn{r_1}.
#' @param d A number in \eqn{\{1, 2, \dots\}} representing the value of the
#'   delay parameter.
#' @param phi_R0 A vector containing the constant and autoregressive parameters
#'   \eqn{(\phi_0^{(0)}, \phi_1^{(0)}, \dots, \phi_{p_0}^{(0)})} of Regime 0.
#'   Note that the first value of this vector is *always* interpreted as the
#'   constant. The coefficients must imply a s tationary process. That is the
#'   case when the (complex) roots of \eqn{1 - \phi_1 - \cdots - \phi_{p_0}} lie
#'   on or outside the unit circle, or equivalently when \eqn{1 \ge \sum_{i =
#'   1}^{p_0} \phi_i}.
#' @param phi_R1 The same as `phi_R0`, but for Regime 1.
#' @param resvar A numeric vector of length 2 representing the variances of the
#'   residuals \eqn{\sigma_{(0)}^2} and \eqn{\sigma_{(1)}^2}. The residuals are
#'   sampled from a normal distribution in the current implementation, but note
#'   that the model is defined for any i.i.d. distribution with zero mean and
#'   finite variance.
#' @param start_regime A scalar 0 or 1 indicating which regime should be first
#'   when `z` starts in the hysteresis zone \eqn{(r_0, r_1]}?
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
#'   * `plot()`
#'   * `summary()`
#'   * `print()`
#'
#' @export
#'
#' @examples
#' z <- z_sim(n_t = 200, n_switches = 5, start_regime = 1)
#' sim <- hystar_sim(z = z, r = c(-.5, .5), d = 2, phi_R0 = c(0, .6), phi_R1 = 1,
#' resvar = c(1, 1))
#' plot(sim)
#' fit <- hystar_fit(y = sim$data$y, z = z)
#' summary(fit)
hystar_sim <- function(z, r, d, phi_R0, phi_R1, resvar = c(1, 1), start_regime = NULL) {

  start_regime <- check_hystar_sim_input(z, r, d, phi_R0, phi_R1, resvar, start_regime)
  if (is.matrix(r)) r <- r[1, , drop = TRUE]

  p0 <- get_order(phi_R0)
  p1 <- get_order(phi_R1)
  k <- n_ineff(p0, p1, d)

  z_del <- z[time_del(y = z, d = d, p0, p1, d_sel = d)]
  H <- ts_hys(z_del, r0 = r[1], r1 = r[2])
  R <- c(rep(start_regime, times = k), ts_reg(H, start = start_regime))
  y <- y_sim(R, phi_R0, phi_R1, resvar)

  phi <- c(phi_R0, phi_R1)
  names(phi) <- c(paste0("phi_R0_", 0:p0), paste0("phi_R1_", 0:p1))
  data <- data.frame(y = y, z = z, H = c(rep(NA, k), H == -1L), R = R)
  out <- structure(
    list(data = data,
         r = r,
         d = d,
         phi = phi,
         orders = c(get_order(phi_R0), get_order(phi_R1)),
         resvar = resvar),
    class = "hystar_sim"
  )

  return(out)
}



