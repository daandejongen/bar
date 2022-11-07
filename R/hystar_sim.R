#' Simulating data from the hysTAR model
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
#' @details Simulation of z. Why initial values are needed.
#'
#' @param z A numeric vector representing the observed time series of the threshold
#' variable. The length of `z` determines the length of the outcome time series \eqn{y_t}.
#' When not provided, a \eqn{z_t} time series will be simulated, see Details.
#' @param r A numeric vector of length 2, representing the threshold values \eqn{r_0} and \eqn{r_1}.
#' @param d A number in \eqn{\{1, 2, \dots\}} representing the value of the delay parameter.
#' @param phi_R0 A vector containing the constant and autoregressive parameters
#' \eqn{(\phi_0^{(0)}, \phi_1^{(0)}, \dots, \phi_{p_0}^{(0)})} of Regime 0.
#' Note that the first value of this vector is \textit{always} interpreted as the constant.
#' The coefficients must imply a stationary process. That is the case when the roots
#' of \eqn{1 - \phi_1 - \cdots - \phi_{p_0}} lie outside the unit circle, or equivalently
#' when \eqn{1 > \sum_{i = 1}^{p_0} \phi_i}.
#' @param phi_R1 The same as `phi_R0`, but for Regime 1.
#' @param resvar A numeric vector of length 2 representing the variances of the
#' residuals \eqn{\sigma_{(0)}^2} and \eqn{\sigma_{(1)}^2}. The residuals are sampled
#' from a normal distribution in the current implementation, but note that the model
#' is defined for any i.i.d. distribution with zero mean and finite variance.
#' @param start_regime A scalar 0 or 1 indicating which regime should be first when
#' `z` starts in the hysteresis zone \eqn{(r_0, r_1]}?
#'
#' @return A list with elements
#'
#' * `$data`. A `data.frame` of class `hystar_data` (with a `plot()` method), containing
#'     * `y`, the outcome variable
#'     * `z`, the threshold variable
#'     * `H`, a logical vector that indicates at which time points the hysteresis
#' effect is happening. Note that this vector starts with `NA`(s), since the first
#' \eqn{d} time points have no values observed for `z`.
#'     * `R`, the regime indicator vector.
#'
#' * `$true_values`, a list containing the parameter values that were used to
#' generate the data.
#'
#' @export
#'
#' @examples
#' z <- z_sim(n_t = 200, n_switches = 5, start_regime = 1)
#' sim <- hystar_sim(z = z, r = c(-.5, .5), d = 2, phi_0 = c(0, .6), phi_1 = 1,
#' resvar = c(1, 1))
#' plot(sim$data)
hystar_sim <- function(z, r, d, phi_R0, phi_R1, resvar = c(1, 1), start_regime = NULL) {

  start_regime <- check_hystar_sim_input(z, r, d, phi_R0, phi_R1, resvar, start_regime)

  if (is.matrix(r)) r <- r[1, , drop = TRUE]

  p0 <- get_order(phi_R0)
  p1 <- get_order(phi_R1)
  k  <- n_ineff(p0, p1, d)

  z_del <- z[time_del(y = z, d = d, p0, p1, d_sel = d)]
  H     <- ts_hys(z_del, r0 = r[1], r1 = r[2])
  R     <- c(rep(start_regime, times = k), ts_reg(H, start = start_regime))

  y     <- y_sim(R, phi_R0, phi_R1, resvar)

  true  <- name_true_vals(d, r, phi_R0, phi_R1, resvar)

  data  <- data.frame(y = y, z = z, H = c(rep(NA, k), H == -1L), R = R)

  out <- structure(
    list(data = data, true_values = true),
    class = "hystar_sim"
  )

  return(out)
}

name_true_vals <- function(d, r, phi_R0, phi_R1, resvar) {
  names(d)   <- "delay"
  names(r)   <- c("r0", "r1")
  phi <- c(phi_R0, phi_R1)
  names(phi) <- c(paste0("phi_R0_", 0:(length(phi_R0)-1)),
                  paste0("phi_R1_", 0:(length(phi_R1)-1)))
  names(resvar) <- paste0("regime", 0:1)

  return(list(d = d,
              r = r,
              phi = phi,
              orders = c(get_order(phi_R0), get_order(phi_R1)),
              resvar = resvar
              )
         )
}



