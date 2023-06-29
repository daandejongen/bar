#' Simulate the threshold/control variable Z
#'
#' @description This is a function you can use to simulate time series data
#' for a threshold variable of the HysTAR model. The time series is a (co)sine
#' wave, such that thresholds are crossed in a predictable way.
#' This function is designed to be used in combination with `hystar_sim()`.
#'
#' @details The first value of `y` that can be predicted in the HysTAR model is
#' at time point \eqn{\max\{d, p\} + 1}, where \eqn{p = \max\{p_0, p_1\}}.
#' This is because we need to observe \eqn{y_{t - p}} and \eqn{z_{t - d}}.
#' So the first observed value of `z` that determines a regime
#' is at time point \eqn{\max\{d, p\} + 1 - d}.
#' To make sure that this time point corresponds to the start that you request,
#' `z_sim()` starts with 10 extra time points. In this way, [`hystar_sim`] can
#' select the appropriate time points, based on `d` and `p0`, `p1`.
#'
#' @inheritSection hystar_sim The HysTAR model
#'
#' @param n_t The desired length of the simulated time series of `z`. The actual
#' vector that is returned will contain 10 more time points, see Details.
#' Note that `n_t` will also be the length of `y`, when you feed `z` to [`hystar_sim`].
#' @param n_switches A scalar indicating the desired number of regime switches.
#' Basically, it is the number of times the variable moves to (and reaches) its
#' minimum or to its maximum. If the thresholds are within the range of `z`,
#' as they should, this will guarantee the same number of regime switches when
#' the delay parameter of the HysTAR model is greater or equal than the highest order.
#' See Details.
#' @param start_regime The starting regime of the HysTAR model, 0 (default) or 1.
#' @param start_hyst Logical, should `z` start in the hysteresis zone? Of course, this also
#' depends on `r`, and `r` is not yet specified in this function. Rather, setting `start_hyst`
#' to `TRUE` makes `z` start at in the middle of its range, which makes it easy to
#' set the threshold values "around" the first values of `z`.
#' @param range A numeric vector of length 2 indicating the desired range (min, max) of `z`.
#'
#' @returns A numeric vector of length `n_t`. This vector has two attributes
#' `"start_regime"` and `"start_hyst"` corresponding to the values you provided.
#' These attributes are used by [hystar_sim()].
#'
#' @export
#'
#' @inherit hystar_sim examples
z_sim <- function(n_t, n_switches,
                  start_regime = 0, start_hyst = FALSE,
                  range = c(-1, 1)) {

  check_z_sim_input(n_t, n_switches, start_regime, start_hyst, range)

  z <- simulate_cossin(n_t, n_switches, start_regime, start_hyst)

  # Amplitude of the sin or cos function
  # z is first magnified by its amplitude, then shifted
  amp <- (abs(range[2] - range[1])) / 2
  z <- z * amp + range[1] + amp

  attr(z, "start_regime") <- start_regime
  attr(z, "start_hyst") <- start_hyst

  return(z)
}

simulate_cossin <- function(n_t, n_switches, start_regime, start_hyst) {
  time <- -10:(n_t - 1L)
  valence <- if (start_regime == 1) 1L else -1L
  f <- if (start_hyst) function(x) -sin(x) else function (x) cos(x)
  out <- valence * f(time * n_switches * pi / n_t)

  return(round(out, 3))
}

y_sim <- function(R, phi_R0, phi_R1, resvar) {
  n <- length(R)
  start_regime <- R[1]
  n_burn_in <- 50L
  p0 <- get_order(phi_R0)
  p1 <- get_order(phi_R1)

  coe_burn_in <- if (start_regime == 0L) phi_R0 else phi_R1
  burn_in <- create_burn_in(n_burn_in, coe_burn_in, resvar[start_regime + 1])

  y <- c(burn_in, numeric(n))

  for (i in seq_len(n)) {
    t <- n_burn_in + i

    if (R[i] == 0L) {
      x <- lag_obs(y, t, p0)
      y[t] <- AR(x, phi_R0, resvar[1])
    }

    if (R[i] == 1L) {
      x <- lag_obs(y, t, p1)
      y[t] <- AR(x, phi_R1, resvar[2])
    }

  }

  return(y[-(1:n_burn_in)])
}

#' @importFrom stats rnorm
create_burn_in <- function(n, coe, resvar) {
  # First p0 or p1 obs from unconditional distribution of starting regime
  p <- length(coe) - 1L
  coe_0 <- if (p == 0) 0L else coe[2:length(coe)]
  mean  <- coe[1] / (1L - sum(coe_0))
  var   <- resvar / (1L - coe_0 %*% coe_0)
  init <- rnorm(n = p, mean = mean, sd = sqrt(var))

  out <- c(init, numeric(n))
  for (i in seq_len(n)) {
    t <- p + i
    out[t] <- AR(x = lag_obs(out, t, p), coe, resvar)
  }

  return(out[-(1:p)])
}

#' @importFrom stats rnorm
AR <- function(x, coe, resvar) {
  # One time step AR simulation.
  pred <- x %*% coe
  resi <- rnorm(1, 0, sqrt(resvar))

  return(pred + resi)
}
