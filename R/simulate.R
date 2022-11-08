#' Simulate the threshold/control variable Z
#'
#' @description This is a function you can use to simulate time series data
#' for a threshold variable of the HysTAR model. You could use this when you want
#' to simulate data from the HysTAR model, but have not observed the threshold variable.
#'
#' @details The number of switches `n_switches` can differ ultimately based on r and d.
#' and if starting regime is weird (only for sine function).
#' Values of `z` will have three digits.
#'
#' @param n_t The length of the simulated time series of `z`. Note that this will also
#' be the length of `y`, when you feed `z` to [`hystar_sim`]
#' @param n_switches A scalar indicating the desired number of regime switches.
#' It depends on the thresholds and the delay of the HysTAR model if this number
#' is achieved, see Details.
#' @param start_regime The starting regime of the HysTAR model, 0 or 1. You must specify the
#' same starting regime in [`hystar_sim`]. Otherwise, `n_switches` may not be realized.
#' @param start_hyst Logical, should `z` start in the hysteresis zone? Of course, this also
#' depends on `r`, and `r` is not yet specified in this function. Rather, setting `start_hyst`
#' to `TRUE` makes `z` start at in the middle of its range, which makes it easy to
#' set the threshold values "around" the first values of `z`.
#' @param range A numeric vector of length 2 indicating the desired range (min, max) of `z`.
#'
#' @return A numeric vector of length `n_t`.
#' @export
#'
#' @examples
#' z <- z_sim(n_t = 200, n_switches = 5, start_regime = 1)
#' y <- hystar_sim(z = z, r = c(-.5, .5), d = 2, phi_R0 = c(0, .6), phi_R1 = 1,
#' resvar = c(1, 1))
z_sim <- function(n_t, n_switches,
                  start_regime, start_hyst = FALSE,
                  range = c(-1, 1)) {

  check_z_sim_input(n_t, n_switches, start_regime, start_hyst, range)

  z <- simulate_cossin(n_t, n_switches, start_regime, start_hyst)

  # Amplitude of the sin or cos function
  # z is first magnified by its amplitude, then shifted
  amp <- (abs(range[2] - range[1])) / 2
  z <- z * amp + range[1] + amp

  return(z)
}

simulate_cossin <- function(n_t, n_switches, start_regime, start_hyst) {
  time <- 0:(n_t - 1L)
  valence <- if (start_regime == 1) 1L else -1L
  f <- if (start_hyst) function(x) -sin(x) else function (x) cos(x)
  out <- valence * f(time * n_switches * pi / (n_t - 1))

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
