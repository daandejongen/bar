#' Simulate the threshold/control variable Z
#'
#' @description blabla
#'
#' @details The number of switches `n_switches` can differ ultimately based on r and d.
#' and if starting regime is weird. (only for sine function)
#'
#' @param start_regime a
#' @param n_switches a
#' @param n_t a
#' @param form a
#' @param scale a
#'
#' @return a
#' @export
#'
#' @examples 1
z_sim <- function(start_regime, n_switches, n_t,
                  form = "cosine", scale = c(-1, 1)) {

  form <- check_z_sim_input(start_regime, n_switches, n_t, form, scale)

  z <- simulate_cossin(start_regime, n_switches, n_t, form)

  # Amplitude of the sin or cos function
  # z is first magnified by its amplitude, then shifted
  amp <- (abs(scale[2] - scale[1])) / 2
  z <- z * amp + scale[1] + amp

  return(z)
}

simulate_cossin <- function(start_regime, n_switches, n_t, form) {
  time <- 0:(n_t - 1)
  valence <- if (start_regime == 1) 1 else -1
  f <- if (form == "cosine") function(x) cos(x) else function (x) -sin(x)
  out <- valence * f(time * n_switches * pi / (n_t - 1))

  return(round(out, 3))
}

y_sim <- function(R, phi_R0, phi_R1, resvar) {
  n <- length(R)
  start_regime <- R[1]
  n_burn_in <- 50
  p0 <- get_order(phi_R0)
  p1 <- get_order(phi_R1)

  coe_burn_in <- if (start_regime == 0) phi_R0 else phi_R1
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
  p <- length(coe) - 1
  coe_0 <- if (p == 0) 0 else coe[2:length(coe)]
  mean  <- coe[1] / (1 - sum(coe_0))
  var   <- resvar / (1 - coe_0 %*% coe_0)
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

# Helpers:
get_init_vals <- function(coe, resvar, len) {

  return(rnorm(n = len, mean = mean, sd = sqrt(var)))
}


