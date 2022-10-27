y_sim <- function(y, eff, R, phi_R0, phi_R1, resvar) {
  p0 <- get_order(phi_R0)
  p1 <- get_order(phi_R1)

  for (t in eff) {

    if (R[t] == 0L) {
      x <- lag_obs(y, t, p0)
      y[t] <- AR(x, phi_R0, resvar[1])
    }

    if (R[t] == 1L) {
      x <- lag_obs(y, t, p1)
      y[t] <- AR(x, phi_R1, resvar[2])
    }

  }

  return(y)
}

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

#' @importFrom stats rnorm
AR <- function(x, coe, resvar) {
  # One time step AR simulation.
  pred <- x %*% coe
  resi <- rnorm(1, 0, sqrt(resvar))

  return(pred + resi)
}

# Helpers:

get_init_vals <- function(coe, resvar, len) {
  # Random observations based on the AR model of the starting regime
  coe_0 <- if (length(coe) == 1) 0 else coe[2:length(coe)]
  mean  <- coe[1] / (1 - sum(coe_0))
  var   <- resvar / (1 - coe_0 %*% coe_0)
  return(rnorm(n = len, mean = mean, sd = sqrt(var)))
}


