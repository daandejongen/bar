simulate_y <- function(y, eff, R, phi, psi, resvar) {
  p0 <- get_order(phi)
  p1 <- get_order(psi)

  for (t in eff) {

    if (R[t] == 0L) {
      x <- lag_obs(y, t, p0)
      y[t] <- AR(x, phi, resvar[1])
    }

    if (R[t] == 1L) {
      x <- lag_obs(y, t, p1)
      y[t] <- AR(x, psi, resvar[2])
    }

  }

  return(y)
}


simulate_z <- function(r, n_t, n_switches, start_regime) {
  # The idea is to create a z-variable that moves around the
  # hysteresis zone (r0, r1]. We pick the length of the hysteresis
  # zone as the distance that z may go above (below) r1 (r0).
  dist <- r[2] - r[1]
  max  <- r[2] + dist
  min  <- r[1] - dist

  # We start and end in the middle of the hysteresis zone.
  z <- numeric(n_t)
  z[1] <- mean(c(r[1], r[2]))
  z[n_t] <- z[1]

  # At the returning points, z goes from increasing back to
  # decreasing or the other way around. At these points,
  # 'z' takes on the maximum or the minimum.
  # We want the first switch to happen as soon as possible,
  # so if start is 0, we have to go to a return point at the max
  # (since then the switch happens at crossing r1).
  ret_pnts <- get_ret_pnts(n_t, n_switches)
  alternate <- if (start_regime == 0) c(max, min) else c(min, max)
  z[ret_pnts] <- rep_len(alternate, length.out = n_switches)

  # Here I create a matrix with two columns containing the
  # from and to time points and two columns containing the
  # from and to z values. To this matrix the 'cross' function
  # is applied to get the connections between the froms and tos.
  x <- c(1, ret_pnts, ret_pnts, n_t)
  y <- z[x]
  mat <- matrix(c(x, y), ncol = 4, byrow = FALSE)
  values <- unlist(apply(X = mat, MARGIN = 1, FUN = cross))
  # In the function 'cross', the value that is "crossed to", is not
  # returned (to avoid duplicates, since this is the start value for
  # the next segment). So we need to add the last value of z.
  return(c(values, z[n_t]))
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
  coe_0 <- coe[2:length(coe)]
  mean  <- coe[1] / (1 - sum(coe_0))
  var   <- resvar / (1 - coe_0 %*% coe_0)
  return(rnorm(n = len, mean = mean, sd = sqrt(var)))
}


get_ret_pnts <- function(length, n_switches) {
  # Returns the time indices where 'z' should turn.
  space <- length %/% (n_switches + 1)
  pnts  <- space * 1:n_switches
  max_noise <- space %/% (length %/% 50)
  noise <- sample(-max_noise:max_noise, size = n_switches, replace = TRUE)

  return(pnts + noise)
}


cross <- function(arg) {
  # Function to connect two points in time X value space.
  # Returns the values that intermediate time points should take
  # on to create a straight line.
  a <- arg[1] # from time point
  b <- arg[2] # to time point
  x <- arg[3] # from value
  y <- arg[4] # to value
  step <- (y - x) / (b - a) # value step size
  # b - a - 1 because we don't want to have the last value, y,
  # because this value will be used as the first value, x, in
  # the next 'cross'.
  values <- x + 0:(b - a - 1) * step

  # Delete the last one
  return(values)
}



