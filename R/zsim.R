zsim <- function(r, length, n_switches, start_regime) {
  dist <- r[2] - r[1]
  max  <- r[2] + dist
  min  <- r[1] - dist

  # We start and end in the middle of the hysteresis zone
  z    <- numeric(length)
  z[1] <- mean(c(r[1], r[2]))
  z[length(z)] <- z[1]

  sw_pnts <- get_sw_pnts(length, n_switches)
  # We want the first switch to happen as soon as possible
  alternate <- if (start_regime == 0) c(max, min) else c(min, max)
  z[sw_pnts] <- rep_len(alternate, length.out = n_switches)

  x <- c(1, sw_pnts, sw_pnts, length(z))
  y <- z[x]
  mat <- matrix(c(x, y), ncol = 4, byrow = FALSE)

  values <- unlist(apply(X = mat, MARGIN = 1, FUN = cross))

  return(values)
}

get_sw_pnts <- function(length, n_switches) {
  sw_space <- length %/% (n_switches + 1)
  sw_pnts  <- sw_space * 1:n_switches
  max_noise <- sw_space %/% 4
  noise <- sample(-max_noise:max_noise, size = n_switches, replace = TRUE)

  return(sw_pnts + noise)
}


cross <- function(arg) {
  a <- arg[1]
  b <- arg[2]
  x <- arg[3]
  y <- arg[4]
  length <- b - a
  by <- (y - x) / (length - 1)
  all <- x + 0:(length - 1) * by

  # Delete the last one
  return(all[-length(all)])
}




