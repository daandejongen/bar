ts_hys <- function(z, r0, r1) {
  # -1 indicates the hysteresis zone, 0 and 1 the
  # lower and upper regime, respectively
  H  <- rep(-1L, times=length(z))
  H  <- H + (z <= r0) + (z >= r1) * 2L
  return(H)
}

ts_reg <- function(H, init = -9) {
  # init, initial value, is needed when starting regime is unknown, but
  # when known, the value of init cannot alter the correct R in any way.
  if (H[1] == -1 && init == -9) {
    stop("initial value should be 0 or 1")
  }

  R <- c(init, H)
  for (i in 2:(length(H)+1)) if (R[i] == -1L) R[i] <- R[i-1]
  R <- R[-1]

  return(R)
}
