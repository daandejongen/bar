ts_hys <- function(z, r0, r1) {
  # -1 indicates the hysteresis zone, 0 and 1 the
  # lower and upper regime, respectively
  H  <- rep(-1L, length(z))
  H  <- H + (z <= r0) + (z >= r1) * 2L

  return(H)
}

ts_reg <- function(H, start = NULL) {
  # start is needed when starting regime is unknown, but
  # when known, the value of start cannot alter the correct R in any way,
  # because it is deleted afterwards.
  if (is.null(start)) start <- 9L

  R <- c(start, H)
  for (i in 2:(length(H)+1)) if (R[i] == -1L) R[i] <- R[i-1]
  R <- R[-1]

  return(R)
}

# One time step AR simulation
AR <- function(x, coe, resvar) {
  pred <- x %*% coe
  resi <- rnorm(1, 0, sqrt(resvar))

  return(pred + resi)
}






