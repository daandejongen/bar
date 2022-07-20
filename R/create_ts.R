ts_hys <- function(z, r0, r1) {
  H  <- rep(-1L, times=length(z))
  # Filling in the upper regime (=1) and lower regime (=0)
  H  <- H + (z <= r0) * 2L
  H  <- H + (z >= r1)
  # -1 is the value that indicates hysteresis
  return(H)
}

ts_reg <- function(H, init) {
  # init, initial value, is needed when starting regime is unknown, but
  # when known, the value of init cannot alter the correct R in any way.
  R <- c(init, H)
  for (i in 2:length(R)) if (R[i-1] == -1) R[i] <- R[i-1]
  R <- R[-1]
}
