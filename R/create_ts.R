ts_hys <- function(z, r0, r1) {
  # -1 indicates the hysteresis zone, 0 and 1 the
  # lower and upper regime, respectively.
  H  <- rep(-1L, length(z))
  H  <- H + (z <= r0) + (z > r1) * 2L

  return(H)
}

ts_reg <- function(H, start = NULL) {
  # 'start' is needed when starting regime is unknown, but
  # when known, the value of start cannot alter the correct R in any way,
  # because it is deleted afterwards.
  if (is.null(start)) start <- 9L
  R <- get_regimes(c(start, H))

  return(R[-1]) #Remove start again.
}





