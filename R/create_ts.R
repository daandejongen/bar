ts_hys <- function(z, r0, r1) {
  # -1 indicates the hysteresis zone, 0 and 1 the
  # lower and upper regime, respectively
  H  <- rep(-1L, length(z))
  H  <- H + (z <= r0) + (z > r1) * 2L

  return(H)
}

ts_reg <- function(H, start = NULL) {
  # start is needed when starting regime is unknown, but
  # when known, the value of start cannot alter the correct R in any way,
  # because it is deleted afterwards.
  if (is.null(start)) start <- 9L
  loop_over <- which(H == -1)

  R <- c(start, H)
  # The + 1 is because we added a start, so the indices need to shift one
  for (i in loop_over + 1) if (R[i] == -1L) R[i] <- R[i-1]
  R <- R[-1]

  return(R)
}




#' @importFrom stats rnorm
AR <- function(x, coe, resvar) {
  # One time step AR simulation
  pred <- x %*% coe
  resi <- rnorm(1, 0, sqrt(resvar))

  return(pred + resi)
}






