time_eff <- function(y, d, p0, p1) {
  # Time points of the effective observations
  n <- length(y)
  b <- n_ineff(d, p0, p1) + 1 # Time index of first effective observation
  return(b:n)
}

time_del <- function(y, d, p0, p1, d_sel) {
  # Time points of the delayed observations
  return(time_eff(y, d, p0, p1) - d_sel)
}

n_ineff <- function(d, p0, p1) {
  return(max(d, p0, p1))
}

lag_obs <- function(y, t, p) {
  # Returns the values of the lagged predictor, i.e., y[t-1], ..., y[t-p]
  # adds a 1 at the start for the constant predictor (the intercept)
  return(if (p == 0) 1 else c(1, y[(t-1):(t-p)]))
}

get_order <- function(coe) {
  return(length(coe) - 1)
}

get_sw_pnts <- function(R) {
  n <- length(R)
  return(which(c(FALSE, R[2:n] - R[1:(n-1)] != 0)))
}

