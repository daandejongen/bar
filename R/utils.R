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


lag_obs <- function(y, i, p) {
  return(if (p == 0) 1 else c(1, y[(i-1):(i-p)]))
}


get_order <- function(coe) {
  return(length(coe) - 1)
}
