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
  coe <- remove_trailing_zeros(coe)
  return(length(coe) - 1)
}

remove_trailing_zeros <- function(x, length_out = 1) {
  last_is_zero <- x[length(x)] == 0

  while (last_is_zero && length(x) > length_out) {
    x <- x[-length(x)]
    last_is_zero <- x[length(x)] == 0
  }

  return(x)
}

remove_z_buffer <- function(z, d, p0, p1) {
  if (!is.numeric(z)) error_numeric(z)
  if (!is_z_simulated(z)) return(z)
  p <- if (attr(z, "start_regime") == 1) p1 else p0
  start <- 11 - max(p, d) + d
  stop <- length(z) - max(p, d) + d
  out <- z[start:stop]
  attributes(out) <- attributes(z)

  return(out)
}

is_z_simulated <- function(z) {
  out <- !is.null(attributes(z)) &&
    all(names(attributes(z)) == c("start_regime", "start_hyst"))

  return(out)
}

get_inter_means <- function(x) {
  n <- length(x)
  lag_mat <- matrix(c(x[1:(n - 1)], x[2:n]), nrow = 2, byrow = TRUE)
  return(colMeans(lag_mat))
}

