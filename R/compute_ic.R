compute_ic <- function(rv, n0, n1, p0, p1) {
  aic  <- unname(compute_aic(rv, n0, n1, p0, p1))
  aicc <- unname(compute_aicc(rv, n0, n1, p0, p1))
  bic  <- unname(compute_bic(rv, n0, n1, p0, p1))

  return(c(aic = aic, aicc = aicc, bic = bic))
}

compute_aic <- function(rv, n0, n1, p0, p1) {
  value <- n0 * log(rv[1]) + 2L * (p0 + 2L) +
    n1 * log(rv[2]) + 2L * (p1 + 2L)

  return(value)
}

compute_aicc <- function(rv, n0, n1, p0, p1) {
  value <- n0 * log(rv[1]) + 2L * n0 * (p0 + 2L) / (n0 - p0 - 3L) +
    n1 * log(rv[2]) + 2L * n1 * (p1 + 2L) / (n1 - p1 - 3L)

  return(value)
}

compute_bic <- function(rv, n0, n1, p0, p1) {
  value <- n0 * log(rv[1]) + log(n0) * (p0 + 2L) +
    n1 * log(rv[2]) + log(n1) * (p1 + 2L)

  return(value)
}
