compute_ic <- function(rv, n0, n1, p0, p1, tar) {
  bic  <- unname(compute_bic(rv, n0, n1, p0, p1))
  aic  <- unname(compute_aic(rv, n0, n1, p0, p1))
  aicc <- unname(compute_aicc(rv, n0, n1, p0, p1))
  aiccp <- aic + if (tar) 6 else 12

  return(c(bic = bic, aic = aic, aicc = aicc, aiccp = aiccp))
}

compute_bic <- function(rv, n0, n1, p0, p1) {
  value <- n0 * log(rv[1]) + log(n0) * (p0 + 2L) +
    n1 * log(rv[2]) + log(n1) * (p1 + 2L)

  return(value)
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




