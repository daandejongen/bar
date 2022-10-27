compute_ic <- function(rv, n0, n1, p0, p1) {
  aic  <- compute_aic(rv, n0, n1, p0, p1)
  aicc <- compute_aicc(rv, n0, n1, p0, p1)
  bic  <- compute_bic(rv, n0, n1, p0, p1)

  return(c(aic = unname(aic), aicc = unname(aicc), bic = unname(bic)))
}

compute_aic <- function(rv, n0, n1, p0, p1) {
  value <- n0*log(rv[1]) + 2*(p0 + 2) +
    n1*log(rv[2]) + 2*(p1 + 2)

  return(value)
}


compute_aicc <- function(rv, n0, n1, p0, p1) {
  value <- n0*log(rv[1]) + 2*n0*(p0 + 2)/(n0-p0-3) +
    n1*log(rv[2]) + 2*n1*(p1 + 2)/(n1-p1-3)

  return(value)
}


compute_bic <- function(rv, n0, n1, p0, p1) {
  value <- n0*log(rv[1]) + log(n0)*(p0 + 2) +
    n1*log(rv[2]) + log(n1)*(p1 + 2)

  return(value)
}
