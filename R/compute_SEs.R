compute_SEs <- function(X, rv, p0, p1) {
  phi_variances <- diag(solve(t(X) %*% X)) *
    c(rep(rv[1], times = p0 + 1), rep(rv[2], times = p1 + 1))
  names(phi_variances) <- c(paste0("SE_phi0", 0:p0), paste0("SE_phi1", 0:p1))

  return(sqrt(phi_variances))
}

#' @importFrom stats pnorm
compute_p_values <- function(coe, SEs) {
  # Estimators of the coefficients are (asymptotically) normally distributed.
  return(2 * pnorm(abs(coe), mean = 0, sd = SEs, lower.tail = FALSE))
}

#' @importFrom stats qnorm
compute_CIs <- function(coe, SEs, alpha) {
  q <- 1 - alpha / 2
  z <- qnorm(q, mean = 0, sd = 1)
  M <- matrix(c(coe - SEs * z, coe + SEs * z),
              ncol = 2, byrow = FALSE)
  colnames(M) <- c(paste0(round(100 * alpha / 2, 1), "%"),
                   paste0(round(100 - 100 * alpha / 2, 1), "%"))
  rownames(M) <- names(coe)

  return(M)
}

