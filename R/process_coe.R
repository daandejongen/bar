create_coe_matrix <- function(coe, p0, p1) {
  coes <- split_coe(coe, p0, p1)
  coes_eq_length <- complete_shorter_coe(coes$coe0, coes$coe1)

  C <- matrix(c(coes_eq_length$coe0,
                coes_eq_length$coe1),
              nrow = 2, byrow = TRUE)

  colnames(C) <- c(paste0("phi", 0:max(p0, p1)))
  rownames(C) <- paste0("Regime", 0:1)

  return(C)
}


create_coe_matrix_SE_p <- function(coe, y, R, rv, p0, p1) {
  SEs <- compute_SEs(y, R, rv, p0, p1)
  p_values <- compute_p_values(coe, SEs)

  M <- matrix(c(coe, SEs, p_values), ncol = 3, byrow = FALSE)
  rownames(M) <- c(paste0("phi_0", 0:p0), paste0("phi_1", 0:p1))
  colnames(M) <- c("est", "SE", "p")

  return(M)
}

# Helpers
regname <- function(x) {
  names(x) <- paste0("Regime", 0:1)
  return(x)
}

split_coe <- function(coe, p0, p1) {
  # We are given one vector of coefficients, but from this only it is unclear
  # where the coefficients of regime 0 stop and regime 1 starts.
  # With the orders we can split this vector at the right place, note that
  # the number of coefficients is one less than the order because of phi0.
  nc0 <- p0 + 1L
  nc1 <- p1 + 1L

  return(list(coe0 = coe[1:nc0], coe1 = coe[(nc0 + 1):(nc0 + nc1)]))
}

complete_shorter_coe <- function(coe0, coe1) {
  # When we want to present both vectors of coefficients (from both regimes)
  # in rows of a matrix, they have to be equally long.
  max_length <- max(length(coe0), length(coe1))
  NAs0 <- rep(NA, max_length - length(coe0))
  NAs1 <- rep(NA, max_length - length(coe1))

  return(list(coe0 = c(coe0, NAs0), coe1 = c(coe1, NAs1)))
}
