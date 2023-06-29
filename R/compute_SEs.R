compute_SEs <- function(y, R, rv, p0, p1) {
  # From Li, Guan, Li and Yu (2015)
  # We first need the autocovariance matrix up to order p.
  S0 <- create_acov_mat(compute_acov_vec(y * (1L - R), p0), y)
  S1 <- create_acov_mat(compute_acov_vec(y * R, p1), y)

  S0 <- solve(S0) * rv[1]
  S1 <- solve(S1) * rv[2]

  variances <- c(diag(S0), diag(S1))
  coenames <- c(paste0("R0_phi", 0:p0), paste0("R1_phi", 0:p1))

  # For small N, there can be problems with positive semi-definiteness(?)
  if (!all(variances > 0)) {
    neg <- coenames[which(variances <= 0)]
    warning(paste0("Some standard errors could not be estimated, \nsince the ",
                   "estimators of ", paste0(neg, collapse = ", "),
                   " have negative variances."),
            call. = FALSE)
  }

  # We want to suppress the warnings with NAs from negative variances here
  # (we have notified the user above).
  suppressWarnings({
    SEs <- sqrt(variances / length(y))
    names(SEs) <- coenames
    })

  return(SEs)
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

compute_acov_vec <- function(y, p) {
  # Note that acov_vec contains the autocovs of lag 0, 1, ..., p - 1.
  n <- length(y)
  mean_y <- mean(y)
  acov <- numeric(p)

  if (p > 0) {
    for (i in 0:(p-1)) {
      y_    <- y[(i + 1):n] # delete first i obs from y
      y_lag <- y[1:(n - i)] # delete last i obs from y
      # Compute the autocovariance at lag i (note we always divide by n)
      acov[i + 1] <- sum((y_ - mean_y) * (y_lag - mean_y)) / n
    }

    return(acov)
  }

  if (p == 0) {
    return(NULL)
  }

}

create_acov_mat <- function(acov_vec, y) {
  # This function will return the (p+1) by (p+1) matrix
  # 1 m m m m
  # m E[x_t x_t^T]
  # m
  # m
  #
  # where x_t = (x_{t-1}, x_{t-2}, ..., x_{t-p})
  # and m is the mean of y
  m <- mean(y)
  p <- length(acov_vec)

  # Outer part
  M <- matrix(NA, nrow = p + 1, ncol = p + 1)
  M[1, 1:(p + 1)] <- M[1:(p + 1), 1] <- c(1, rep(m, times = p))

  if (p == 0) {
    return(M)
  }

  # Inner lower right part of the matrix, E[x_t x_t^T]
  # note that acov_vec contains the autocovs of lag 0, 1, ..., p - 1.
  A <- matrix(nrow = p, ncol = p)
  A[] <- acov_vec[abs(col(A) - row(A)) + 1L] + m**2

  # Filling inner part in outer part
  M[2:(p + 1), 2:(p + 1)] <- A

  return(M)
}



