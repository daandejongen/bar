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
  n <- length(y)
  mean_y <- mean(y)
  acov <- numeric(p)
  for (i in 0:(p - 1)) {
    y_    <- y[(i + 1):n] # delete first i obs from y
    y_lag <- y[1:(n - i)] # delete last i obs from y
    # Compute the autocovariance at lag i (note we always divide by n)
    acov[i + 1] <- sum((y_ - mean_y) * (y_lag - mean_y)) / n
  }

  return(acov)
}

create_acov_mat <- function(acov_vec, y) {
  # This function will return the matrix
  # E[x_t x_t^T]
  # where x_t = (1, y[t-1], ..., y[t-p])
  n <- length(acov_vec) + 1L
  A <- matrix(raw(), nrow = n, ncol = n)
  M <- matrix(acov_vec[abs(col(A) - row(A)) + 1L], n, n)
  M[1, 1:n] <- M[1:n, 1] <- c(1, rep(mean(y), times = length(acov_vec)))

  return(M)
}



