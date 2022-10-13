compute_SEs <- function(y, R, rv, p0, p1) {
  S0 <- create_acov_mat(compute_acov_vec(y * (1 - R), p0))
  S1 <- create_acov_mat(compute_acov_vec(y * R, p1))

  S0 <- solve(S0) * rv[1]
  S1 <- solve(S1) * rv[2]

  variances <- c(diag(S0), diag(S1))

  SEs <- sqrt(variances / length(y))
}


compute_CIs <- function()


compute_acov_vec <- function(y, p) {
  n <- length(y)
  m <- mean(y)
  acov <- numeric(p)
  for (i in 0:(p - 1)) {
    a <- y[(1 + i):n]
    b <- y[1:(n - i)]
    acov[i + 1] <- sum((a - m) * (b - m)) / n
  }

  return(acov)
}


create_acov_mat <- function(acov_vec, y) {
  n <- length(acov_vec) + 1
  M <- matrix(nrow = n, ncol = n)
  for (i in 2:n) {
    for (j in 2:n) {
      M[i, j] <- acov_vec[abs(i - j) + 1]
    }
  }

  M[1, 1:n] <- M[1:n, 1] <- c(1, rep(mean(y), times = length(acov_vec)))

  return(M)
}
