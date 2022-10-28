compute_SEs <- function(y, R, rv, p0, p1) {
  S0 <- create_acov_mat(compute_acov_vec(y * (1 - R), p0), y)
  S1 <- create_acov_mat(compute_acov_vec(y * R, p1), y)

  S0 <- solve(S0) * rv[1]
  S1 <- solve(S1) * rv[2]

  variances <- c(diag(S0), diag(S1))
  coenames <- c(paste0("R0_phi", 0:p0), paste0("R1_phi", 0:p1))

  if (!all(variances > 0)) {
    neg <- coenames[which(variances <= 0)]
    warning(paste0("Some standard errors could not be estimated, \nsince the ",
                   "estimators of ", paste0(neg, collapse = ", "),
                   " have negative variances."),
            call. = FALSE)
  }

  suppressWarnings({
    SEs <- sqrt(variances / length(y))
    })

  names(SEs) <- coenames

  return(SEs)
}

#' @importFrom stats pnorm
compute_p_values <- function(coe, SEs) {
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
  A <- matrix(raw(), nrow = n, ncol = n)
  M <- matrix(acov_vec[abs(col(A) - row(A)) + 1L], n, n)

  M[1, 1:n] <- M[1:n, 1] <- c(1, rep(mean(y), times = length(acov_vec)))

  return(M)
}



