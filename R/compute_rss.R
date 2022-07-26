

# Residual sum of squares (rss)
compute_rss <- function(y, x, R, p0, p1, n) {
  X <- create_X(x, R, p0, p1, n)
  coef <- solve(t(X) %*% X) %*% t(X) %*% y
  res <- y - X %*% coef
  rss <- t(res) %*% (res)
  return(list(rss = rss, coef = coef, res = res))
}

# Helper function to create the design matrix
create_X <- function(x, R, p0, p1) {
  n  <- length(R)
  R0 <- matrix(rep(1-R, p0+1), nrow = n, byrow = FALSE)
  R1 <- matrix(rep(R,   p1+1), nrow = n, byrow = FALSE)
  x0 <- x[, 1:(p0+1)]
  x1 <- x[, 1:(p1+1)]
  X  <- cbind(x0 * R0, x1 * R1)
  return(X)
}
