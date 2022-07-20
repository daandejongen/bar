compute_rss <- function(y, x, R, p0, p1, n){
  X <- create_X(x, R, p0, p1, n)
  coef <- solve(t(X) %*% X) %*% t(X) %*% y
  rss <- t(y - X %*% coef) %*% (y - X %*% coef)
  return(rss)
}

# Helper function to create the design matrix
create_X <- function(x, R, p0, p1, n) {
  R0 <- matrix(rep(1-R, p0+1), nrow=n, byrow=FALSE)
  R1 <- matrix(rep(R, p0+1),   nrow=n, byrow=FALSE)
  x0 <- x[, 1:(p0+1)]
  x1 <- x[, 1:(p1+1)]
  X <- cbind(x0 * R0, x1 * R1)
}
