create_x <- function(y, eff, p0, p1) {
  x <- matrix(1, nrow = length(eff), ncol = 1)

  p <- max(p0, p1)

  if (p == 0) {
    return(x)
  }

  for (i in 1:p) {
    x <- cbind(x, y[eff - i])
  }

  return(x)
}


create_X <- function(x, p0, p1, R) {
  n  <- nrow(x)
  R0 <- matrix(rep(1L - R, p0 + 1), nrow = n, byrow = FALSE)
  R1 <- matrix(rep(R, p1 + 1), nrow = n, byrow = FALSE)
  x0 <- x[, 1:(p0+1)]
  x1 <- x[, 1:(p1+1)]
  X  <- cbind(x0 * R0, x1 * R1)
  return(X)
}




