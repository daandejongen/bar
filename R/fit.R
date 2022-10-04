fit <- function(y, X) {
  coe <- solve(t(X) %*% X) %*% t(X) %*% y
  res <- y - X %*% coe
  rss <- t(res) %*% (res)

  return(list(rss = rss[1, 1, drop = TRUE],
              coe = coe[ , 1, drop = TRUE],
              res = res[ , 1, drop = TRUE]))
}


estimate_resvar <- function(R, res) {

  n0 <- sum(1-R)
  n1 <- sum(R)
  res0 <- res * (1-R)
  res1 <- res * R

  resvar0 <- (res0 %*% res0) / n0
  resvar1 <- (res1 %*% res1) / n1

  return(c(resvar0, resvar1))
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

