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
  resvar <- c((res0 %*% res0) / n0, (res1 %*% res1) / n1)

  return(resvar)
}
