fit <- function(y, X) {
  coef <- tryCatch(
    error = function(cond) NULL,
    solve(t(X) %*% X) %*% t(X) %*% y
  )

  if (is.null(coef)){
    return(list(rss = Inf, coef = NA, res = NA))
  }

  res <- y - X %*% coef
  rss <- t(res) %*% (res)

  return(list(rss = rss, coef = coef, res = res))
}
