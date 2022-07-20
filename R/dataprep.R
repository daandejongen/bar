create_x <- function(y, p_max) {
  n <- length(y)
  x <- rep(1, n)
  for (i in 1:p) cbind(x, y[(p_max-i+1):(n-i)])
  return(x)
}

create_grid <- function(z, r_bounds, method) {
  r_bounds <- sort(r_bounds)
  a <- ifelse(method == "quantile", quantile(z, r_bounds), r_bounds)
  z <- sort(z[z > a[1] & z < a[2]])
  grid <- t(combn(z, 2))
  return(grid)
}




