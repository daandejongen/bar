# Function to create n - p by p + 1 matrix of lagged predictors
select_obs <- function(n, d, p) {
  a <- max(d, p) + 1 # Time index of first effective observation
  effective <- a:n
  delayed <- (a-d):(n-d)
  return(list(eff = effective, del = delayed))
}

create_x <- function(y, eff, p) {
  x <- matrix(1, nrow = length(eff), ncol = 1)

  if (p == 0) {
    return(x)
  }

  for (i in 1:p) {
    x <- cbind(x, y[eff-i])
  }

  return(x)
}

create_grid <- function(z, r_bounds, search) {
  if (search == "none") {
    return(matrix(r, ncol=2))
  }

  a <- if (search == "quantile") quantile(z, r_bounds) else r_bounds
  z_val <- sort(unique(z[z >= a[1] & z <= a[2]]))

  r_neq <- t(combn(z_val, 2))
  r_eq  <- matrix(z_val, nrow = length(z_val), ncol = 2, byrow = FALSE)
  grid  <- rbind(r_neq, r_eq)

  return(grid)
}




