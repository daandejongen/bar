select_obs <- function(n, d, p0, p1) {
  a <- max(p0, p1) + 1 # Time index of first effective observation
  effective <- a:n
  delayed <- (a-d):(n-d)
  return(list(eff = effective, del = delayed))
}

# Function to create n - p by p + 1 matrix of lagged predictors
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

get_z_values <- function(search, r_bounds) {
  a <- if (search == "quantile") quantile(z, r_bounds) else r_bounds
  z_val <- sort(unique(z[z >= a[1] & z <= a[2]]))
  return(z_val)
}

create_grid <- function(z, d, r_bounds, search) {
  if (search == "none") {
    return(matrix(r, ncol=2))
  }

  z_values <- get_z_values(search, r_bounds)
  r_neq <- t(combn(z_values, 2))
  r_eq  <- matrix(z_values, nrow = length(z_val), ncol = 2, byrow = FALSE)
  grid <- rbind(r_neq, r_eq)
  colnames(grid) <- c("r0", "r1")

  return(grid)
}






