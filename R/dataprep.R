time_eff <- function(y, d, p0, p1) {
  # Time points of the effective observations
  n <- length(y)
  b <- n_ineff(d, p0, p1) + 1 # Time index of first effective observation
  return(b:n)
}


time_del <- function(y, d, p0, p1, d_sel) {
  # Time points of the delayed observations
  return(time_eff(y, d, p0, p1) - d_sel)
}


n_ineff <- function(d, p0, p1) {
  return(max(d, p0, p1))
}


lag_obs <- function(y, i, p) {
  return(if (p == 0) 1 else c(1, y[(i-1):(i-p)]))
}


get_order <- function(coe) {
  return(length(coe) - 1)
}


create_x <- function(y, d, p0, p1) {
  # Function to create n - p by p + 1 matrix of lagged predictors
  eff <- time_eff(y, d, p0, p1)
  x <- matrix(1, nrow = length(eff), ncol = 1)

  p <- max(p0, p1)

  if (p == 0) {
    return(x)
  }

  for (i in 1:p) {
    x <- cbind(x, y[eff-i])
  }

  return(x)
}


create_X <- function(x, p0, p1, R) {
  n  <- nrow(x)
  R0 <- matrix(rep(1L-R, p0+1), nrow = n, byrow = FALSE)
  R1 <- matrix(rep(R,    p1+1), nrow = n, byrow = FALSE)
  x0 <- x[, 1:(p0+1)]
  x1 <- x[, 1:(p1+1)]
  X  <- cbind(x0 * R0, x1 * R1)
  return(X)
}


get_z_values <- function(z, search, r_bounds) {
  a <- if (search == "quantile") quantile(z, r_bounds) else r_bounds
  z_val <- sort(unique(z[z >= a[1] & z <= a[2]]))
  return(z_val)
}


create_grid_r <- function(z, r_bounds, search) {

  if (search == "none") {
    return(matrix(r_bounds, ncol=2))
  }

  z_values <- get_z_values(z, search, r_bounds)
  r_neq <- t(combn(z_values, 2))
  r_eq  <- matrix(z_values, nrow = length(z_values), ncol = 2, byrow = FALSE)
  grid <- rbind(r_neq, r_eq)

  return(grid)
}


add_d <- function(d, grid) {
  r0s <- rep(grid[, 1], times = length(d))
  r1s <- rep(grid[, 2], times = length(d))
  ds  <- rep(d, each = nrow(grid))
  grid <- cbind(ds, r0s, r1s)
  colnames(grid) <- c("d", "r0", "r1")
  return(grid)
}






