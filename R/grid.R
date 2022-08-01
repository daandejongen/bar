#' @importFrom stats quantile
get_z_values <- function(z, search, r_bounds) {
  a <- if (search == "quantile") quantile(z, r_bounds) else r_bounds
  z_val <- sort(unique(z[z >= a[1] & z <= a[2]]))
  return(z_val)
}


#' @importFrom utils combn
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
