gridmaker <- function(z, r_bounds, search, d, eff) {
  grid <- create_grid_r(z, r_bounds, search)
  grid <- add_d(d, grid)
  grid <- add_start(grid, z, eff)
  colnames(grid) <- c("d", "r0", "r1", "s")

  return(grid)
}


#' @importFrom utils combn
create_grid_r <- function(z, r_bounds, search) {

  if (search %in% c("none", "custom")) {
    return(matrix(r_bounds, ncol = 2))
  }

  z_values <- get_z_values(z, search, r_bounds)
  r_neq <- t(combn(z_values, 2))
  r_eq  <- matrix(z_values, nrow = length(z_values), ncol = 2, byrow = FALSE)
  grid <- rbind(r_neq, r_eq)

  return(grid)
}


#' @importFrom stats quantile
get_z_values <- function(z, search, r_bounds) {
  a <- if (search == "quantile") quantile(z, r_bounds) else r_bounds
  z_val <- sort(unique(z[z >= a[1] & z <= a[2]]))
  return(z_val)
}


add_d <- function(d, grid) {
  r0s <- rep(grid[, 1], times = length(d))
  r1s <- rep(grid[, 2], times = length(d))
  ds  <- rep(d, each = nrow(grid))
  grid <- unname(cbind(ds, r0s, r1s))
  return(grid)
}


add_start <- function(grid, z, eff) {
  start_known <- get_start_known(grid[, 1], grid[, 2], grid[, 3], z, eff)
  grid <- unname(cbind(grid, start_known))

  grid_unknown <- grid[!start_known, ]
  grid_unknown[, 4] <- 1 # alternative start

  return(rbind(grid, grid_unknown))
}


get_start_known <- function(ds, r0s, r1s, z, eff) {
  ifelse(z[eff[1] - ds] <= r0s | z[eff[1] - ds] > r1s,
         yes = 9L, no = 0L)
}







