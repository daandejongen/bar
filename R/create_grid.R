create_grid <- function(z, r, d, eff, thin, tar) {
  grid <- if (is.matrix(r)) r else create_grid_r(z, r, thin, tar)
  grid <- add_d(d, grid)
  grid <- add_start(grid, z, eff)
  colnames(grid) <- c("d", "r0", "r1", "s")

  return(grid)
}

#' @importFrom utils combn
create_grid_r <- function(z, r, thin, tar) {
  z_values <- get_z_values(z, r, thin)
  r_eq  <- matrix(z_values, nrow = length(z_values), ncol = 2, byrow = FALSE)

  if (tar) {
    return(r_eq)
  }

  if (!tar) {
    r_neq <- t(combn(z_values, 2))
    grid <- rbind(r_neq, r_eq)
    return(grid)
  }
}

add_d <- function(d, grid) {
  r0s <- rep(grid[, 1], times = length(d))
  r1s <- rep(grid[, 2], times = length(d))
  ds  <- rep(d, each = nrow(grid))
  grid <- unname(cbind(ds, r0s, r1s))
  return(grid)
}

add_start <- function(grid, z, eff) {
  starts <- apply(grid, MARGIN = 1, FUN = get_start, z = z, eff = eff)
  grid <- unname(cbind(grid, starts))

  grid[starts == -1L, 4] <- 0L
  grid_unknown <- grid[starts == -1L, ]
  grid_unknown[, 4] <- 1L # alternative start

  return(rbind(grid, grid_unknown))
}

# Helpers
#' @importFrom stats quantile
get_z_values <- function(z, r, thin) {
  if (thin) {
    qs <- seq(from = r[1], to = r[2], by = .01)
    values <- quantile(z, qs)
  }
  if (!thin) {
    a <- quantile(z, r)
    values <- sort(unique(z[z >= a[1] & z <= a[2]]))
  }

  return(get_inter_means(values))
}

get_start <- function(g, z, eff) {
  d  <- g[1]
  r0 <- g[2]
  r1 <- g[3]
  go_back <- d

  # If max(p0, p1) > d, and z[eff[1] - d] is in the hysteresis zone,
  # we have some observations at the start of z that may inform us about
  # the starting regime, e.g. if z[eff[1] - d - 1] > r1
  while (go_back <= eff[1] - 1) {
    zi <- z[eff[1] - go_back]
    if (zi <= r0) {
      return(0L)
    } else if (zi <= r1) {
      go_back <- go_back + 1
    } else {
      return(1L)
    }
  }
  return(-1L)
}




