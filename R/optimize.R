optimize_grid <- function(y, eff, x, z, p0, p1, grid) {
  # The high-level optimizer, first creates matrix of possible R
  # time series based on the search grid, then selects optimal rows.
  Rs <- get_Rs(y, eff, x, z, p0, p1, grid)
  optim <- optim_R(y, x, p0, p1, Rs)

  return(est = grid[optim$argsmin, , drop = FALSE],
         R_est = Rs[optim$argsmin, , drop = FALSE])
}


select_min_d <- function(M) {
  # If multiple delay values yield the same optimal solution,
  # the smallest value for d is selected.
  sel <- which(M[, "d"] == min(M[, "d"]))
  return(M[sel, ])
}


get_Rs <- function(y, eff, x, z, p0, p1, grid) {
  # Create a matrix with R time series in the rows,
  # for all possible settings of r and d. If start is unclear,
  # optimal start of R is selected.
  Rs <- matrix(NA, nrow = nrow(grid), ncol = length(y))

  for (i in 1:nrow(grid)) {
    z_del <- z[eff - grid[i, "d"]]
    H <- ts_hys(z = z_del, r0 = grid[i, "r0"], r1 = grid[i, "r1"])
    if (H[1] == -1L) Rs[i, ] <- optim_start(y, x, p0, p1, H)
    if (H[1] != -1L) Rs[i, ] <- ts_reg(H)
  }

  return(Rs)
}


optim_R <- function(y, x, p0, p1, Rs) {
  # From a matrix with R time series in the rows, select the one(s)
  # with the least residual sum of squares.
  get_rss <- function(R) {
    X <- create_X(x, p0, p1, R)
    return(fit(y, X)$rss)
  }

  results <- apply(X = Rs, MARGIN = 1, FUN = get_rss)

  return(list(rss = min(results),
              argsmin = which(results == min(results)))
         )
}


optim_start <- function(y, x, p0, p1, H) {
  # Select the optimal R time series if the start is unclear
  # (the first delayed obs of z falls in the hysteresis zone),
  # based on the residual sum of squares of both options.
  # Is called by the creator of the R matrix. This leads to some
  # double fitting of the same R (later in the optimization of Rs
  # the series will be fitted again), but yields a clearer structure.
  Rs <- matrix(c(ts_reg(H, 0), ts_reg(H, 1)), nrow = 2, byrow = TRUE)
  optim <- optim_R(y, x, p0, p1, Rs)

  return(R_sel = Rs[optim$argsmin, ])
}




