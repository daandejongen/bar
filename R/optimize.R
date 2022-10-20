optim_grid <- function(y, eff, x, z, p0, p1, grid) {
  results <- numeric(nrow(grid))
  prev <- rep(-9L, times = length(eff))

  for (i in 1:nrow(grid)) {
    H <- ts_hys(z[eff - grid[i, "d"]], grid[i, "r0"], grid[i, "r1"])
    R <- ts_reg(H, start = grid[i, "s"])
    if (all(R == prev)) {
      results[i] <- results[i-1]
    } else {
      X <- create_X(x, p0, p1, R)
      results[i] <- fit(y, X)$rss
    }
    prev <- R
  }
  argsmin <- which(results == min(results))

  return(list(est = grid[argsmin, , drop = FALSE], min_rss = min(results)))
}


select_min_d <- function(M) {
  # If multiple delay values yield the same optimal solution,
  # the smallest value for d is selected.
  select <- which(M[, "d"] == min(M[, "d"]))
  return(M[select, , drop = FALSE])
}


select_r <- function(M, select) {
  dist <- apply(X = M[, 2:3, drop = FALSE], MARGIN = 1, FUN = sum)
  # If there are multiple pairs that are the widest (smallest),
  # which.max (which.min) makes sure that only one is selected
  if (select == "widest")   row_select <- which.max(dist)
  if (select == "smallest") row_select <- which.min(dist)
  return(M[row_select, , drop = FALSE])
}




