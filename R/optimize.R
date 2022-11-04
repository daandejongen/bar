optim_p <- function(y, x, z, eff, grid, p_options, r_select, ic_method) {
  equivs <- vector(mode = "list", length = nrow(p_options))

  for (i in 1:nrow(p_options)) {
    p0_sel <- p_options[i, "p0"]
    p1_sel <- p_options[i, "p1"]
    optim <- optim_grid(y, eff, x, z, p0_sel, p1_sel, grid, r_select)
    est <- optim$est
    p_options[i, c("d", "r0", "r1", "s")] <- optim$est
    equivs[[i]] <- optim$equiv
    results <- run_model(y, x, z, eff, p0_sel, p1_sel,
                         d_sel = est["d"], r0_sel = est["r0"],
                         r1_sel = est["r1"], s_sel = est["s"],
                         return_HR = FALSE)
    p_options[i, "ic"] <- results$ic[ic_method]
  }
  argmin <- which(p_options[, "ic"] == min(p_options[, "ic"]))

  return(list(est = p_options[argmin, , drop = TRUE],
              equiv = equivs[[argmin]])
  )
}

optim_grid <- function(y, eff, x, z, p0, p1, grid, r_select) {
  optims <- get_optims(y, eff, x, z, p0, p1, grid)
  optims_d <- select_min_d(optims)
  optims_r <- select_r(optims_d, r_select)

  return(list(est = optims_r[, , drop = TRUE], equiv = optims))
}

get_optims <- function(y, eff, x, z, p0, p1, grid) {
  results <- numeric(nrow(grid))
  prev <- rep(-9L, times = length(eff))

  for (i in 1:nrow(grid)) {
    H <- ts_hys(z[eff - grid[i, "d"]], grid[i, "r0"], grid[i, "r1"])
    R <- ts_reg(H, start = grid[i, "s"])
    if (all(R == prev)) {
      results[i] <- results[i-1]
    } else {
      X <- create_X(x, p0, p1, R)
      results[i] <- fit(y[eff], X)$rss
    }
    prev <- R
  }
  argsmin <- which(results == min(results))

  return(grid[argsmin, , drop = FALSE])
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




