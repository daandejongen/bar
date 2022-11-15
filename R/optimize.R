optim_p <- function(y, x, z, eff, grid, p_options, p_select) {
  equivs <- vector(mode = "list", length = nrow(p_options))

  for (i in seq_len(nrow(p_options))) {
    p0_sel <- p_options[i, "p0"]
    p1_sel <- p_options[i, "p1"]
    optim <- optim_grid(y, eff, x, z, p0_sel, p1_sel, grid)
    p_options[i, c("d", "r0", "r1", "s")] <- optim$est
    equivs[[i]] <- optim$equiv
    results <- run_model(y, x, z, eff, p0_sel, p1_sel,
                         d_sel  = optim$est["d"],
                         r0_sel = optim$est["r0"],
                         r1_sel = optim$est["r1"],
                         s_sel  = optim$est["s"],
                         return_HR = FALSE)
    p_options[i, "ic"] <- results$ic[p_select]
  }
  argmin <- which(p_options[, "ic"] == min(p_options[, "ic"]))

  return(list(est = p_options[argmin, , drop = TRUE],
              equiv = equivs[[argmin]])
  )
}

optim_grid <- function(y, eff, x, z, p0, p1, grid) {
  optims <- get_optims(y, eff, x, z, p0, p1, grid)
  optims_d <- select_min_d(optims)
  optims_r <- select_r(optims_d)

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
  row_select <- which(M[, "d"] == min(M[, "d"]))
  return(M[row_select, , drop = FALSE])
}

select_r <- function(M) {
  dist <- apply(X = M[, c("r0", "r1"), drop = FALSE], MARGIN = 1,
                FUN = function(x) abs(x[1] - x[2]))
  # If there are multiple pairs that are the smallest,
  # which.min makes sure that only one is selected
  row_select <- which.min(dist)
  return(M[row_select, , drop = FALSE])
}

#' @importFrom utils combn
create_p_options <- function(p0, p1) {
  # We want to have every way to choose a value for p0 from 0, 1, ..., max_p0
  # and a value for p1 from 0, 1, ..., max_p1.
  p0_options <- rep(p0, each = length(p1))
  p1_options <- rep(p1, times = length(p0))

  p_mat <- matrix(c(p0_options, p1_options), ncol = 2)
  colnames(p_mat) <-  c("p0", "p1")
  drs_mat <- matrix(nrow = nrow(p_mat), ncol = 4)
  colnames(drs_mat) <-  c("d", "r0", "r1", "s")
  ic_mat <- matrix(nrow = nrow(p_mat), ncol = 1)
  colnames(ic_mat) <-  "ic"

  return(cbind(p_mat, drs_mat, ic_mat))
}


