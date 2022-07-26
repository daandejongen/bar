optimize_delay <- function(y, x, z, d, p0, p1, grid) {
  results <- vector("list", length(d))

  for (i in seq_along(d)) {
    s <- select_obs(n = length(y), d = d[i], p0, p1)
    z_del <- z[s$del]
    result <- optimize_grid(y, x, z = z_del, p0, p1, grid)
    result$d <- d[i]
    results[[i]] <- result
  }

  rsss <- lapply(X = results, FUN = function(x) x$rss)

  return(results)
}


optimize_grid <- function(y, x, z, p0, p1, grid) {
  ns <- nrow(grid)
  rsss <-  numeric(ns)
  starts <- rep("known", ns)

  for (i in 1:n) {
    H <- ts_hys(z, r0 = grid[i, 1], r1 = grid[i, 2])
    if (H[1] == -1) {
      a <- optimize_start(y, x, p0, p1, H)
      rsss[[i]] <- a$rss
      starts[[i]] <- a$start
    } else {
      R <- ts_reg(H)
      rsss[[i]] <- get_rss(y, x, p0, p1, R)
    }
  }
  rssmin <- min(rss)
  argmin <- which(rsss == rssmin)

  return(rss = rssmin,
         start = starts[[argmin]],
         r_est = grid[argmin, ])
}


optimize_start <- function(y, x, p0, p1, H) {
  rss <- c(NA, NA)

  for (i in 0:1) {
    R <- ts_reg(H, init = i)
    rss[i+1] <- fit(y, x, p0, p1, R)
  }

  rss <- min(rss)
  start <- paste0("selected", which.min(rss) - 1)
  return(rss = rss, start = start)
}


# Residual sum of squares (rss)
fit <- function(y, x, R, p0, p1) {
  X <- create_X(x, R, p0, p1)
  coef <- solve(t(X) %*% X) %*% t(X) %*% y
  res <- y - X %*% coef
  rss <- t(res) %*% (res)
  return(list(rss = rss, coef = coef, res = res))
}

# Helper function to create the design matrix
create_X <- function(x, R, p0, p1) {
  n  <- length(R)
  R0 <- matrix(rep(1-R, p0+1), nrow = n, byrow = FALSE)
  R1 <- matrix(rep(R,   p1+1), nrow = n, byrow = FALSE)
  x0 <- x[, 1:(p0+1)]
  x1 <- x[, 1:(p1+1)]
  X  <- cbind(x0 * R0, x1 * R1)
  return(X)
}
