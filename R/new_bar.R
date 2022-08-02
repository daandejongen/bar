new_bar <- function(y, eff, x, z, p0, p1, optim, select, n_search) {

  optim_d <- select_min_d(optim)
  optim_r <- select_r(optim_d, select)

  d    <- optim_d[1 , "d"]
  r    <- optim_r[, c("r0", "r1")]
  z    <- z[eff - d]
  H    <- ts_hys(z, r[1], r[2])
  R    <- ts_reg(H, start = optim_r[1, "s"])
  fit  <- fit(y, create_X(x, p0, p1, R))
  coe  <- coe_to_matrix(fit$coe, p0, p1)
  rv   <- regname(estimate_resvar(R, fit$res))
  p    <- regname(c(p0, p1))
  rss  <- fit$rss
  n    <- c(length(y), sum(1-R), sum(R))
  names(n) <- c("total", paste0("regime", 0:1))

  data <- data.frame(y = y, z = z, hysteresis = (H == -1), regime = R)

  bar <- structure(
    list(data         = data,
         residuals    = fit$res,
         coefficients = coe,
         delay        = d,
         thresholds   = r,
         orders       = p,
         resvar       = rv,
         rss          = fit$rss,
         n            = n,
         equiv_pars   = optim
         ),
    class   = "bar",
    n_grid_search = n_search
    )

  return(bar)
}


# Helpers

regname <- function(x) {
  names(x) <- paste0("regime", 0:1)
  return(x)
}

coe_to_matrix <- function(coe, p0, p1) {
  nc <- 1L + max(p0, p1)

  zeros0 <- rep(0L, nc - (p0 + 1L))
  zeros1 <- rep(0L, nc - (p1 + 1L))

  coe0 <- c(coe[1:(1 + p0)], zeros0)
  coe1 <- c(coe[(p0+2):(p0+p1+2)], zeros1)

  C <- matrix(c(coe0, coe1), nrow = 2, byrow = TRUE)

  colnames(C) <- c("constant", paste0("Lag ", 1:(nc-1)))
  rownames(C) <- paste0("regime", 0:1)

  return(C)
}






