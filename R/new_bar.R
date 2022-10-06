new_bar <- function(y, eff, x, z, p0, p1, optim, select, n_search) {

  optim_d <- select_min_d(optim)
  optim_r <- select_r(optim_d, select)

  d    <- optim_d[1 , "d"]
  a    <- n_ineff(d, p0, p1)
  r    <- optim_r[, c("r0", "r1")]
  z_del<- z[eff - d]
  H    <- ts_hys(z_del, r[1], r[2])
  R    <- ts_reg(H, start = optim_r[1, "s"])
  fit  <- fit(y[eff], create_X(x, p0, p1, R))
  coe  <- coe_to_matrix(fit$coe, p0, p1)
  n    <- c(length(eff), sum(1-R), sum(R))
  rv   <- estimate_resvar(R, fit$res)
  aic  <- compute_aic(rv, n[2], n[3], p0, p1)
  aicc <- compute_aicc(rv, n[2], n[3], p0, p1)
  bic  <- compute_bic(rv, n[2], n[3], p0, p1)
  ic   <- c("aic" = aic, "aicc" = aicc, "bic" = bic)

  rss  <- fit$rss

  rv       <- regname(rv)
  p        <- regname(c(p0, p1))
  names(n) <- c("total", paste0("regime", 0:1))

  data <- new_bardata(y = y[eff],
                      z = z_del,
                      H = H == -1,
                      R = R,
                      r = r,
                      n_ineff = a)

  bar <- structure(
    list(data         = data,
         residuals    = fit$res,
         coefficients = coe,
         delay        = d,
         thresholds   = r,
         orders       = p,
         resvar       = rv,
         rss          = fit$rss,
         ic           = ic,
         n            = n,
         equiv_pars   = optim
         ),
    class = "bar",
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

  zeros0 <- rep(NA, nc - (p0 + 1L))
  zeros1 <- rep(NA, nc - (p1 + 1L))

  coe0 <- c(coe[1:(1 + p0)], zeros0)
  coe1 <- c(coe[(p0+2):(p0+p1+2)], zeros1)

  C <- matrix(c(coe0, coe1), nrow = 2, byrow = TRUE)

  colnames(C) <- c("constant", paste0("Lag ", 1:(nc-1)))
  rownames(C) <- paste0("regime", 0:1)

  return(C)
}






