new_hystar <- function(y, eff, x, z, p0, p1, optim, select, n_search) {

  # Selecting the final estimates of the grid search
  optim_d <- select_min_d(optim)
  optim_r <- select_r(optim_d, select)
  d    <- optim_d[1 , "d"]
  a    <- n_ineff(d, p0, p1)
  r    <- optim_r[, c("r0", "r1")]

  # Fitting the model one last time with the grid search estimates
  z_del<- z[eff - d]
  H    <- ts_hys(z_del, r[1], r[2])
  R    <- ts_reg(H, start = optim_r[1, "s"])
  fit  <- fit(y[eff], create_X(x, p0, p1, R))

  coe  <- fit$coe
  names(coe) <- c(paste0("R0_phi", 0:p0), paste0("R1_phi", 0:p1))

  # Information criteria
  n    <- c(length(eff), sum(1-R), sum(R))
  rv   <- estimate_resvar(R, fit$res)
  aic  <- compute_aic(rv, n[2], n[3], p0, p1)
  aicc <- compute_aicc(rv, n[2], n[3], p0, p1)
  bic  <- compute_bic(rv, n[2], n[3], p0, p1)
  ic   <- c("aic" = aic, "aicc" = aicc, "bic" = bic)

  # Some naming stuff
  rv       <- regname(rv)
  p        <- regname(c(p0, p1))
  names(n) <- c("total", paste0("regime", 0:1))

  data <- new_hystar_data(y = y[eff],
                      z = z_del,
                      H = H == -1,
                      R = R,
                      r = r,
                      n_ineff = a)

  hystar <- structure(
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
    class = "hystar"
    )

  return(hystar)
}


new_hystar_data <- function(y, z, H, R, r, n_ineff) {
  out <- structure(
    data.frame(y = y, z = z, H = H, R = R),
    class = "hystar_data",
    r = r, n_ineff = n_ineff
  )

  return(out)
}








