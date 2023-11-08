run_model <- function(y, x, z, eff,
                      p0_sel, p1_sel, d_sel, r0_sel, r1_sel, s_sel, return_HR = TRUE) {
  y_eff <- y[eff]
  z_del <- z[eff - d_sel]

  H <- ts_hys(z_del, r0_sel, r1_sel)
  R <- ts_reg(H, s_sel)
  n0 <- sum(1L - R)
  n1 <- sum(R)

  X <- create_X(x, p0_sel, p1_sel, R)
  fit <- fit(y_eff, X)
  resvar <- estimate_resvar(R, fit$res)
  names(resvar) <- c("sigma2_0", "sigma2_1")
  ic <- compute_ic(resvar, n0, n1, p0_sel, p1_sel)
  SEs <- compute_SEs(X, resvar, p0_sel, p1_sel)

  if (return_HR) {
    return(list(fit = fit, SEs = SEs, resvar = resvar, ic = ic, H = H, R = R))
  } else {
    return(list(fit = fit, SEs = SEs, resvar = resvar, ic = ic))
  }

}
