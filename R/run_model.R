run_model <- function(y, x, z, eff,
                      p0_sel, p1_sel, d_sel, r0_sel, r1_sel, s_sel,
                      return_HR) {
  y_eff <- y[eff]
  z_del <- z[eff - d_sel]

  H <- ts_hys(z_del, r0_sel, r1_sel)
  R <- ts_reg(H, s_sel)
  n0 <- sum(1L - R)
  n1 <- sum(R)

  X <- create_X(x, p0_sel, p1_sel, R)
  fit <- fit(y_eff, X)
  resvar <- estimate_resvar(R, fit$res)
  ic <- compute_ic(resvar, n0, n1, p0_sel, p1_sel)

  if (return_HR) {
    return(list(fit = fit, resvar = resvar, ic = ic, H = H, R = R))
  } else {
    return(list(fit = fit, resvar = resvar, ic = ic))
  }
}
