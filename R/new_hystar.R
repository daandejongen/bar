new_hystar <- function(y, x, z, eff, est, model, equiv) {

  coe <- model$fit$coe
  names(coe) <- c(paste0("R0_phi", 0:est["p0"]), paste0("R1_phi", 0:est["p1"]))

  n <- c(length(eff), sum(1 - model$R), sum(model$R))
  names(n) <- c("used", paste0("regime", 0:1))

  NA_k <- rep(NA, times = length(y) - length(eff))
  data <- new_hystar_data(y = y, z = z,
                          H = c(NA_k, model$H == -1),
                          R = c(NA_k, model$R),
                          r = est[c("r0", "r1")], n_ineff = length(y) - length(eff))

  hystar <- structure(
    list(data         = data,
         residuals    = model$fit$res,
         coefficients = coe,
         delay        = est["d"],
         thresholds   = est[c("r0", "r1")],
         orders       = est[c("p0", "p1")],
         resvar       = model$resvar,
         rss          = model$fit$rss,
         ic           = model$ic,
         n            = n,
         equiv_pars   = equiv
         ),
    class = "hystar"
    )

  return(hystar)
}


new_hystar_data <- function(y, z, H, R, r, n_ineff) {
  out <- structure(
    data.frame(y = y, z = z, H = H, R = R),
    class = c("hystar_data", "data.frame"),
    r = r, n_ineff = n_ineff
  )

  return(out)
}








