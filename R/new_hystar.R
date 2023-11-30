new_hystar_fit <- function(y, x, z, eff, est, model, equiv, tar, r_search, call) {

  coe <- model$fit$coe
  names(coe) <- c(paste0("phi0", 0:est["p0"]), paste0("phi1", 0:est["p1"]))

  n <- c(length(eff), sum(1 - model$R), sum(model$R))
  names(n) <- c("n_used", "n_regime0", "n_regime1")

  sigma_time_series <- ifelse(model$R == 1, model$resvar[2], model$resvar[1])
  standardized_residuals <- model$fit$res / sigma_time_series

  NA_k <- rep(NA, times = length(y) - length(eff))
  data <- data.frame(y = y, z = z,
                     H = c(NA_k, model$H == -1),
                     R = c(NA_k, model$R))

  hystar <- structure(
    list(data         = data,
         residuals    = model$fit$res,
         residuals_st = standardized_residuals,
         coefficients = coe,
         st_errors    = model$SEs,
         delay        = est["d"],
         thresholds   = est[c("r0", "r1")],
         orders       = est[c("p0", "p1")],
         resvar       = model$resvar,
         rss          = model$fit$rss,
         ic           = model$ic,
         n            = n,
         eff          = eff,
         equiv_pars   = equiv,
         r_search     = r_search,
         tar          = tar,
         call         = call
         ),
    class = "hystar_fit"
    )

  return(hystar)
}

new_hystar_sim <- function(y, z, H, R, phi_R0, phi_R1, r, d, resvar, k, call) {
  phi <- c(phi_R0, phi_R1)
  p0 <- get_order(phi_R0)
  p1 <- get_order(phi_R1)
  names(phi) <- c(paste0("phi_R0_", 0:p0), paste0("phi_R1_", 0:p1))

  data <- data.frame(y = y, z = z, H = c(rep(NA, k), H == -1L), R = R)

  out <- structure(
    list(data = data,
         thresholds = r,
         d = d,
         phi = phi,
         orders = c(get_order(phi_R0), get_order(phi_R1)),
         resvar = resvar,
         tar = r[1] == r[2],
         call = call),
    class = "hystar_sim"
  )

  return(out)
}




