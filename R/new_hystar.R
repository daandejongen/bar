new_hystar_fit <- function(y, x, z, eff, est, model, equiv, tar, r_search) {

  coe <- model$fit$coe
  names(coe) <- c(paste0("R0_phi", 0:est["p0"]), paste0("R1_phi", 0:est["p1"]))

  n <- c(length(eff), sum(1 - model$R), sum(model$R))
  names(n) <- c("used", paste0("regime", 0:1))

  sig_time_series <- ifelse(model$R == 1, model$resvar[2], model$resvar[1])
  standardized_residuals <- model$fit$res / sig_time_series

  NA_k <- rep(NA, times = length(y) - length(eff))
  data <- data.frame(y = y, z = z,
                     H = c(NA_k, model$H == -1),
                     R = c(NA_k, model$R))

  hystar <- structure(
    list(data         = data,
         residuals    = model$fit$res,
         residuals_st = standardized_residuals,
         coefficients = coe,
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
         tar          = tar
         ),
    class = "hystar_fit"
    )

  return(hystar)
}

new_hystar_sim <- function(y, z, H, R, phi_R0, phi_R1, r, d, resvar, k) {
  phi <- c(phi_R0, phi_R1)
  p0 <- get_order(phi_R0)
  p1 <- get_order(phi_R1)
  names(phi) <- c(paste0("phi_R0_", 0:p0), paste0("phi_R1_", 0:p1))

  data <- data.frame(y = y, z = z, H = c(rep(NA, k), H == -1L), R = R)

  out <- structure(
    list(data = data,
         r = r,
         d = d,
         phi = phi,
         orders = c(get_order(phi_R0), get_order(phi_R1)),
         resvar = resvar,
         tar = r[1] == r[2]),
    class = "hystar_sim"
  )

  return(out)
}




