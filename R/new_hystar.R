new_hystar_fit <- function(y, x, z, eff, est, model, equiv) {

  coe <- model$fit$coe
  names(coe) <- c(paste0("R0_phi", 0:est["p0"]), paste0("R1_phi", 0:est["p1"]))

  n <- c(length(eff), sum(1 - model$R), sum(model$R))
  names(n) <- c("used", paste0("regime", 0:1))

  NA_k <- rep(NA, times = length(y) - length(eff))
  data <- data.frame(y = y, z = z,
                     H = c(NA_k, model$H == -1),
                     R = c(NA_k, model$R))

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
         eff          = eff,
         equiv_pars   = equiv
         ),
    class = "hystar_fit"
    )

  return(hystar)
}

