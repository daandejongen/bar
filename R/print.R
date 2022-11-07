#' @export
print.hystar_fit <- function(x) {
  coe <- split_coe(x$coefficients, x$orders[1], x$orders[2])
  print_hystar(n = x$n[1], d = x$delay,
               r0 = x$thresholds[1],
               r1 = x$thresholds[2],
               coe0 = round(coe$coe0, 2),
               coe1 = round(coe$coe1, 2),
               rv0 = round(x$resvar[1], 2),
               rv1 = round(x$resvar[2], 2),
               simfit = "fitted on")
}

#' @export
print.hystar_sim <- function(x) {
  print_hystar(n = nrow(x$data),
               d = x$true_values$d,
               r0 = x$true_values$r[1],
               r1 = x$true_values$r[2],
               coe0 = round(x$true_values$phi[1], 2),
               coe1 = round(x$true_values$phi[1], 2),
               rv0 = round(x$true_values$resvar[1], 2),
               rv1 = round(x$true_values$resvar[2], 2),
               simfit = "that generated")
}

print_hystar <- function(n, d, r0, r1, coe0, coe1, rv0, rv1, simfit) {
  cat(paste0("hysTAR model ", simfit, " ", n, " observations."),
      "\n\n",
      "if R[t] = 0:\n", make_formula(coe0, rv0),
      "\n\n",
      "if R[t] = 1:\n", make_formula(coe1, rv1),
      "\n\n",
      hys_switch(d, r0, r1),
      "\n\n",
      " and e[t] ~ N(0, 1)."
  )
}

#' @export
summary.hystar_fit <- function(x) {
  n <- unname(x$n)
  coe <- create_coe_matrix_SE_p(coe = x$coefficients,
                                y = x$data$y[x$eff],
                                R = x$data$R[x$eff],
                                rv = x$resvar,
                                p0 = x$orders[1],
                                p1 = x$orders[2])
  res_stat <- round(quantile(x$residuals, c(0, .25, .5, .75, 1)), 3)
  names(res_stat) <- c("min", "1q", "median", "3q", "max")

  cat(paste0("hysTAR model fitted on ", n[1], " observations,",
             " of which\n", n[2], " observations in regime 0 and\n",
             n[3], " observations in regime 1.\n")
  )

  cat("\nEstimated thresholds:\n")
  print(round(x$thresholds, 3))

  cat("\nEstimated delay:\n")
  cat(x$delay, "\n")

  cat("\nEstimated model coefficients:\n")
  print(round(coe, 3))

  cat("\nEstimated residual variances:\n")
  print(round(x$resvar, 3))

  cat("\nResiduals: \n")
  print(res_stat)

  cat("\nInformation criteria:\n")
  print(x$ic)
}

#' @export
summary.hystar_sim <- function(x) {

  true <- x$true_values
  n <- c(nrow(x$data), sum(1 - x$data$R), sum(x$data$R))

  cat(paste0("hysTAR model with ", n[1], " simulated observations,",
             " of which\n", n[2], " observations in regime 0 and\n",
             n[3], " observations in regime 1.\n")
  )

  cat("\nTresholds:\n")
  print(true$r)

  cat("\nDelay:\n")
  print(true$d)

  cat("\nCoefficients:\n")
  print(create_coe_matrix(coe = true$phi, p0 = true$orders[1], p1 = true$orders[2]))

  cat("\nResidual variances:\n")
  print(true$resvar)
}

# Helpers
make_formula <- function(coe, rv) {
  p <- length(coe) - 1
  lags_str <- if (p == 0) NULL else paste0("y[t-", 1:p, "]")

  plusmin <- ifelse(coe[-1] > 0, " + ", " - ")
  coe <- abs(coe)

  a <- paste0("y[t] = ", coe[1])
  b <- paste0(plusmin, coe[-1], " ", lags_str, collapse = "")

  paste0(a, b, " + ", round(sqrt(rv), 2), " e[t]")
}

hys_switch <- function(d, r0, r1) {
  ztd <- paste0("z[t-", d, "]")
  paste0("Where:\n",
         "R[t] = 0 \t if ", ztd, " <= ", r0, "\n",
         "R[t] = R[t-1] \t if ", r0, " < ", ztd, " <= ", r1, "\n",
         "R[t] = 1 \t if ", r1, " < ", ztd)
}
