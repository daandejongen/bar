#' @export
print.bar <- function(x) {

  n   <- unname(x$n)
  coe <- round(x$coefficients, 2)
  r0  <- round(x$thresholds[1], 2)
  r1  <- round(x$thresholds[2], 2)

  cat(paste0("BAR model fitted on ", n[1], " observations."),
      "\n\n",
      "if R[t] = 0:\n", make_formula(coe[1, , drop = TRUE], x$resvar[1]),
      "\n\n",
      "if R[t] = 1:\n", make_formula(coe[2, , drop = TRUE], x$resvar[2]),
      "\n\n",
      hys_switch(x$delay, r0, r1),
      "\n\n",
      " and e[t] ~ N(0, 1)."
  )
}


#' @export
summary.bar <- function(x) {

  n <- unname(x$n)

  coe <- x$coefficients
  res_stat <- round(quantile(x$residuals, c(0, .25, .5, .75, 1)), 3)
  names(res_stat) <- c("min", "1q", "median", "3q", "max")

  cat(paste0("BAR model fitted on ", n[1], " observations,",
             " of which\n", n[2], " observations in regime 0 and\n",
             n[3], " observations in regime 1.\n")
  )

  cat("\nEstimated thresholds:\n")
  print(round(x$thresholds, 3))

  cat("\nEstimated model coefficients:\n")
  print(round(coe, 3))

  cat("\nEstimated residual variances:\n")
  print(round(x$resvar, 3))

  cat("\nResiduals: \n")
  print(res_stat)

  cat("\nModel aic:\n")
  cat(x$aic)
}


# Helpers

make_formula <- function(coe, rv) {
  coe <- coe[!is.na(coe)]
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





