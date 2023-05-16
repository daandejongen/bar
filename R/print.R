#' @export
print.hystar_fit <- function(x, ...) {
  coe <- split_coe(x$coefficients, x$orders[1], x$orders[2])
  print_hystar(n = x$n[1], d = x$delay,
               r0 = x$thresholds[1],
               r1 = x$thresholds[2],
               coe0 = round(coe$coe0, 2),
               coe1 = round(coe$coe1, 2),
               rv0 = round(x$resvar[1], 2),
               rv1 = round(x$resvar[2], 2),
               simfit = "fitted on",
               tar = x$tar)

  invisible()
}

#' @export
print.hystar_sim <- function(x, ...) {
  coe <- split_coe(x$phi,
                   x$orders[1],
                   x$orders[2])
  print_hystar(n = nrow(x$data),
               d = x$d,
               r0 = x$r[1],
               r1 = x$r[2],
               coe0 = round(coe$coe0, 2),
               coe1 = round(coe$coe1, 2),
               rv0 = round(x$resvar[1], 2),
               rv1 = round(x$resvar[2], 2),
               simfit = "that generated",
               tar = x$tar)

  invisible()
}

print_hystar <- function(n, d, r0, r1, coe0, coe1, rv0, rv1, simfit, tar) {
  model <- if (tar) "TAR model " else "HysTAR model "
  cat(paste0(model, simfit, " ", n, " observations."),
      "\n\n",
      "if R[t] = 0:\n", make_formula(coe0, rv0),
      "\n\n",
      "if R[t] = 1:\n", make_formula(coe1, rv1),
      "\n\n",
      hys_switch(d, r0, r1),
      "\n\n",
      " and e[t] ~ N(0, 1).\n"
  )

  invisible()
}

#' @export
#' @importFrom stats quantile
summary.hystar_fit <- function(object, ...) {
  n <- unname(object$n)
  coe <- create_coe_matrix_SE_p(coe = object$coefficients,
                                y = object$data$y[object$eff],
                                R = object$data$R[object$eff],
                                rv = object$resvar,
                                p0 = object$orders[1],
                                p1 = object$orders[2])
  res_stat <- round(quantile(object$residuals, c(0, .25, .5, .75, 1)), 3)
  names(res_stat) <- c("min", "1q", "median", "3q", "max")

  model <- if (object$tar) "TAR model " else "HysTAR model "
  cat(paste0(model, "fitted on ", n[1], " observations,",
             " of which\n", n[2], " observations in regime 0 and\n",
             n[3], " observations in regime 1.\n")
  )

  cat("\nEstimated thresholds:\n")
  print(round(object$thresholds, 3))

  cat("\nEstimated delay:\n")
  cat(object$delay, "\n")

  cat("\nEstimated model coefficients:\n")
  print(round(coe, 3))

  cat("\nEstimated residual variances:\n")
  print(round(object$resvar, 3))

  cat("\nResiduals: \n")
  print(res_stat)

  cat("\nInformation criteria:\n")
  print(object$ic)

  cat("\n")

  invisible()
}

#' @export
summary.hystar_sim <- function(object, ...) {
  n <- c(nrow(object$data), sum(1 - object$data$R), sum(object$data$R))

  cat(paste0("hysTAR model with ", n[1], " simulated observations,",
             " of which\n", n[2], " observations in regime 0 and\n",
             n[3], " observations in regime 1.\n")
  )

  cat("\nTresholds:\n")
  print(object$r)

  cat("\nDelay:\n")
  print(object$d)

  cat("\nCoefficients:\n")
  print(create_coe_matrix(coe = object$phi, p0 = object$orders[1], p1 = object$orders[2]))

  cat("\nResidual variances:\n")
  print(object$resvar)

  cat("\n")

  invisible()
}

# Helpers
make_formula <- function(coe, rv) {
  p <- get_order(coe)

  string_intercept <- get_string_intercept(coe[1])
  string_lags <- get_string_lags(coe[-1], int_0 = coe[1] == 0)
  string_res <- get_string_res(rv, all_zero = all(coe == 0))

  paste0(string_intercept, string_lags, string_res)
}

get_string_intercept <- function(int) {
  valence <- if (int < 0) "- " else ""
  string_value <- if (int != 0) paste0(abs(int), " ") else ""

  return(paste0(valence, string_value))
}

get_string_lags <- function(coe, int_0) {
  show <- coe != 0
  if (sum(show) == 0) {
    return()
  }

  valences <- ifelse(coe < 0, "- ", "+ ")[show]
  if (int_0 && valences[1] == "+ ") valences[1] <- ""

  values <- ifelse(abs(coe) == 1, "", paste0(abs(coe), " "))[show]
  ys <- paste0("y[t-", 1:length(coe), "] ")[show]

  return(paste0(valences, values, ys, collapse = ""))
}

get_string_res <- function(rv, all_zero) {
  valence <- if (!all_zero) "+ " else ""
  s <- if (rv == 1) paste0(valence, "e[t]") else paste0(valence, rv, " e[t]")
}


hys_switch <- function(d, r0, r1) {
  ztd <- paste0("z[t-", d, "]")
  paste0("Where:\n",
         "R[t] = 0 \t if ", ztd, " <= ", r0, "\n",
         "R[t] = R[t-1] \t if ", r0, " < ", ztd, " <= ", r1, "\n",
         "R[t] = 1 \t if ", r1, " < ", ztd)
}
