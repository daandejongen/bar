#' @export
#' @importFrom stats coef
coef.hystar_fit <- function(object, ...) {
  M <- create_coe_matrix(object$coefficients,
                         object$orders[1],
                         object$orders[2])
  return(round(M, 3))
}

#' @export
#' @importFrom stats residuals
residuals.hystar_fit <- function(object, standardized = TRUE, ...) {
  return(if (standardized) object$residuals_st else object$residuals)
}

#' @export
confint.hystar_fit <- function(object, parm, level = 0.95, ...) {
  if (missing(parm)) parm <- 1:length(object$coefficients)
  coe <- object$coefficients[parm]
  SEs <- compute_SEs(y = object$data$y[object$eff],
                     R = object$data$R[object$eff],
                     rv = object$resvar,
                     p0 = object$orders[1],
                     p1 = object$orders[2])[parm]

  alpha <- 1 - level

  CIs <- compute_CIs(coe, SEs, alpha = alpha)

  return(round(CIs, 3))
}

#' @export
#' @importFrom stats fitted
fitted.hystar_fit <- function(object, ...) {
  y <- object$data$y[object$eff]
  return(y - object$residuals)
}

#' @export
#' @importFrom stats nobs
nobs.hystar_fit <- function(object, ...) {
  return(object$n)
}

