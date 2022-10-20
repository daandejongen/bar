#' @export
#' @importFrom stats coef
coef.hystar <- function(object) {
  M <- create_coe_matrix(object$coefficients,
                         object$orders[1],
                         object$orders[2])
  return(round(M, 3))
}


#' @export
#' @importFrom stats residuals
residuals.hystar <- function(object) {
  return(object$residuals)
}


#' @export
confint.hystar <- function(object, parm, level = 0.95, ...) {
  if (missing(parm)) parm <- 1:length(object$coefficients)
  coe <- object$coefficients[parm]
  SEs <- compute_SEs(y = object$data$y,
                     R = object$data$R,
                     rv = object$resvar,
                     p0 = object$orders[1],
                     p1 = object$orders[2])[parm]

  alpha <-  1 - level

  CIs <- compute_CIs(coe, SEs, alpha = alpha)

  return(CIs)
}


#' @export
#' @importFrom stats fitted
fitted.hystar <- function(object) {
  return(object$data[["y"]] - object$residuals)
}


#' @export
#' @importFrom stats nobs
nobs.hystar <- function(object) {
  return(object$n)
}







