#' @export
#' @importFrom stats coef
coef.bar <- function(object) {
  return(round(object$coefficients, 3))
}


#' @export
#' @importFrom stats residuals
residuals.bar <- function(object) {
  return(object$residuals)
}


#' @export
#' @importFrom stats fitted
fitted.bar <- function(object) {
  return(object$data[["y"]] - object$residuals)
}


#' @export
#' @importFrom stats nobs
nobs.bar <- function(object) {
  return(object$n)
}







