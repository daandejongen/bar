#' @export
#' @importFrom stats coef
coef.bar <- function(x) {
  return(round(x$coefficients), 3)
}


#' @export
#' @importFrom stats residuals
residuals.bar <- function(x) {
  return(x$residuals)
}


#' @export
#' @importFrom stats fitted
fitted.bar <- function(x) {
  return(x$data[["y"]] - x$residuals)
}


#' @export
#' @importFrom stats nobs
nobs.bar <- function(x) {
  return(x$n)
}







