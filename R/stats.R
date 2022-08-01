#' @export
#' @importFrom stats coef
coef.bar <- function(x) {
  return(round(attr(x, "coe"), 3))
}


#' @export
#' @importFrom stats residuals
residuals.bar <- function(x) {
  return(attr(x, "res"))
}


#' @export
#' @importFrom stats fitted
fitted.bar <- function(x) {
  return(attr(x, "y_eff") - attr(x, "res"))
}


#' @export
#' @importFrom stats nobs
nobs.bar <- function(x) {
  return(attr(x, "n"))
}







