#' Estimating a buffered autoregressive model with Least Squares estimation.
#'
#' @param y A numeric vector representing the outcome time series.
#' @param z A numeric vector representing the threshold time series.
#' @param d A vector with one or more values for the delay parameter.
#' @param p0 A vector with one or more values for the order of regime 0.
#' @param p1 A vector with one or more values for the order of regime 1.
#' @param r A numeric vector of length 2 with (boundary) threshold values
#' @param search
#'
#' @return An object of class bar.
#' @export
#'
#' @examples
#' a <- 1
bar <- function(y, z=y, d, p, r, search) {
  check_all(y, z, d, p0, p1, r) #only checks with errors
  search <- check_search(search) #checks AND returns match.arg()

  p_max <- max(p0, p1)
  y_eff <- y[(p+1):length(y)]
  n_eff <- length(y_eff)

  x <- create_x(y, p_max)
  g <- matrix(r, nrow=1, ncol=2)
  g <- if (search=="none") g else create_grid(z, r, search)
}
