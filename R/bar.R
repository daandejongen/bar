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
bar <- function(y, z=y, d, p0, p1, r, search) {
  # Check AND return match.arg() for `search`
  search <- check_search(search)
  # Checks for correct arguments that throw an error
  check_data(y, z)
  check_dp(d, p0, p1)
  check_r(r)

  # Some data preparation
  p_max <- max(p0, p1)
  y_eff <- y[(p+1):length(y)]
  n_eff <- length(y_eff)
  x <- create_x(y, p_max)
  g <- if (search=="none") matrix(r, ncol=2) else create_grid(z, r, search)



}
