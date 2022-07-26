#' Estimating a buffered autoregressive model with Least Squares estimation.
#'
#' @param y A numeric vector representing the outcome time series.
#' @param z A numeric vector representing the threshold time series.
#' @param d A vector with one or more values for the delay parameter.
#' @param p0 A vector with one or more values for the order of regime 0.
#' @param p1 A vector with one or more values for the order of regime 1.
#' @param r A numeric vector of length 2 with (boundary) threshold values
#' @param search What type of boundary values of r are specified to find
#' optimal threshold values: "quantile" for quantile values, "scale" for
#' values on the scale of z, "none" if r is fixed
#'
#'
#' @return An object of class bar.
#' @export
#'
#' @examples
#' a <- 1
barfit <- function(y, z = y, d = 1, p0 = 1, p1 = 1,
                   r = c(.1, .9), search = "quantile") {
  # Check AND return match.arg() for `search`
  search <- check_search(search)
  # Checks for correct arguments that throw an error
  check_data(y, z)
  check_dp(d, p0, p1)
  check_r(r)

  p <- max(p0, p1)
  n <- length(y)
  s <- select_obs(n, d_max = max(d), p)
  y_eff <- y[s$eff]
  x <- create_x(y = y, eff = s$eff, p = p)

  grid <- create_grid(z = z, d = d, r_bounds = r, search = search)
  delays <-


}
