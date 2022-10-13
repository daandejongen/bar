#' @importFrom utils combn
p_options <- function(max_p0, max_p1) {
  # We want to have every way to choose a value for p0 from 0, 1, ..., max_p0
  # and a value for p1 from 0, 1, ..., max_p1.
  p0_options <- rep(0:max_p0, times = max_p1 + 1)
  p1_options <- rep(0:max_p1, each  = max_p0 + 1)

  return(matrix(c(p0_options, p1_options), ncol = 2))
}

add_ic_col <- function(options_mat) {
  ic <- matrix(ncol = 1, nrow = nrow(options_mat))

  return(cbind(options_mat, ic))
}

