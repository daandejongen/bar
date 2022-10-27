#' @importFrom utils combn
create_p_options <- function(p0, p1) {
  # We want to have every way to choose a value for p0 from 0, 1, ..., max_p0
  # and a value for p1 from 0, 1, ..., max_p1.
  p0_options <- rep(p0, times = length(p1))
  p1_options <- rep(p1, each  = length(p0))

  p_mat <- matrix(c(p0_options, p1_options), ncol = 2)
  colnames(p_mat) <-  c("p0", "p1")
  drs_mat <- matrix(nrow = nrow(p_mat), ncol = 4)
  colnames(drs_mat) <-  c("d", "r0", "r1", "s")
  ic_mat <- matrix(nrow = nrow(p_mat), ncol = 1)
  colnames(ic_mat) <-  "ic"

  return(cbind(p_mat, drs_mat, ic_mat))
}
