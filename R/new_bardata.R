new_bardata <- function(y, z, H, R, r, n_ineff) {

  out <- structure(
    data.frame(y = y, z = z, H = H, R = R),
    class = "bardata",
    r = r, n_ineff = n_ineff
  )

  return(out)
}
