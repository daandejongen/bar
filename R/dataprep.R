create_design_mat <- function(y, p) {
  n <- length(y)
  x <- rep(1, n)
  for (i in 1:p) cbind(x, y[(p-i+1):(nT-i)])
  return(x)
}

get_r_search <- function(z, thr_bounds, method) {
  method <- arg.match("quantile", "custom")
  thr_bounds <- sort(thr_bounds)
  if (length(thr_bounds)!=2) {
    stop("There must be two values in thr_bounds", call.=FALSE)
  }
  if (method == "quantile") {
    a <- quantile(z, thr_bounds)
  } else a <- thr_bounds
  z <- sort(z[z>a[1] & z<a[2]])
  return(combn(z, 2))
}




