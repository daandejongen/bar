check_data <- function(y, z){
  error_numeric(y)
  error_numeric(z)

  if (length(y) != length(z)) {
    stop("`y` and z` must be of equal length",
         call.=FALSE)
  }
}

check_dp <- function(d, p) {
  error_numeric(d)
  error_numeric(p)

  if (length(p) != 2) {
    stop(paste0("You must provide exactly two orders, ",
                "one for each regime. \n",
                "You specified ",
                length(p),
                " orders.")
    )
  }

  if (d > max(p)) {
    stop("The delay `d` cannot exceed the largest order.",
         call.=FALSE)
  }

  if (!all(is_whole(c(p0, p1, d)))) {
    stop("`p0`, `p1` and `d` should be whole numbers.",
         call.=FALSE)
  }
}

check_r <- function(r, z, method) {
  error_numeric(r)
  method <- arg.match("quantile",
                      "custom",
                      "none")

  if (length(r) != 2) {
    stop(paste0("You must provide exactly two values for r.\n",
                "You specified ", length(r), " values."),
         call.=FALSE
    )
  }

  if (r[1] > r[2]) {
    stop(paste0("The first threshold value should be larger ",
                "than the second one."),
         call.=FALSE
    )
  }

  if (method %in% c("none", "custom")) {
    if (r[1]<min(z) || r[2]>max(z)) {
      stop("Threshold values must fall in the range of z.",
           call.=FALSE)
    }
  }
}

is_whole <- function(x, tol=.Machine$double.eps) {
  return(abs(round(x) - x) < tol)
}

error_numeric <- function(x) {
  if (!is.numeric(x)) {
    stop(paste0(quote(x),
                " should be numeric, ",
                "you provided an object of type ",
                typeof(x), "."),
         call.=FALSE
    )
  }
}
