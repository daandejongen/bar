check_all <- function(y, z, d, p0, p1, r) {
  check_data(y, z)
  check_dp(d, p0, p1)
  check_r(r)
}

check_data <- function(y, z) {
  if (!is.numeric(y)) error_numeric(y)
  if (!is.numeric(z)) error_numeric(z)

  if (length(y) != length(z)) {
    stop(paste0("y and z must be of equal length:\n",
                "Currently, y is of length ", length(y),
                " and z is of length ", length(z), "."),
         call.=FALSE
         )
  }
}

check_dp <- function(d, p0, p1) {
  if (!is.numeric(d)) error_numeric(d)
  if (!is.numeric(p0)) error_numeric(p0)
  if (!is.numeric(p1)) error_numeric(p1)

  if (d > max(p0, p1)) {
    stop("The delay `d` cannot exceed the largest order.",
         call.=FALSE)
  }

  if (!all(is_whole(c(p0, p1, d)))) {
    stop("`p0`, `p1` and `d` should be whole numbers.",
         call.=FALSE)
  }
}

check_r <- function(r, z, method) {
  if (!is.numeric(r)) error_numeric(r)

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

check_search <- function(search) {
  if (!is.character(search)) {
    stop(paste0("search should be an object of type character,\n",
                "you provided an object of type ", type(search)),
         call.=FALSE
    )
  }

  s <- tryCatch(
    error = function() {
      stop("search must be one of these: none, quantile, custom.",
           call.=FALSE)
    },
    match.arg(
      arg=search,
      choices=c("none", "quantile", "custom")
    )
  )

  return(s)
}


is_whole <- function(x, tol=.Machine$double.eps) {
  whole <- all(abs(round(x) - x) < tol)
  return(whole)
}

error_numeric <- function(x) {
  stop(paste0(substitute(x), " should be numeric, ",
              "you provided an object of type ",
              typeof(x), "."),
       call.=FALSE
  )
}
