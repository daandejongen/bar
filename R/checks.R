check_data <- function(y, z) {
  if (!is.numeric(y)) error_numeric(y)
  if (!is.numeric(z)) error_numeric(z)

  if (length(y) != length(z)) {
    stop(paste0("y and z must be of equal length:\n
                Currently, y is of length ", length(y),
                " and z is of length ", length(z), "."),
         call.=FALSE
         )
  }
}

check_dp <- function(d, p0, p1) {
  if (!is.numeric(d))  error_numeric(d)
  if (!is.numeric(p0)) error_numeric(p0)
  if (!is.numeric(p1)) error_numeric(p1)

  if (!all(is_whole(c(p0, p1, d)))) {
    stop("`p0`, `p1` and `d` should be whole numbers.", call.=FALSE)
  }

  if (d > max(p0, p1)) {
    stop("The delay `d` cannot exceed the largest order.", call.=FALSE)
  }
}

check_r <- function(r) {
  if (!is.numeric(r)) error_numeric(r)

  if (length(r) != 2) {
    stop(paste0("You must provide exactly two values for r.\n
                You specified ", length(r), " values."), call.=FALSE)
  }

  if (r[1] > r[2]) {
    stop(paste0("The first threshold value should be smaller or equal
                than the second one."), call.=FALSE)
  }
}

check_search <- function(search) {
  if (!is.character(search)) {
    stop(paste0("search should be an object of type character,\n
                you provided an object of type ", typeof(search)
                ),
         call.=FALSE
         )
  }

  s <- tryCatch(
    error = function(cond) {
      stop("search must be one of these: none, quantile, scale",
           call.=FALSE
           )
    },
    match.arg(
      arg=search,
      choices=c("none", "quantile", "scale")
    )
  )

  return(s)
}

# Helpers

is_whole <- function(x, tol=.Machine$double.eps) {
  whole <- all(abs(round(x) - x) < tol)
  return(whole)
}

error_numeric <- function(x) {
  err <- structure(
    list(
      message = paste0(substitute(x), " should be numeric, ",
                       "you provided an object of type ",
                       typeof(x), "."),
      call = NULL
    ),
    class = c("not_numeric", "error", "condition")
  )
  stop(err)
}
