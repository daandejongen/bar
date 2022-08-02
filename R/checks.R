check_data <- function(y, z) {
  if (!is.numeric(y)) error_numeric(y)
  if (!is.numeric(z)) error_numeric(z)

  if (length(y) != length(z)) {
    stop(paste0("'y' and 'z' must be of equal length:\n",
                "Currently, 'y' has length ", length(y),
                " and 'z' has length ", length(z), "."),
         call. = FALSE)
  }
}

check_dp <- function(d, p0, p1) {
  if (!is.numeric(d))  error_numeric(d)
  if (!is.numeric(p0)) error_numeric(p0)
  if (!is.numeric(p1)) error_numeric(p1)

  if (!all(is_whole(c(p0, p1, d)))) {
    stop("'p0', 'p1' and 'd' should be whole numbers.", call. = FALSE)
  }

}


check_search_r <- function(search, r) {
  if (!is.numeric(r)) error_numeric(r)

  s <- tryCatch(
    error = function(cond) {
      stop("'search' must be one of these: none, quantile, custom, scale",
           call. = FALSE)
    },
    match.arg(
      arg = search,
      choices = c("none", "quantile", "custom", "scale")
    )
  )

  if (s == "custom") {

    if (!is.matrix(r) || ncol(r) != 2) {
      stop("If 'search' is custom, 'r' should be a matrix with two columns.",
           call. = FALSE)
    }
    r0 <- r[, 1, drop = TRUE]
    r0 <- r[, 2, drop = TRUE]

  } else {

    if (length(r) != 2) {
      stop(paste0("You must provide exactly two values for r.\n
                You specified ", length(r), " values."), call. = FALSE)
    }
    r0 <- r[1]
    r1 <- r[2]

  }

  if (! all(r0 <= r1)) {
    stop(paste0("The first threshold value should (always) ",
                "be smaller or equal than the second one."), call. = FALSE)
  }

  return(s)
}


check_r_select <- function(r_select) {
  r_select <- tryCatch(
    error = function(cond) {
      stop("'r_select' must be one of these: widest, smallest",
           call. = FALSE)
    },
    match.arg(
      arg = r_select,
      choices = c("widest", "smallest")
    )
  )
}


check_sim_input <- function(z, d, r, phi, psi, resvar, init_vals,
                            start_regime) {

  if (!is.numeric(z))         error_numeric(z)
  if (!is.numeric(phi))       error_numeric(phi)
  if (!is.numeric(psi))       error_numeric(psi)
  if (!is.numeric(resvar))    error_numeric(resvar)
  if (!is.numeric(r))         error_numeric(r)
  if (!is.numeric(init_vals) && !is.null(init_vals)) error_numeric(init_vals)

  if (r[1] > r[2]) {
    stop(paste0("The first threshold value should (always) ",
                "be smaller or equal than the second one."), call. = FALSE)
  }

  if (!all(c(phi[-1], psi[-1]) > -1 & c(phi[-1], psi[-1]) < 1)) {
    stop("Autoregressive coefficients in 'phi' and 'psi' must
         be between -1 and 1.", call. = FALSE)
  }

  if (resvar[1] < 0 || resvar[2] < 0) {
    stop("Residual variances in 'resvar' cannot be negative.",
         call. = FALSE)
  }

  p <- max(get_order(phi), get_order(psi), d)

  if (!is.null(init_vals)) {
    if (p < length(init_vals)) {
      stop("Too many initial values provided in 'init_vals'.", call. = FALSE)
    }
    if (p > length(init_vals)) {
      print(c(p, init_vals))
      stop("Too few initial values provided in 'init_vals'.", call. = FALSE)
    }
  }

  if (!is_whole(d)) {
    stop("'d' must be a whole number.", call. = FALSE)
  }

  start_unknown <- r[1] < z[p+1-d] && r[2] > z[p+1-d]
  if (start_unknown && is.null(start_regime)) {
    stop("A starting regime is needed, but not provided.\n
         The first value of 'z' (delayed)
         falls in the hysteresis zone (r[1], r[2]).",
         call. = FALSE)
  }
  if (start_unknown && ! start_regime %in% c(0, 1)) {
    stop("The starting regime 'start_regime' must be 0 or 1.", call. = FALSE)
  }
}

# Helpers

is_whole <- function(x, tol = .Machine$double.eps) {
  whole <- all(abs(round(x) - x) < tol)
  return(whole)
}


error_numeric <- function(x) {
  err <- structure(
    list(
      message = paste0("'", substitute(x), "' should be numeric, ",
                       "you provided an object of type ",
                       typeof(x), "."),
      call = NULL
    ),
    class = c("not_numeric", "error", "condition")
  )
  stop(err)
}
