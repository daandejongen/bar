check_hystar_fit_input <- function(y, z, d, p0, p1,
                                   r, r_type, r_select,
                                   ic_method) {
  r_type   <- check_r_type(r_type)
  r_select <- check_r_select(r_select)
  check_yz(y, z)
  check_whole_nn(d)
  check_whole_nn(p0)
  check_whole_nn(p1)
  check_r(r)
  check_rz(r, r_type, z)
  ic_method <- check_ic_method(ic_method)

  return(c(r_type, r_select, ic_method))
}

check_yz <- function(y, z) {
  if (!is.numeric(y)) error_numeric(y)
  if (!is.numeric(z)) error_numeric(z)

  if (length(y) != length(z)) {
    stop(paste0("`y` and `z` must be of equal length.\n",
                "Currently, `y` has length ", length(y),
                " and `z` has length ", length(z), "."),
         call. = FALSE)
  }
}

check_r <- function(r) {
  if (!is.numeric(r)) error_numeric(r)

  if (is.vector(r)) {
    if (length(r) != 2) {
      stop(paste0("If `r` is a vector, its length must be 2. You provided ",
                  "a vector of length ", length(r), "."),
           call. = FALSE)
    }
    if (r[1] >= r[2]) {
      stop(paste0("If `r` is a vector, it must represent an interval. ",
                  "However, the second value is now smaller than the first."),
           call. = FALSE)
    }
  }

  if (is.matrix(r)) {
    if (ncol(r) != 2) {
      stop(paste0("If `r` is a matrix, it must have 2 columns. \n You ",
                  "provided a matrix with ", ncol(r), " columns."),
           call. = FALSE)
    }
    if (! all(r[, 1] <= r[, 2])) {
      stop(paste0("The second threshold value should be always larger ",
                  "than or equal to the first."), call. = FALSE)
    }
  }
}

check_r_type <- function(r_type) {
  choices <- c("quantile", "scale")
  r_type <- tryCatch(
    error = function(cond) {
      stop(paste0("`r_type` must be one of these: ",
                  paste0(choices, collapse = ", "), "."),
           call. = FALSE)
    },
    # The match.arg() function is used to give users the option to
    # abbreviate the argument.
    match.arg(
      arg = r_type,
      choices = choices
    )
  )
  return(r_type)
}

check_rz <- function(r, r_type, z) {
  if (r_type == "quantile" && !all(0 <= r & r <= 1)) {
    stop("`r_type` is quantile, so the values of 'r' must be in [0, 1].",
         call. = FALSE)
  }

  if (r_type == "scale" && !all(min(z) <= r & r <= max(z))) {
    stop(paste0("'r_type' is scale, so the values of 'r' must be in in ",
                "the range of 'z'."), call. = FALSE)
  }
}

check_r_select <- function(r_select) {
  choices <- c("widest", "smallest")
  r_select <- tryCatch(
    error = function(cond) {
      stop(paste0("'r_select' must be one of these: ",
                  paste0(choices, collapse = ", "), "."),
           call. = FALSE)
    },
    match.arg(
      arg = r_select,
      choices = choices
    )
  )

  return(r_select)
}

check_ic_method <- function(ic_method) {
  choices <- c("aic", "aicc", "bic")
  ic_method <- tryCatch(
    error = function(cond) {
      stop(paste0("'ic_method' must be one of these: ",
                  paste0(choices, collapse = ", "), "."),
           call. = FALSE)
    },
    match.arg(
      arg = ic_method,
      choices = choices
    )
  )

  return(ic_method)
}
