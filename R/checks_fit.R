check_hystar_fit_input <- function(y, z, d, p0, p1, p_select, r, thin) {
  check_yz(y, z)
  check_whole_nn(d)
  check_whole_nn(p0)
  check_whole_nn(p1)
  check_r(r)
  check_thin(thin)
  check_rz(r, z)
  p_select <- check_p_select(p_select)

  return(p_select)
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
    if (ncol(r) != 2)
      stop(paste0("If `r` is a matrix, it must have 2 columns. \n You ",
                  "provided a matrix with ", ncol(r), " columns."),
           call. = FALSE)
    if (! all(r[, 1] <= r[, 2]))
      stop(paste0("The second threshold value should be always larger ",
                  "than or equal to the first."), call. = FALSE)
  }
}

check_thin <- function(thin) {
  if (!(thin %in% c(TRUE, FALSE)))
    stop(paste0("`thin` must be TRUE or FALSE."), call. = FALSE)
}

check_rz <- function(r, z) {
  if (is.vector(r) && !all(0 <= r & r <= 1))
    stop("`r` is a vector, so the values of 'r' must be valid quantiles.",
         call. = FALSE)

  if (matrix(r) && !all(min(z) <= r & r <= max(z)))
    stop(paste0("'r' is a matrix, so the values of 'r' must be in in ",
                "the range of 'z'."), call. = FALSE)
}

check_p_select <- function(p_select) {
  p_select <- tolower(p_select)
  choices <- c("aic", "aicc", "bic")
  p_select <- tryCatch(
    error = function(cond) {
      stop(paste0("'p_select' must be one of these: ",
                  paste0(choices, collapse = ", "), "."),
           call. = FALSE)
    },
    match.arg(
      arg = p_select,
      choices = choices
    )
  )

  return(p_select)
}
