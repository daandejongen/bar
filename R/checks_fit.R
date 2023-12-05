check_hystar_fit_input <- function(z, d, p0, p1, p_select, r, thin, tar, show_progress) {
  check_d(d)
  check_p0(p0)
  check_p1(p1)
  check_r_fit(r, tar)
  check_thin(thin)
  check_tar(tar)
  check_rz(r, z)
  check_show_progress(show_progress)
  # p_select uses match.arg so the user can abbreviate,
  # so we want to return that value.
  p_select <- check_p_select(p_select)

  return(p_select)
}

check_data <- function(data) {
  if (missing(data))
    stop(paste0("Argument `data` is missing, with no default."), call. = FALSE)

  if (is.vector(data)) {
    data <- matrix(rep(data, times = 2), ncol = 2, byrow = FALSE)
  }

  if (is.data.frame(data)) {
    data <- as.matrix(data)
  }

  if (!is.matrix(data))
    stop(paste0("`data` should be a vector, matrix or data.frame. ",
                "You provided an object of class ", class(data)),
         call. = FALSE)

  data <- data[, c(1, 2), drop = FALSE]

  if (!is.numeric(data)) error_numeric(data)

  if (any(is.na(data)))
    stop(paste0("`data` cannot have missing values."))

  n_unique <- length(unique(data[, 2]))
  if (n_unique < 3)
    stop(paste0("There are fewer than 3 unique values of ",
                "the control variable, namely: ", n_unique), call. = FALSE)
}

check_p0 <- function(p0) {
  if (!is.numeric(p0)) error_numeric(p0)
  if (!is_whole(p0))   error_whole(p0)
  if (!all(p0 >= 0))   error_nonnegative(p0)
}

check_p1 <- function(p1) {
  if (!is.numeric(p1)) error_numeric(p1)
  if (!is_whole(p1))   error_whole(p1)
  if (!all(p1 >= 0))   error_nonnegative(p1)
}

check_r_fit <- function(r, tar) {
  if (!is.numeric(r)) error_numeric(r)

  if (is.vector(r)) {
    if (length(r) != 2)
      stop(paste0("If `r` is a vector, its length must be 2. You provided ",
                  "a vector of length ", length(r), "."), call. = FALSE)
    if (r[1] >= r[2])
      stop(paste0("If `r` is a vector, it must represent an interval.\n",
                  "But, the second value of `r` is smaller than the first."),
           call. = FALSE)
  }

  if (is.matrix(r)) {
    if (ncol(r) != 2)
      stop(paste0("If `r` is a matrix, it must have 2 columns. \n You ",
                  "provided a matrix with ", ncol(r), " columns."),
           call. = FALSE)
    if (!all(r[, 1] <= r[, 2]))
      stop(paste0("The second threshold value should be always larger ",
                  "than or equal to the first."), call. = FALSE)
    if (tar && !all(r[, 1] == r[, 2]))
      stop(paste0("You want to fit a TAR model, but the threshold matrix you",
                  " provided has unequal threshold values. That is, for some row(s),",
                  " r_0 is not r_1. Note that r_0 = r_1 in the TAR model, by definition."),
           call. = FALSE)
    if (!tar && all(r[, 1] == r[, 2]))
      warning(paste0("You provided a matrix for the threshold search in which r_0 = r_1 ",
                     "in every row. Note that this is equivalent to fitting a TAR model, ",
                     "but that you set `tar = FALSE` in the hystar_fit function call."))
  }
}

check_thin <- function(thin) {
  if (!(thin %in% c(TRUE, FALSE))) error_logical(thin)
}

check_tar <- function(tar) {
  if (!(tar %in% c(TRUE, FALSE))) error_logical(tar)
}

check_show_progress <- function(show_progress) {
  if (!(show_progress %in% c(TRUE, FALSE))) error_logical(show_progress)
}

check_tar <- function(tar) {
  if (!(tar %in% c(TRUE, FALSE)))
    stop("`tar` must be TRUE or FALSE.", call. = FALSE)
}

check_rz <- function(r, z) {
  if (is.vector(r) && !all(0 <= r & r <= 1))
    stop("`r` is a vector, so the values of `r` must be valid quantiles.",
         call. = FALSE)

  if (is.matrix(r) && (!all(min(z) <= r & r <= max(z))))
    stop(paste0("`r` is a matrix, so the values of `r` must be in in ",
                "the range of `z`."), call. = FALSE)
}

check_p_select <- function(p_select) {
  if (length(p_select) > 1) p_select <- p_select[1]
  if (!is.character(p_select))
    stop(paste0("`p_select` must be of type character."),
         call. = FALSE)
  p_select <- tolower(p_select)
  choices <- c("aic", "aicc", "bic", "aiccp")
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
