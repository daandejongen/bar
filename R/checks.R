check_hystar_fit_input <- function(y, z, d, p0, p1, r, r_type, r_select) {
  r_type   <- check_r_type(r_type)
  r_select <- check_r_select(r_select)
  check_yz(y, z)
  check_whole_nn(d)
  check_whole_nn(p0)
  check_whole_nn(p1)
  check_r(r)
  check_rz(r, r_type, z)

  return(c(r_type, r_select))
}

check_hystar_sim_input <- function(r, d, phi_R0, phi_R1, resvar, init_vals,
                               z, n_t, n_switches, self_exciting, start_regime) {
  check_r(r)
  check_whole_nn(d)
  check_phi(phi_R0, R01 = 0)
  check_phi(phi_R1, R01 = 1)
  check_resvar(resvar)
  check_init_vals(init_vals, phi_R0, phi_R1, d)
  check_start_regime(start_regime)

  # With respect to z, n_t, n_switches, self_exciting, start_regime, the user
  # has three options:
  # 1. provide z;
  # 2. z = NULL. And provide n_t, self_exciting and start_regime;
  # 3. z = NULL. And provide n_t, n_switches and start_regime.

  if (is.null(z)) {
    check_whole_nn(n_t)
    check_n_t_switches(n_t, n_switches, r)
  }

  if (!is.null(z)) {
    check_z_compatible(z, n_t, n_switches)
    check_z(z, r, phi_R0, phi_R1, d, start_regime)
  }
}

check_yz <- function(y, z) {
  if (!is.numeric(y)) error_numeric(y)
  if (!is.numeric(z)) error_numeric(z)

  if (length(y) != length(z)) {
    stop(paste0("'y' and 'z' must be of equal length.\n",
                "Currently, 'y' has length ", length(y),
                " and 'z' has length ", length(z), "."),
         call. = FALSE)
  }
}

check_whole_nn <- function(x) {
  if (!is.numeric(x)) error_numeric(x)
  if (!is_whole(x))   error_whole(x)
  if (x < 0)          error_nonnegative(x)
}

check_r <- function(r) {
  if (!is.numeric(r)) error_numeric(r)

  if (is.vector(r)) {
    if (length(r) != 2) {
      stop(paste0("If 'r' is a vector, its length must be 2. You provided ",
                  "a vector of length ", length(r), "."),
           call. = FALSE)
    }
    if (r[1] >= r[2]) {
      stop(paste0("If 'r' is a vector, it must represent an interval. ",
                  "However, the second value is now smaller than the first."),
           call. = FALSE)
    }
  }

  if (is.matrix(r)) {
    if (ncol(r) != 2) {
      stop(paste0("If 'r' is a matrix, it must have 2 columns. \n You ",
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
  r_type <- tryCatch(
    error = function(cond) {
      stop("'r_type' must be 'quantile' or 'scale'",
           call. = FALSE)
    },
    # The match.arg() function is used to give users the option to
    # abbreviate the argument.
    match.arg(
      arg = r_type,
      choices = c("quantile", "scale")
    )
  )
  return(r_type)
}

check_rz <- function(r, r_type, z) {
  if (r_type == "quantile" && !all(0 <= r & r <= 1)) {
    stop("'r_type' is quantile, so the values of 'r' must be in [0, 1].",
         call. = FALSE)
  }

  if (r_type == "scale" && !all(min(z) <= r & r <= max(z))) {
    stop(paste0("'r_type' is scale, so the values of 'r' must be in in ",
                "the range of 'z'."), call. = FALSE)
  }
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

check_resvar <- function(resvar) {
  if (!is.numeric(resvar)) error_numeric(resvar)
  if (length(resvar) != 2) {
    stop(paste0("You must provide exactly one residual variance for ",
                "each regime.\nYou provided ", length(resvar), " values."),
         call. = FALSE)
  }
  if (!all(resvar > 0)) {
    stop("Values in 'resvar' should be postitive.", call. = FALSE)
  }
}

check_phi <- function(phi, R01) {
  if (!is.numeric(phi)) error_numeric(phi)
  S <- sum(phi[-1])
  if (S >= 1) {
    if (S == 1) {
      warning(paste0("The AR process in regime ", R01, " is non-stationary,\n",
                     "because it has a unit root."), call. = FALSE)
    }
    if (S > 1) {
      warning(paste0("The AR process in regime ", R01, " is non-stationary,\n",
                     "because it has an explosive root."), call. = FALSE)
    }
  }
}

check_init_vals <- function(init_vals, phi_R0, phi_R1, d) {
  if (is.null(init_vals)) return()
  if (!is.numeric(init_vals)) error_numeric(init_vals)
  a <- max(c(get_order(phi_R0), get_order(phi_R1), d))
  if (a != length(init_vals)) {
    stop(paste0("Too few or many initial values provided in 'init_vals'.\n",
                "since max{p0, p1, d} = ", a, "values were needed but ",
                length(init_vals), " were given."), call. = FALSE)
  }
}

check_start_regime <- function(start_regime) {
  if (is.null(start_regime)) return()
  if (! (start_regime %in% c(0, 1))) {
    stop("'start_regime' must be 0 or 1.", call. = FALSE)
  }
}

check_n_t_switches <- function(n_t, n_switches, r) {
  check_whole_nn(n_t)
  check_whole_nn(n_switches)
  if (n_switches >= n_t) {
    stop(paste0(n_switches, " switches are not possible if the length of\n",
                "the time series is ", n_t))
  }
}

check_z_compatible <- function(z, n_t, n_switches) {
  if (!is.null(n_t) && n_t != length(z)) {
    warning(paste0("'n_t' is ignored, because a time series for 'z' ",
                   "of a different length was provided."))
  }

  if (!is.null(n_switches)) {
    warning(paste0("'n_switches' is ignored, because a time series for 'z' ",
                   "was provided."))
  }
}

check_z <- function(z, r, phi_R0, phi_R1, d, start_regime) {
  if (!is.numeric(z)) error_numeric(z)

  # For the first observation of y that is predicted, y[a + 1],
  # we can use observations 1, ..., a + 1 - d to infer about the regime
  # at time point a + 1
  a <- max(c(phi_R0, phi_R1, d))
  first_obs <- (a + 1) - d
  if (all(first_z > r[1] & first_z <= r[2]) && is.null(start_regime)) {
    stop(paste0("Observation(s) ",
                paste(1:first_obs, sep = ", "),
                " fall in the hysteresis zone, but no starting regime
                was given. "),
         call. = FALSE)
  } else {
    if (!is.null(start_regime)) {
      warning(paste0("'start_regime' was ignored, because it could be inferred ",
                     "from 'z' and 'r'."))
    }
  }
}

# Helpers
is_whole <- function(x, tol = .Machine$double.eps) {
  whole <- all(abs(round(x) - x) < tol)
  return(whole)
}

error_whole <- function(x) {
  err <- structure(
    list(
      message = paste0("'", substitute(x), "' must be a whole number."),
      call = NULL
    ),
    class = c("not_whole", "error", "condition")
  )
  stop(err)
}

error_numeric <- function(x) {
  err <- structure(
    list(
      message = paste0("'", substitute(x), "' must be numeric. ",
                       "You provided an object of type ",
                       typeof(x), "."),
      call = NULL
    ),
    class = c("not_numeric", "error", "condition")
  )
  stop(err)
}

error_nonnegative <- function(x) {
  err <- structure(
    list(
      message = paste0("'", substitute(x), "' must be non-negative."),
      call = NULL
    ),
    class = c("not_nonnegative", "error", "condition")
  )
  stop(err)
}

