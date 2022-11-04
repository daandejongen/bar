check_hystar_sim_input <- function(z, r, d, phi_R0, phi_R1, resvar, start_regime) {
  check_z(z)
  check_r(r)
  check_whole_nn(d)
  check_phi(phi_R0, R01 = 0)
  check_phi(phi_R1, R01 = 1)
  check_resvar(resvar)
  p <- max(get_order(phi_R0), get_order(phi_R0))
  start_inferred <- get_start(g = c(d, r[1], r[2]), z, time_eff(z, d, p, p))
  start <- check_start(start_inferred, start_regime)

  return(start)
}

check_z_sim_input <- function(start_regime, n_switches, n_t, form, scale) {
  check_start_regime(start_regime)
  check_whole_nn(n_switches)
  check_whole_nn(n_t)
  check_n_t_switches(n_t, n_switches)
  form <- check_form(form)
  check_scale(scale)

  return(form)
}

check_z <- function(z) {
  if (!is.numeric(z)) error_numeric(z)
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
  if (length(phi) < 1) {
    stop(paste0("You must provide at least 1 value in phi_R", R01, "."),
         call. = FALSE)
  }
  S <- sum(phi[-1])
  if (S >= 1) {
    if (S == 1) {
      warning(paste0("The AR process in regime ", R01, " is non-stationary, ",
                     "because it has a unit root."), call. = FALSE)
    }
    if (S > 1) {
      warning(paste0("The AR process in regime ", R01, " is non-stationary, ",
                     "because it has an explosive root."), call. = FALSE)
    }
  }
}

check_n_t_switches <- function(n_t, n_switches) {
  check_whole_nn(n_t)
  check_whole_nn(n_switches)
  if (n_switches >= n_t) {
    stop(paste0(n_switches, " switches are not possible if the length of ",
                "the time series is ", n_t, "."))
  }
}

check_start <- function(start_inferred, start_regime) {

  if (start_inferred == -1) {
    if (!is.null(start_regime)) {
      check_start_regime(start_regime)
      return(start_regime)
    }
    if (is.null(start_regime)) {
      stop(paste0("The starting regime is unknown, ",
                  "but `start_regime` was not provided "),
           call. = FALSE)
    }
  }

  if (start_inferred != -1) {
    if (!is.null(start_regime)) {
      warning(paste0("`start_regime` is ignored, because it can be inferred ",
                     "from `z` and `r`."))
    }
    return(start_inferred)
  }

}

check_start_regime <- function(start_regime) {
  if (is.null(start_regime)) return()
  if (! (start_regime %in% c(0, 1))) {
    stop("'start_regime' must be 0 or 1.", call. = FALSE)
  }
}

check_form <- function(form) {
  form <- tryCatch(
    error = function(cond) {
      stop("`form` must be 'cosine' or 'sine'",
           call. = FALSE)
    },
    match.arg(
      arg = form,
      choices = c("cosine", "sine")
    )
  )

  return(form)
}

check_scale <- function(scale) {
  if (!is.numeric(scale)) error_numeric(scale)
  if (length(scale) != 2) {
    stop(paste0("`scale` should be a vector of length 2.\n",
                "You provided a vector of length ", length(scale), "."),
         call. = FALSE)
  }
  if (scale[1] >= scale[2]) {
    stop(paste0("`scale` should be a proper interval. \n",
                "Right now, the second value is not larger than the first."),
         call. = FALSE)
  }
}
