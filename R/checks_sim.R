check_hystar_sim_input <- function(z, z_sim, r, d, phi_R0, phi_R1, resvar,
                                   init_vals, start_regime) {
  check_r(r)
  check_whole_nn(d)
  check_phi(phi_R0, R01 = 0)
  check_phi(phi_R1, R01 = 1)
  check_resvar(resvar)
  check_init_vals(init_vals, phi_R0, phi_R1, d)
  check_start_regime(start_regime)
  p <- max(get_order(phi_R0), get_order(phi_R0))
  start <- check_z_start(z, r, p, d, start_regime)

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

check_n_t_switches <- function(n_t, n_switches) {
  check_whole_nn(n_t)
  check_whole_nn(n_switches)
  if (n_switches >= n_t) {
    stop(paste0(n_switches, " switches are not possible if the length of ",
                "the time series is ", n_t, "."))
  }
}

check_z_start <- function(z, r, p, d, start_regime) {
  if (!is.numeric(z)) error_numeric(z)

  start <- get_start(g = c(d, r[1], r[2]), z, time_eff(z, d, p, p))

  if (start == -1) {
    stop(paste0("Observation(s) ",
                paste(1:first_obs, sep = ", "),
                " fall in the hysteresis zone, but no starting regime
                was given. "),
         call. = FALSE)
  } else {
    if (!is.null(start_regime)) {
      warning(paste0("`start_regime` was ignored, because it could be inferred ",
                     "from `z` and `r`."))
    }
  }

  return(start)
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
