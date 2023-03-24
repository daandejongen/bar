check_hystar_sim_input <- function(z, r, d, phi_R0, phi_R1, resvar, start_regime) {
  check_z(z)
  check_phi(phi_R0, R01 = "0") # Argument RO1 is for printing purposes.
  check_phi(phi_R1, R01 = "1")
  p0 <- get_order(phi_R0)
  p1 <- get_order(phi_R1)
  check_d(d)
  z <- remove_z_buffer(z, d, p0, p1)
  check_zdp(z, d, p0, p1)
  check_resvar(resvar)
  check_r_sim(r, z)
  start_inferred <- get_start(g = c(d, r[1], r[2]), z, time_eff(z, d, p0, p1))
  start <- check_start(start_inferred, start_regime, z)

  return(list(z = z, start = start))
}

check_z_sim_input <- function(n_t, n_switches, start_regime, start_hyst, range) {
  check_n_t_switches(n_t, n_switches)
  check_start_regime(start_regime)
  check_start_hyst(start_hyst)
  check_range(range)
}

check_z <- function(z) {
  if (missing(z))
    stop(paste0("Argument `z` is missing, with no default."),
         call. = FALSE)
  if (!is.numeric(z)) error_numeric(z)
}

check_d <- function(d) {
  if (missing(d))
    stop(paste0("Argument `d` is missing, with no default."), call. = FALSE)
  if (!is.numeric(d)) error_numeric(d)
  check_whole_nn(d)
}

check_r_sim <- function(r, z) {
  if (missing(r))
    stop(paste0("Argument `r` is missing, with no default."), call. = FALSE)
  if (!is.numeric(r)) error_numeric(r)
  if (length(r) != 2)
    stop(paste0("You must provide two threshold values in `r`. You provided ",
                length(r), " values."),
         call. = FALSE)

  if (r[1] > r[2])
    stop("The second threshold should be larger than or equal to the first.",
         call. = FALSE)

  if (r[1] <= min(z) || r[2] >= max(z))
    stop(paste0("You must provide threshold values inside the range of `z`."),
         call. = FALSE)
}

check_zdp <- function(z, d, p0, p1) {
  if (d >= length(z))
    stop(paste0("The delay cannot be equal to ",
                "or greater than the length of `z`."),
         call. = FALSE)

  if (max(p0, p1) >= length(z))
    stop(paste0("The orders of the regimes can both not be equal to ",
                "or greater than the length of `z`."),
         call. = FALSE)

  if (max(p0, p1) - d > 10)
    stop(paste0("For practical purposes, the maximum order can not be ",
                "more than 10 larger than the delay."),
         call. = FALSE)
}

check_resvar <- function(resvar) {
  if (!is.numeric(resvar)) error_numeric(resvar)
  if (length(resvar) != 2)
    stop(paste0("You must provide exactly one residual variance for ",
                "each regime.\nYou provided ", length(resvar), " values."),
         call. = FALSE)
  if (!all(resvar > 0))
    stop("Values in 'resvar' should be postitive.", call. = FALSE)
}

check_phi <- function(phi, R01) {
  if (missing(phi))
    stop(paste0("Argument `phi_R", R01, "` is missing, with no default."),
         call. = FALSE)
  if (!is.numeric(phi)) error_numeric(phi)

  if (length(phi) < 1)
    stop(paste0("You must provide at least 1 value in phi_R", R01, "."),
         call. = FALSE)

  S <- sum(phi[-1])

  if (S >= 1) {
    assumption_string <- paste0("Note that `y` must be stationary ",
                                "in order for the standard errors to be valid.\n",
                                "See Details in `?hystar_sim()`.")
    if (S == 1)
      warning(paste0("The AR process in regime ", R01, " is non-stationary, ",
                     "because it has a unit root.\n", assumption_string),
              call. = FALSE)

    if (S > 1)
      warning(paste0("The AR process in regime ", R01, " is non-stationary, ",
                     "because it has an explosive root.\n", assumption_string),
              call. = FALSE)
  }
}

check_n_t_switches <- function(n_t, n_switches) {
  check_whole_nn(n_t)
  check_whole_nn(n_switches)
  if (n_switches >= n_t)
    stop(paste0(n_switches, " switches are not possible if the length of ",
                "the time series is ", n_t, "."))
  if (n_switches < 2)
    stop(paste0("`n_switches` must be at least 2."), call. = FALSE)
}

check_start <- function(start_inferred, start_regime, z) {
  z_simulated <- is_z_simulated(z)

  if (z_simulated) {
    # The user can accidentally provide the argument start_regime.
    # If that is the correct start regime, we will ignore it.
    # Otherwise, we give a warning and say that the start regime
    # from z_sim will be used.
    if (!is.null(start_regime) && start_regime != attr(z, "start_regime"))
      warning(paste0("`start_regime` does not match `start_regime` that ",
                     "was used in `z_sim()`: ", attr(z, "start_regime"),
                     ".\nNote that you don't ",
                     "need to provide the `start_regime` argument in this case,",
                     "and that the starting regime from z_sim() will be used."),
              call. = FALSE)

    # When z_sim() starts in the middle of its range, the user should put the
    # threshold values 'around' this middle. Otherwise, the start will not be
    # hysteretic.
    if (start_inferred != -1 && attr(z, "start_hyst"))
      stop(paste0("You requested an hysteretic start in `z_sim()`, ",
                  "but the start can be inferred from `z`, `d` and `r`.\n",
                  "Make sure that you put the threshold values 'around' the ",
                  "middle of the range of `z`."),
           call. = FALSE)

    return(attr(z, "start_regime"))
  }

  if (!z_simulated) {
    if (start_inferred != -1) {
      if (!is.null(start_regime) && start_inferred != start_regime)
        warning(paste0("`start_regime` is different from what is implied by ",
                       "the values of `z`, `d` and `r`. \nThe model implied ",
                       "start will be used."), call. = FALSE)
      return(start_inferred)
    }

    if (start_inferred == -1 && is.null(start_regime))
      stop(paste0("The starting regime is unknown, ",
                  "but `start_regime` was not provided."), call. = FALSE)

    if (start_inferred == -1 && !is.null(start_regime)) {
      check_start_regime(start_regime)
      return(start_regime)
    }
  }
}

check_start_regime <- function(start_regime) {
  if (is.null(start_regime)) return()
  if (! (start_regime %in% c(0, 1)))
    stop("'start_regime' must be 0 or 1.", call. = FALSE)
}

check_start_hyst <- function(start_hyst) {
  if (!(start_hyst %in% c(TRUE, FALSE)))
    stop(paste0("`starthyst` must be TRUE or FALSE"), call. = FALSE)
}

check_range <- function(range) {
  if (!is.numeric(range)) error_numeric(range)

  if (length(range) != 2)
    stop(paste0("`range` should be a vector of length 2.\n",
                "You provided a vector of length ", length(range), "."),
         call. = FALSE)

  if (range[1] >= range[2])
    stop(paste0("`range` should be a proper interval. \n",
                "Right now, the second value is not larger than the first."),
         call. = FALSE)
}
