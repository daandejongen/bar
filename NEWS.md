# `hystar` 1.2.1.9000 (development version)

* Default argument for `p_select` in `hystar_fit()` now shows all four options. The new default is `p_select = c("bic", "aic", "aicc", "aiccp")`, and the first option `"bic"` is taken when the user doesn't provide the argument.

* Not necessary to provide all regime names and colors, can all be specified separately

* Added `call` to `hystar_sim` and `hystar_fit` objects. Call now also printed in `print` method.

# `hystar` 1.2.1

* Added a welcome message with a logo and first directions for help.

* Added user-feedback about the progress of the estimation when running `hystar_fit()`

* Added the change-point information criterion

# `hystar` 1.2.0

* More options to *customize* the graphs from the `plot()`-method for R-objects of class `hystar_fit` and `hystar_sim`:

  - the colors of the background and lines
  
  - line types and widths of `z`, `y` and the thresholds

* More default arguments in `z_sim`: `n_t = 100` and `n_switches = 2`. Now, all arguments have a default, which allows you to create examples faster.

* More default arguments in `hystar_sim`: `r = c(-0.5, 0.5)`, `d = 0`, `phi_R0 = c(0, 0.5)` and `phi_R1 = c(2, 0.5)`. Now, the only necessary argument is `z`.

# `hystar` 1.1.0

* Fixed a mistake in the estimation of the asymptotic standard error. I noticed this during simulations, where some confidence interval coverage rates were strikingly low. All is fine now!

# `hystar` 1.0.0

* First CRAN release!

