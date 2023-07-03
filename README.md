# Simulation and Estimation of the Hysteretic TAR Model

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/daandejongen/hystar/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/daandejongen/hystar/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The `hystar` package allows you to simulate and estimate the hysteretic
threshold autoregressive (HysTAR) model. The package has three main functions:

* `z_sim` simulates a treshold variable. You can choose how many times the
system will switch between regimes,

* `hystar_sim` simulates an outcome variable according to the HysTAR model,

* `hystar_fit` estimates the model parameters with the conditional least
squares method.

The hystar package also has a `plot()` method that combines the
threshold and outcome variable in one figure and visualizes the regimes and 
thresholds.

## Install
You can install the `hystar`-package via CRAN by running:
`install.packages("hystar")`

## Use
```
z <- z_sim(n_t = 200, n_switches = 5)
sim <- hystar_sim(z = z, r = c(-.5, .5), d = 2, phi_R0 = c(0, .6), phi_R1 = 1)
fit <- hystar_fit(sim$data)
plot(fit)
```

## Cite
If you have used this package for an academic publication, please cite it with:

De Jong, D. (2022). _hystar: Simulation and Estimation of the Hysteretic TAR Model_. R package version 0.0.9, <https://github.com/daandejongen/hystar/>.

BibTeX:
```
@Manual{,
    title = {hystar: Simulation and Estimation of the Hysteretic TAR Model},
    author = {Daan {de Jong}},
    year = {2022},
    note = {R package version 0.0.9},
    url = {https://github.com/daandejongen/hystar/},
  }
```

## References
The HysTAR model was originally proposed by Li, Guan, Li and Yu (2015). 

Li, Guodong, Bo Guan, Wai Keung Li, en Philip L. H. Yu. ‘Hysteretic Autoregressive Time Series Models’. Biometrika 102, nr. 3 (september 2015): 717–23.
