# Simulation and Estimation of the Hysteretic TAR Model

<!-- badges: start -->
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)]
(https://lifecycle.r-lib.org/articles/stages.html#stable)

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

## An example
```
z <- z_sim(n_t = 200, n_switches = 5, start_regime = 1)
sim <- hystar_sim(z = z, r = c(-.5, .5), d = 2, phi_R0 = c(0, .6), phi_R1 = 1,
                  resvar = c(1, 1))
fit <- hystar_fit(y = sim$data$y, z = z)
plot(fit)
```

## How to install?
You can install the package by running `devtools::install_github("hystar")`.
(Make sure you have installed the devtools package with
`install.packages("devtools")` and have loaded and attached it with
`library(devtools)`.)

The package will be submitted to CRAN in the near future.

## Cite this package
If you have used this package in a study, please cite it with:

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

Currently, I am working on a paper that introduces the HysTAR model into
a psychological context.

