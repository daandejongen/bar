# hystar <a href="https://daandejongen.github.io/hystar/"><img src="man/figures/logo.png" align="right" height="139" /></a>

<!-- badges: start -->
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-green.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
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

## Installation

`install.packages("hystar")` for the CRAN version.

`devtools::install_github("daandejongen/hystar")` for the development version.

## Use
```
control_variable <- z_sim()
simulated_hystar_model <- hystar_sim(z = z)
fitted_hystar_model <- hystar_fit(simulated_hystar_model$data)
```

## Cite

If you have used this package for an academic publication, please cite it with:

De Jong, D. (2022). _hystar: Simulation and Estimation of the Hysteretic TAR Model_. R package version 1.0.0, <https://github.com/daandejongen/hystar/>.

BibTeX:
```
@Manual{,
    title = {hystar: Simulation and Estimation of the Hysteretic TAR Model},
    author = {Daan {de Jong}},
    year = {2022},
    note = {R package version 1.0.0},
    url = {https://github.com/daandejongen/hystar/},
  }
```

## References
The HysTAR model was originally proposed by Li, Guan, Li and Yu (2015). 

Li, Guodong, Bo Guan, Wai Keung Li, en Philip L. H. Yu. ‘Hysteretic Autoregressive Time Series Models’. Biometrika 102, nr. 3 (september 2015): 717–23.
