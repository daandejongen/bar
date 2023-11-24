
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hystar <a href="https://daandejongen.github.io/hystar/"><img src="man/figures/logo.png" align="right" height="139" /></a>

<!-- badges: start -->

[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-green.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/daandejongen/hystar/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/daandejongen/hystar/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Overview

The R-package [`hystar`](https://cran.r-project.org/package=hystar)
package allows you to simulate and estimate the hysteretic threshold
autoregressive (HysTAR) model. It comes with three functions:

- `hystar_fit`, to estimate the HysTAR parameters with the conditional
  least squares method,

- `z_sim`, to simulate a threshold variable,

- `hystar_sim`, to simulate an outcome variable

Results from the time series analysis can be assessed with the standard
methods in R, like `plot`, `summary` and `print`. Additionally, you can
extract the predictive residuals with the `residuals`-method for further
analysis.

## Use

A minimal example:

``` r
library(hystar)
control_variable <- z_sim(n_t = 100)
simulated_hystar_model <- hystar_sim(z = control_variable)
fitted_hystar_model <- hystar_fit(data = simulated_hystar_model$data)
summary(fitted_hystar_model)
#> HysTAR model fitted on 99 observations, of which
#> 50 observations in regime 0 and
#> 49 observations in regime 1.
#> 
#> Estimated thresholds:
#>     r0     r1 
#> -0.509  0.562 
#> 
#> Estimated delay:
#> 0 
#> 
#> Estimated model coefficients:
#>          est    SE     p
#> phi_00 0.068 0.142 0.634
#> phi_01 0.553 0.093 0.000
#> phi_10 3.038 0.535 0.000
#> phi_11 0.256 0.131 0.051
#> 
#> Estimated residual variances:
#> sigma2_0 sigma2_1 
#>    0.955    0.857 
#> 
#> Residuals: 
#>    min     1q median     3q    max 
#> -2.379 -0.645  0.005  0.580  2.655 
#> 
#> Information criteria:
#>       aic      aicc       bic 
#>  2.104235  3.159307 13.515765
```

## Install

For the current [CRAN
release](https://cran.r-project.org/package=hystar) (1.0.0):

    install.packages("hystar")

For the development version (1.2.0.9000):

    devtools::install_github("daandejongen/hystar")

## Cite

If you have used this package for an scientific publication, please cite
it with:

De Jong, D. (2022). *hystar: Simulation and Estimation of the Hysteretic
TAR Model*. R package version 1.2.0,
<https://github.com/daandejongen/hystar/>.

BibTeX:

    @Manual{,
        title = {hystar: Simulation and Estimation of the Hysteretic TAR Model},
        author = {Daan {de Jong}},
        year = {2022},
        note = {R package version 1.2.0},
        url = {https://github.com/daandejongen/hystar/},
      }

## Get more info

For more information, see the [`hystar`
website](https://daandejongen.github.io/hystar/).

If you want to read more, see the paper with the original proposal of
the HysTAR model in Biometrika ([Li, Guan, Li and Yu
(2015)](https://academic.oup.com/biomet/article-abstract/102/3/717/2365298?login=false)).

If you need something more accessible, I am working on a paper about
using the HysTAR model in psychological research. There, I also explain
in more detail what hysteresis is.
