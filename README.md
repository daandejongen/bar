
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hystar <a href="https://daandejongen.github.io/hystar/"><img src="man/figures/logo.png" align="right" height="139" /></a>

<!-- badges: start -->

[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-green.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/daandejongen/hystar/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/daandejongen/hystar/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/daandejongen/hystar/branch/master/graph/badge.svg)](https://app.codecov.io/gh/daandejongen/hystar?branch=master)
<!-- badges: end -->

## Overview

With the R-package
[`hystar`](https://cran.r-project.org/package=hystar), you can simulate
data from the hysteretic threshold autoregressive (HysTAR) model, and
estimate its parameters. It comes with three functions:

- `hystar_fit`, to **estimate** the HysTAR parameters with the
  **conditional least squares** method, using your own data or simulated
  data,

- `z_sim`, to simulate a **threshold variable**,

- `hystar_sim`, to simulate an **outcome variable**.

Results from the time series analysis can be assessed with the standard
methods in R, like `plot`, `summary` and `print`. Additionally, you can
extract the predictive residuals with the `residuals`-method for further
analysis.

## Use

A minimal example:

``` r
library(hystar)
#> 
#>    __            __
#>   / /_ __ ______/ /_________
#>  / _  / // (_ -/  _/ _  / __\
#> /_//_/\_, /___)\__/\_,_/_/
#>      /___/             1.2.0
#> 
#> Estimation and simulation of the HysTAR Model.
#> For function help, run `?hystar_fit`, `?hystar_sim` or `?z_sim`.
#> For more information, run `hystar_info()` (opens a URL in your browser).
control_variable <- z_sim(n_t = 100)
simulated_hystar_model <- hystar_sim(z = control_variable)
fitted_hystar_model <- hystar_fit(data = simulated_hystar_model$data)
summary(fitted_hystar_model)
#> HysTAR model fitted on 99 observations, of which
#> 51 observations in regime 0 and
#> 48 observations in regime 1.
#> 
#> Estimated thresholds:
#>     r0     r1 
#> -0.454  0.562 
#> 
#> Estimated delay:
#> 0 
#> 
#> Estimated model coefficients:
#>          est    SE     p
#> phi_00 0.314 0.156 0.045
#> phi_01 0.346 0.106 0.001
#> phi_10 1.882 0.435 0.000
#> phi_11 0.538 0.108 0.000
#> 
#> Estimated residual variances:
#> sigma2_0 sigma2_1 
#>    1.009    1.097 
#> 
#> Residuals: 
#>    min     1q median     3q    max 
#> -2.639 -0.676  0.014  0.823  2.532 
#> 
#> Information criteria:
#>      bic      aic     aicc    aiccp 
#> 28.28185 16.87277 17.92886 28.87277
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

For more information about the package, see the [`hystar`
website](https://daandejongen.github.io/hystar/).

If you want to read more about the HysTAR model itself, see the paper
with the original proposal of the HysTAR model in Biometrika ([Li, Guan,
Li and Yu
(2015)](https://academic.oup.com/biomet/article-abstract/102/3/717/2365298?login=false)).
Or, for a mathematically more accessible introduction, see [the
paper](https://osf.io/preprints/psyarxiv/zrcft) (pre-print) I wrote
about detecting hysteresis with the HysTAR model in psychological time
series.
