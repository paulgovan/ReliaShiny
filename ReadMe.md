
# ReliaShiny <a href="https://paulgovan.github.io/ReliaShiny/"><img src="man/figures/logo.png" align="right" height="139" alt="ReliaShiny website" /></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/ReliaShiny)](https://CRAN.R-project.org/package=ReliaShiny)
[![R-CMD-check](https://github.com/paulgovan/ReliaShiny/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/paulgovan/ReliaShiny/actions/workflows/R-CMD-check.yaml)
[![](http://cranlogs.r-pkg.org/badges/last-month/ReliaShiny)](https://cran.r-project.org/package=ReliaShiny)
[![](http://cranlogs.r-pkg.org/badges/grand-total/ReliaShiny)](https://cran.r-project.org/package=ReliaShiny)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![](https://img.shields.io/badge/doi-10.32614/CRAN.package.ReliaShiny-green.svg)](https://doi.org/10.32614/CRAN.package.ReliaShiny)
<!-- badges: end -->

## Introduction

**Welcome to ReliaShiny!** ReliaShiny is an interactive web application
for reliability analysis. The app is built using the
[shiny](https://shiny.posit.co/) package in R. ReliaShiny provides an
easy-to-use interface for performing reliability analysis using the
[WeibullR](https://cran.r-project.org/package=WeibullR),
[WeibullR.ALT](https://cran.r-project.org/package=WeibullR.ALT), and
[ReliaGrowR](https://cran.r-project.org/package=ReliaGrowR) packages in
R.

## Getting Started

To install `ReliaShiny` in R:

``` r
install.packages("ReliaShiny")
```

To install the development version:

``` r
devtools::install_github("paulgovan/ReliaShiny")
```

To launch the app:

``` r
ReliaShiny::ReliaShiny()
```

<img
src="https://github.com/paulgovan/ReliaShiny/blob/master/inst/images/Landing.png?raw=true"
style="width:100.0%" />

Or to access the app through a browser, visit
[govan.shinyapps.io/reliashiny/](https://govan.shinyapps.io/reliashiny/).

## Features

### Life Data Analysis

Fit Weibull and Lognormal distributions to time-to-failure data using
Maximum Likelihood Estimation or Rank Regression. Generate probability
plots and contour plots to assess fit and parameter uncertainty.

<img
src="https://github.com/paulgovan/ReliaShiny/blob/master/inst/images/ProbPlot.png?raw=true"
style="width:100.0%" />

### Reliability Growth Analysis

Model reliability growth using Crow-AMSAA, Piecewise NHPP, or automatic
change-point detection. Visualize results with Reliability Growth and
Duane plots.

<img
src="https://github.com/paulgovan/ReliaShiny/blob/master/inst/images/GrowthPlot.png?raw=true"
style="width:100.0%" />

### Repairable Systems

Analyze repairable systems with Power Law, Log-Linear, or Piecewise NHPP
models. Visualize cumulative events, event rates, and the Mean
Cumulative Function (MCF).

<img
src="https://github.com/paulgovan/ReliaShiny/blob/master/inst/images/Repairable%20Systems/RepairablePlot.png?raw=true"
style="width:100.0%" />

### Accelerated Life Testing

Fit Weibull or Lognormal distributions under accelerated stress
conditions using Arrhenius or Power Law life-stress relationships.
Visualize ALT probability plots and life-stress relationships.

<img
src="https://github.com/paulgovan/ReliaShiny/blob/master/inst/images/ALT/ALTPlot.png?raw=true"
style="width:100.0%" />

## Code of Conduct

Please note that the ReliaShiny project is released with a [Contributor
Code of
Conduct](https://paulgovan.github.io/ReliaShiny/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
