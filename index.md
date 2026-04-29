# ReliaShiny

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

![](https://github.com/paulgovan/ReliaShiny/blob/master/inst/images/Landing.png?raw=true)

Or to access the app through a browser, visit
[govan.shinyapps.io/reliashiny/](https://govan.shinyapps.io/reliashiny/).

## Features

### Life Data Analysis

Fit Weibull and Lognormal distributions to time-to-failure data using
Maximum Likelihood Estimation or Rank Regression. Generate probability
plots and contour plots to assess fit and parameter uncertainty.

![](https://github.com/paulgovan/ReliaShiny/blob/master/inst/images/ProbPlot.png?raw=true)

### Reliability Growth Analysis

Model reliability growth using Crow-AMSAA, Piecewise NHPP, or automatic
change-point detection. Visualize results with Reliability Growth and
Duane plots.

![](https://github.com/paulgovan/ReliaShiny/blob/master/inst/images/GrowthPlot.png?raw=true)

### Repairable Systems

Analyze repairable systems with Power Law, Log-Linear, or Piecewise NHPP
models. Visualize cumulative events, event rates, and the Mean
Cumulative Function (MCF).

![](https://github.com/paulgovan/ReliaShiny/blob/master/inst/images/Repairable%20Systems/RepairablePlot.png?raw=true)

### Accelerated Life Testing

Fit Weibull or Lognormal distributions under accelerated stress
conditions using Arrhenius or Power Law life-stress relationships.
Visualize ALT probability plots and life-stress relationships.

![](https://github.com/paulgovan/ReliaShiny/blob/master/inst/images/ALT/ALTPlot.png?raw=true)

## Citation

If you use ReliaShiny in your research, please cite the following:

> Govan, P. (2026). ReliaShiny: A Shiny Application for Reliability
> Analysis. *IEEE Reliability Magazine*, 1–9.
> <https://doi.org/10.1109/MRL.2026.3669057>

> Govan, P. (2023). *ReliaShiny: A Shiny App Reliability Analysis*. R
> package version 0.2.0.
> <https://doi.org/10.32614/CRAN.package.ReliaShiny>

## Code of Conduct

Please note that the ReliaShiny project is released with a [Contributor
Code of
Conduct](https://paulgovan.github.io/ReliaShiny/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
