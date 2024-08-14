
# WeibullR.shiny

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/WeibullR.shiny)](https://CRAN.R-project.org/package=WeibullR.shiny)
[![CRAN
checks](https://badges.cranchecks.info/summary/WeibullR.shiny.svg)](https://cran.r-project.org/web/checks/check_results_WeibullR.shiny.html)
[![](http://cranlogs.r-pkg.org/badges/last-month/WeibullR.shiny)](https://cran.r-project.org/package=WeibullR.shiny)
[![](http://cranlogs.r-pkg.org/badges/grand-total/WeibullR.shiny)](https://cran.r-project.org/package=WeibullR.shiny)
[![Lifecycle:experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![](https://img.shields.io/badge/doi-10.32614/CRAN.package.WeibullR.shiny-green.svg)](https://doi.org/10.32614/CRAN.package.WeibullR.shiny)
<!-- badges: end -->

<img
src="https://github.com/paulgovan/WeibullR.shiny/blob/master/inst/app/www/WeibullR.png?raw=true"
style="width:25.0%" />

# Introduction

Welcome to **WeibullR.shiny**, a Shiny web application for life data
analysis based on the WeibullR package in R. This app has an intuitive
interface for performing Weibull analysis, making it accessible for
users with varying levels of experience.

# Getting Started

To install WeibullR.shiny in R:

``` r
install.packages("WeibullR.shiny")
```

To install the development version:

``` r
devtools::install_github('paulgovan/weibullr.shiny')
```

To launch the app:

``` r
WeibullR.shiny::WeibullR.shiny()
```

Or to access the app through a browser, visit
[paulgovan.shinyapps.io/WeibullRshiny](https://paulgovan.shinyapps.io/WeibullRshiny).

# Example

## Landing

Upon launching the app, you’ll be greeted by the Landing page. From
here, you can:

- Access this README.
- Download an example dataset.
- Navigate to the Data tab.

![](https://github.com/paulgovan/WeibullR.shiny/blob/master/inst/images/Landing.png?raw=true)

## Data

For demonstration, we’ll use the “Time-to-Failure” dataset. First,
download the dataset from the Landing page. Then, use the Data Input
option in the Data tab to upload it to the app.

At this stage, your app should resemble the following:

![](https://github.com/paulgovan/WeibullR.shiny/blob/master/inst/images/Data.png?raw=true)

You can explore additional options for data arrangement, but for this
example, we’ll proceed with the default settings.

## Modeling

Next, navigate to the Modeling tab to build your Weibull model. The app
will generate a Probability Plot using default settings. Feel free to
experiment with different configurations to tailor the model to your
needs.

Below the plot, you’ll find additional settings for further
customization.

![](https://github.com/paulgovan/WeibullR.shiny/blob/master/inst/images/ProbPlot.png?raw=true)

Finally, visit the Contour Plot tab to create a contour plot. As with
the previous plot, various customization options are available.

![](https://github.com/paulgovan/WeibullR.shiny/blob/master/inst/images/ContPlot.png?raw=true)

## Code of Conduct

Please note that the WeibullR.shiny project is released with a
[Contributor Code of
Conduct](https://paulgovan.github.io/WeibullR.shiny/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

## More Resources

[WeibullR.plotly](https://paulgovan.github.io/WeibullR.plotly/) is a
package for building interactive Weibull models.

[WeibullR.learnr](https://paulgovan.github.io/WeibullR.learnr/) is an
interactive introduction to life data analysis.
