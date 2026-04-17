# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working
with code in this repository.

## Commands

**Run the app locally:**

``` r
devtools::load_all()
ReliaShiny::ReliaShiny()
```

**Run tests:**

``` r
devtools::test()
```

**Run a single test file:**

``` r
testthat::test_file("tests/testthat/test-shiny-app.R")
```

**R CMD check (full package check):**

``` r
devtools::check()
```

**Regenerate documentation:**

``` r
devtools::document()
```

**Update shinytest2 snapshots:**

``` r
shinytest2::snapshot_review("inst/app/tests/testthat/")
```

## Architecture

ReliaShiny is an R package with a single exported function
[`ReliaShiny()`](https://paulgovan.github.io/ReliaShiny/reference/ReliaShiny.md)
(in
[`R/ReliaShiny.R`](https://paulgovan.github.io/ReliaShiny/R/ReliaShiny.R))
that launches the Shiny app via
[`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html). Nearly
all application logic lives in one monolithic file:
[`inst/app/app.R`](https://paulgovan.github.io/ReliaShiny/inst/app/app.R)
(~1600 lines).

### App structure

The app uses `shinydashboard` with URL bookmarking enabled. The sidebar
has two main analysis sections:

1.  **Life Data Analysis** — Weibull-based survival analysis using the
    `WeibullR` package. Users upload or select sample time-to-failure
    data, choose a distribution (Weibull 2P/3P, Lognormal, Weibayes) and
    estimation method (MLE or Rank Regression), then see probability
    plots, contour plots, and parameter tables.

2.  **Reliability Growth Analysis** — NHPP-based growth modeling using
    the `ReliaGrowR` package. Users upload or select sample cumulative
    failure data, choose a model (Crow-AMSAA, Piecewise NHPP, or auto
    change-point detection), then see growth plots, Duane plots, and
    parameter tables.

### Key dependencies

| Package                           | Role                                    |
|-----------------------------------|-----------------------------------------|
| `WeibullR`                        | Weibull distribution fitting            |
| `ReliaGrowR`                      | Reliability growth (Crow-AMSAA, NHPP)   |
| `ReliaPlotR`                      | Plot generation for both analysis types |
| `shinydashboard` / `shinyWidgets` | UI layout and widgets                   |
| `plotly`                          | Interactive plots                       |

### Testing

Tests use `shinytest2` for end-to-end snapshot testing: -
[`tests/testthat/test-shiny-app.R`](https://paulgovan.github.io/ReliaShiny/tests/testthat/test-shiny-app.R)
— package-level test that launches the full app -
[`inst/app/tests/testthat/test-shinytest2.R`](https://paulgovan.github.io/ReliaShiny/inst/app/tests/testthat/test-shinytest2.R)
— app-level snapshot tests with stored PNG/JSON snapshots in
`_snaps/shinytest2/`

When modifying the UI or reactive outputs, run snapshot tests and update
baselines with `shinytest2::snapshot_review()` if changes are
intentional.

### CI/CD

GitHub Actions runs `R-CMD-check` on macOS, Windows, and Ubuntu against
R release, devel, and oldrel-1. The `pkgcheck` workflow runs ropensci
package checks on pushes to master. The `pkgdown` workflow builds and
deploys the documentation site.
