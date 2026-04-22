# ReliaShiny v0.5

## New features

- Added **Repairable Systems** module for recurrent event analysis using
  Non-Homogeneous Poisson Process (NHPP) models (Power Law, Log-Linear).
- Added **Accelerated Life Testing (ALT)** module supporting Weibull and
  Lognormal distributions with Arrhenius and Power Law life-stress
  relationships.
- ALT results table now includes goodness-of-fit metrics for the
  life-stress relationship: R², Adj. R², LogLik, AIC, and BIC.
- ALT life-stress relationship plot now displays R² and Adj. R² as an
  annotation.
- ALT probability plots include an option to show/hide confidence
  bounds.
- Landing page action buttons now labeled with module names and sized
  for better usability.
- Landing page layout is responsive: infoboxes and buttons stack
  vertically on mobile screens.

## Testing

- Added comprehensive unit tests for all summary extraction helper
  functions (`extract_wblr_summ`, `extract_rga_summ`,
  `extract_nhpp_summ`, `extract_alt_summ`).
- Added end-to-end `shinytest2` tests covering all four analysis
  modules.

## Bug fixes and improvements

- Fixed [`system.file()`](https://rdrr.io/r/base/system.file.html) paths
  that returned empty strings on shinyapps.io; replaced with
  [`file.path()`](https://rdrr.io/r/base/file.path.html) relative paths.
- Extracted summary helper functions to `R/helpers.R` for standalone
  testability.
- Fixed R CMD check namespace warnings for `WeibullR.ALT` and
  `shinycssloaders`.
- Added `shinycssloaders` loading spinners to all plot outputs.

# ReliaShiny v0.4 (formerly WeibullR.shiny)

- `WeibullR.shiny` was renamed to `ReliaShiny` to better reflect its
  purpose as a reliability analysis tool.
- Updated documentation and tutorials to reflect the new name.
- Updated dependency on `ReliaPlotR` package.
- Minor bug fixes and performance improvements.

# WeibullR.shiny v0.3.1

## Minor improvements and bug fixes

# WeibullR.shiny v0.3

- Now with support for Reliability Growth Analysis including Crow-AMSAA,
  Piecewise Weibull NHPP, Change Point Detection, and Duane models.
- New tutorials on Time-to-Failure and Reliability Growth Analysis.
- Other minor improvements and bug fixes.

# WeibullR.shiny v0.2.1

## Minor improvements and bug fixes

- Updated contact info and citation.

# WeibullR.shiny v0.2

## Minor improvements and bug fixes

- Now with a lognormal plotting canvas.
- More plotting options.

# WeibullR.shiny v0.1.2

## Updates and minor improvements

- Updated dependencies
- UI improvements

# WeibullR.shiny v0.1

## Initial release

- Weibullr.shiny function
