#' A Shiny Weibull Analysis App.
#'
#' WeibullR.shiny is a Shiny web app for Weibull Analysis from WeibullR.
#' @import plotly
#' @import rhandsontable
#' @import shiny
#' @import shinydashboard
#' @import WeibullR
#' @import WeibullR.plotly
#' @export
#' @seealso \url{http://paulgovan.github.io/WeibullR.shiny/}
#' @examples
#' if (interactive()) {
#'   WeibullR.shiny()
#' }
WeibullR.shiny <- function() {
  shiny::runApp(system.file('app', package = 'WeibullR.shiny'))
}
