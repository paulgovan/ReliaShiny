library(shinytest2)

# Helper: check rendered table HTML contains a cell value
has_cell <- function(html, text) isTRUE(grepl(text, html, fixed = TRUE))

# ---- Life Data module --------------------------------------------------------

test_that("Life Data: Weibull 2P MLE fits and renders results table", {
  skip_on_cran()
  app <- AppDriver$new(name = "life-data-weibull", seed = 42, height = 857, width = 1211)

  app$set_inputs(sidebarMenu = "model")
  app$wait_for_idle(timeout = 15000)

  html <- app$get_value(output = "wblr_results")
  expect_true(is.character(html) && nchar(html) > 0)
  expect_true(has_cell(html, "Model Type"))
  expect_true(has_cell(html, "Beta"))
  expect_true(has_cell(html, "Eta"))
  expect_true(has_cell(html, "Log-likelihood"))
})

test_that("Life Data: Lognormal RR renders results table with R^2", {
  skip_on_cran()
  app <- AppDriver$new(name = "life-data-lognormal", seed = 42, height = 857, width = 1211)

  app$set_inputs(sidebarMenu = "model")
  app$set_inputs(dist = "lognormal", meth = "rr-xony")
  app$wait_for_idle(timeout = 15000)

  html <- app$get_value(output = "wblr_results")
  expect_true(is.character(html) && nchar(html) > 0)
  expect_true(has_cell(html, "Mulog"))
  expect_true(has_cell(html, "R^2"))
})

# ---- Reliability Growth module -----------------------------------------------

test_that("Reliability Growth: Crow-AMSAA fits and renders results", {
  skip_on_cran()
  app <- AppDriver$new(name = "growth-crow-amsaa", seed = 42, height = 857, width = 1211)

  app$set_inputs(sidebarMenu = "growthModel")
  app$set_inputs(growthModel = "1", times = "times", failures = "failures")
  app$wait_for_idle(timeout = 15000)

  html <- app$get_value(output = "rga_results")
  expect_true(is.character(html) && nchar(html) > 0)
  expect_true(has_cell(html, "Crow-AMSAA"))
  expect_true(has_cell(html, "Lambda"))
})

test_that("Reliability Growth: download handler returns non-empty CSV", {
  skip_on_cran()
  app <- AppDriver$new(name = "growth-download", seed = 42, height = 857, width = 1211)

  app$set_inputs(sidebarMenu = "growthModel")
  app$set_inputs(growthModel = "1", times = "times", failures = "failures")
  app$wait_for_idle(timeout = 15000)

  path <- app$get_download("downloadRgaResults")
  df   <- read.csv(path)
  expect_gt(nrow(df), 0)
  expect_true("Param" %in% names(df))
})

# ---- Repairable Systems module -----------------------------------------------

test_that("Repairable Systems: Power Law NHPP fits and renders results", {
  skip_on_cran()
  app <- AppDriver$new(name = "rs-power-law", seed = 42, height = 857, width = 1211)

  app$set_inputs(sidebarMenu = "rsModel")
  app$set_inputs(rsNhppModel = "1")
  app$wait_for_idle(timeout = 15000)

  html <- app$get_value(output = "rs_results")
  expect_true(is.character(html) && nchar(html) > 0)
  expect_true(has_cell(html, "Power Law"))
  expect_true(has_cell(html, "LogLik"))
})

# ---- Accelerated Life Testing module -----------------------------------------

test_that("ALT: Weibull Arrhenius fits Nelson data and results table is populated", {
  skip_on_cran()
  app <- AppDriver$new(name = "alt-weibull-arrhenius", seed = 42, height = 857, width = 1211)

  app$set_inputs(sidebarMenu = "altModel")
  app$wait_for_idle(timeout = 20000)

  html <- app$get_value(output = "alt_results")
  expect_true(is.character(html) && nchar(html) > 0)
  expect_true(has_cell(html, "Beta (Shape)"))
  expect_true(has_cell(html, "Eta @ Stress"))
  expect_true(has_cell(html, "AF @ Stress"))
  expect_true(has_cell(html, "Life-Stress"))
})

test_that("ALT: Meeker data with Power Law model renders results", {
  skip_on_cran()
  app <- AppDriver$new(name = "alt-power-law", seed = 42, height = 857, width = 1211)

  app$set_inputs(altDataSelect = "2")
  app$wait_for_idle(timeout = 10000)
  app$set_inputs(sidebarMenu = "altModel")
  app$set_inputs(altModel = "power")
  app$wait_for_idle(timeout = 20000)

  html <- app$get_value(output = "alt_results")
  expect_true(is.character(html) && nchar(html) > 0)
  expect_true(has_cell(html, "power"))
})

test_that("ALT: download returns non-empty CSV", {
  skip_on_cran()
  app <- AppDriver$new(name = "alt-download", seed = 42, height = 857, width = 1211)

  app$set_inputs(sidebarMenu = "altModel")
  app$wait_for_idle(timeout = 20000)

  path <- app$get_download("downloadAltResults")
  df   <- read.csv(path)
  expect_gt(nrow(df), 0)
  expect_true("Param" %in% names(df))
})
