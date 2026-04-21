library(shinytest2)

# ---- Life Data module --------------------------------------------------------

test_that("Life Data: Weibull 2P MLE fits and renders results table", {
  skip_on_cran()
  app <- AppDriver$new(name = "life-data-weibull", seed = 42, height = 857, width = 1211)

  app$set_inputs(sidebarItemExpanded = "LifeData")
  app$set_inputs(`sidebar` = "data")
  app$set_inputs(dataInput = "1", dataSelect = "1")
  app$wait_for_idle(timeout = 10000)

  app$set_inputs(`sidebar` = "model")
  app$set_inputs(dist = "weibull2p", meth = "mle")
  app$wait_for_idle(timeout = 15000)

  tbl <- app$get_value(output = "wblr_results")
  expect_false(is.null(tbl))

  app$expect_values(output = "probPlot", screenshot_args = FALSE)
})

test_that("Life Data: Lognormal RR renders results table with R^2", {
  skip_on_cran()
  app <- AppDriver$new(name = "life-data-lognormal", seed = 42, height = 857, width = 1211)

  app$set_inputs(sidebarItemExpanded = "LifeData")
  app$set_inputs(`sidebar` = "model")
  app$set_inputs(dist = "lognormal", meth = "rr-xony")
  app$wait_for_idle(timeout = 15000)

  tbl <- app$get_value(output = "wblr_results")
  expect_false(is.null(tbl))
})

# ---- Reliability Growth module -----------------------------------------------

test_that("Reliability Growth: Crow-AMSAA fits and renders plot", {
  skip_on_cran()
  app <- AppDriver$new(name = "growth-crow-amsaa", seed = 42, height = 857, width = 1211)

  app$set_inputs(sidebarItemExpanded = "ReliabilityGrowth")
  app$set_inputs(`sidebar` = "growthData")
  app$set_inputs(growthDataInput = "1", growthDataSelect = "1")
  app$wait_for_idle(timeout = 10000)

  app$set_inputs(`sidebar` = "growthModel")
  app$set_inputs(growthModel = "1", times = "times", failures = "failures")
  app$wait_for_idle(timeout = 15000)

  tbl <- app$get_value(output = "rga_results")
  expect_false(is.null(tbl))

  app$expect_values(output = "growthPlot", screenshot_args = FALSE)
})

test_that("Reliability Growth: download handler returns non-empty CSV", {
  skip_on_cran()
  app <- AppDriver$new(name = "growth-download", seed = 42, height = 857, width = 1211)

  app$set_inputs(sidebarItemExpanded = "ReliabilityGrowth")
  app$set_inputs(`sidebar` = "growthModel")
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

  app$set_inputs(sidebarItemExpanded = "RepairableSystems")
  app$set_inputs(`sidebar` = "rsData")
  app$set_inputs(rsDataInput = "1", rsDataSelect = "1")
  app$wait_for_idle(timeout = 10000)

  app$set_inputs(`sidebar` = "rsModel")
  app$set_inputs(rsNhppModel = "1")
  app$wait_for_idle(timeout = 15000)

  tbl <- app$get_value(output = "rs_results")
  expect_false(is.null(tbl))

  app$expect_values(output = "rsNhppPlot", screenshot_args = FALSE)
})

# ---- Accelerated Life Testing module -----------------------------------------

test_that("ALT: Weibull Arrhenius fits Nelson data and results table has 13 rows", {
  skip_on_cran()
  app <- AppDriver$new(name = "alt-weibull-arrhenius", seed = 42, height = 857, width = 1211)

  app$set_inputs(sidebarItemExpanded = "AcceleratedLifeTesting")
  app$set_inputs(`sidebar` = "altData")
  app$set_inputs(altDataInput = "1", altDataSelect = "1")
  app$wait_for_idle(timeout = 10000)

  app$set_inputs(`sidebar` = "altModel")
  app$set_inputs(altDist = "weibull", altModel = "arrhenius")
  app$wait_for_idle(timeout = 15000)

  tbl <- app$get_value(output = "alt_results")
  expect_false(is.null(tbl))
  # 5 fixed + 3 Eta + 3 AF + 2 GoF = 13 rows
  expect_equal(nrow(tbl), 13)

  # R² must be in [0, 1]
  r2_row <- tbl[tbl$Param == "R\u00b2 (Life-Stress)", "Value"]
  expect_true(!is.null(r2_row) && length(r2_row) > 0)
  expect_true(as.numeric(r2_row) >= 0 && as.numeric(r2_row) <= 1)

  app$expect_values(output = "altProbPlot", screenshot_args = FALSE)
})

test_that("ALT: Meeker data with Power Law model renders results", {
  skip_on_cran()
  app <- AppDriver$new(name = "alt-power-law", seed = 42, height = 857, width = 1211)

  app$set_inputs(sidebarItemExpanded = "AcceleratedLifeTesting")
  app$set_inputs(`sidebar` = "altData")
  app$set_inputs(altDataInput = "1", altDataSelect = "2")
  app$wait_for_idle(timeout = 10000)

  app$set_inputs(`sidebar` = "altModel")
  app$set_inputs(altDist = "weibull", altModel = "power")
  app$wait_for_idle(timeout = 15000)

  tbl <- app$get_value(output = "alt_results")
  expect_false(is.null(tbl))
  expect_equal(tbl$Value[tbl$Param == "ALT Model"], "power")
})

test_that("ALT: download returns non-empty CSV", {
  skip_on_cran()
  app <- AppDriver$new(name = "alt-download", seed = 42, height = 857, width = 1211)

  app$set_inputs(sidebarItemExpanded = "AcceleratedLifeTesting")
  app$set_inputs(`sidebar` = "altModel")
  app$set_inputs(altDist = "weibull", altModel = "arrhenius")
  app$wait_for_idle(timeout = 15000)

  path <- app$get_download("downloadAltResults")
  df   <- read.csv(path)
  expect_gt(nrow(df), 0)
  expect_true("Param" %in% names(df))
})
