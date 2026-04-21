test_that("extract_wblr_summ returns correct structure for Weibull 2P MLE", {
  dat <- read.csv(system.file("app", "data", "acid_gas_compressor.csv", package = "ReliaShiny"))
  obj <- WeibullR::wblr(dat$time)
  obj <- WeibullR::wblr.fit(obj, method.fit = "mle")

  result <- extract_wblr_summ(obj)

  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 2)
  expect_true("Param" %in% names(result))
  expect_true("Value" %in% names(result))
  expect_true("Model Type" %in% result$Param)
  expect_true("Beta" %in% result$Param)
  expect_true("Eta" %in% result$Param)
  expect_true("Log-likelihood" %in% result$Param)
  expect_equal(result$Value[result$Param == "Model Type"], "Weibull")
})

test_that("extract_wblr_summ returns correct structure for Weibull 2P RR", {
  dat <- read.csv(system.file("app", "data", "acid_gas_compressor.csv", package = "ReliaShiny"))
  obj <- WeibullR::wblr(dat$time)
  obj <- WeibullR::wblr.fit(obj, method.fit = "rr-xony")

  result <- extract_wblr_summ(obj)

  expect_true("R^2" %in% result$Param)
  r2_val <- as.numeric(result$Value[result$Param == "R^2"])
  expect_true(r2_val >= 0 && r2_val <= 1)
})

test_that("extract_wblr_summ handles Lognormal distribution", {
  dat <- read.csv(system.file("app", "data", "acid_gas_compressor.csv", package = "ReliaShiny"))
  obj <- WeibullR::wblr(dat$time)
  obj <- WeibullR::wblr.fit(obj, dist = "lognormal", method.fit = "mle")

  result <- extract_wblr_summ(obj)

  expect_true("Mulog" %in% result$Param)
  expect_true("Sigmalog" %in% result$Param)
  expect_equal(result$Value[result$Param == "Model Type"], "Lognormal")
})

test_that("extract_rga_summ returns correct structure for Crow-AMSAA", {
  dat <- read.csv(system.file("app", "data", "simpleData.csv", package = "ReliaShiny"))
  obj <- ReliaGrowR::rga(dat$times, dat$failures)

  result <- extract_rga_summ(obj)

  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 2)
  expect_true("Model Type" %in% result$Param)
  expect_true("Beta" %in% result$Param)
  expect_true("Lambda" %in% result$Param)
  expect_true("LogLik" %in% result$Param)
  expect_true("AIC" %in% result$Param)
  expect_true("BIC" %in% result$Param)
  # Value column is a list (AsIs); index with single bracket + [[1]]
  expect_equal(result$Value[result$Param == "Model Type"][[1]], "Crow-AMSAA")
})

test_that("extract_rga_summ Growth Rate is between 0 and 1 for improving system", {
  dat <- read.csv(system.file("app", "data", "simpleData.csv", package = "ReliaShiny"))
  obj <- ReliaGrowR::rga(dat$times, dat$failures)

  result <- extract_rga_summ(obj)

  gr <- as.numeric(result$Value[result$Param == "Growth Rate"][[1]])
  expect_true(gr > 0 && gr < 1)
})

test_that("extract_nhpp_summ returns correct structure for Power Law", {
  rs  <- read.csv(system.file("app", "data", "simpleRepairData.csv", package = "ReliaShiny"))
  rs  <- rs[order(rs$time), ]
  obj <- ReliaGrowR::nhpp(
    time       = "time",
    event      = "event",
    data       = rs,
    model_type = "Power Law"
  )

  result <- extract_nhpp_summ(obj)

  expect_s3_class(result, "data.frame")
  expect_true("Model Type" %in% result$Param)
  expect_true("Total Events" %in% result$Param)
  expect_true("LogLik" %in% result$Param)
  expect_equal(result$Value[result$Param == "Model Type"], "Power Law")
})

test_that("extract_alt_summ returns correct row count for Nelson data (3 stress levels)", {
  dat <- read.csv(system.file("app", "data", "nelsonData.csv", package = "ReliaShiny"))
  stress_levels <- sort(unique(dat$stress))

  alt_list <- lapply(stress_levels, function(s) {
    sub <- dat[dat$stress == s, ]
    WeibullR.ALT::alt.data(sub$time, stress = s)
  })
  obj <- WeibullR.ALT::alt.fit(
    WeibullR.ALT::alt.parallel(
      WeibullR.ALT::alt.make(alt_list, dist = "weibull", alt.model = "arrhenius",
                             view_dist_fits = FALSE),
      view_parallel_fits = FALSE
    )
  )

  result <- extract_alt_summ(obj)

  # 5 fixed + 3 Eta + 3 AF + 2 GoF = 13
  expect_equal(nrow(result), 13)
  expect_equal(ncol(result), 2)
  expect_true("Beta (Shape)" %in% result$Param)
  expect_true("Eta @ Stress 300" %in% result$Param)
  expect_true("AF @ Stress 300" %in% result$Param)
  expect_true("R\u00b2 (Life-Stress)" %in% result$Param)

  # AF at lowest stress must be 1
  af_low <- as.numeric(result$Value[result$Param == "AF @ Stress 300"])
  expect_equal(af_low, 1)

  # R² must be in [0, 1]
  r2 <- as.numeric(result$Value[result$Param == "R\u00b2 (Life-Stress)"])
  expect_true(r2 >= 0 && r2 <= 1)
})

test_that("extract_alt_summ works with Meeker data (Power Law)", {
  dat <- read.csv(system.file("app", "data", "meekerData.csv", package = "ReliaShiny"))
  stress_levels <- sort(unique(dat$stress))

  alt_list <- lapply(stress_levels, function(s) {
    sub <- dat[dat$stress == s, ]
    WeibullR.ALT::alt.data(sub$time, stress = s)
  })
  obj <- WeibullR.ALT::alt.fit(
    WeibullR.ALT::alt.parallel(
      WeibullR.ALT::alt.make(alt_list, dist = "weibull", alt.model = "power",
                             view_dist_fits = FALSE),
      view_parallel_fits = FALSE
    )
  )

  result <- extract_alt_summ(obj)

  expect_equal(nrow(result), 13)
  expect_equal(result$Value[result$Param == "ALT Model"], "power")
})
