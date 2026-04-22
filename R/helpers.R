#' Extract RGA summary as a data frame
#'
#' @param rga_obj Object returned by \code{ReliaGrowR::rga()}.
#' @param digits Number of decimal places to round numeric values.
#' @return A two-column data frame with \code{Param} and \code{Value}.
#' @export
extract_rga_summ <- function(rga_obj, digits = 4) {
  model_type <- if (!is.null(rga_obj$breakpoints)) "Piecewise NHPP" else "Crow-AMSAA"

  total_failures <- rga_obj$n_obs
  growth_rates   <- round(as.numeric(rga_obj$growth_rate), digits)
  betas          <- 1 - growth_rates
  lambdas        <- round(as.numeric(rga_obj$lambdas), digits)
  fit_stats      <- round(c(LogLik = rga_obj$logLik, AIC = rga_obj$AIC, BIC = rga_obj$BIC), digits)

  make_names <- function(base, vals) {
    if (length(vals) > 1) paste0(base, "[", seq_along(vals), "]") else base
  }

  params <- c(
    "Model Type", "Total Failures",
    make_names("Beta", betas),
    make_names("Growth Rate", growth_rates),
    make_names("Lambda", lambdas),
    names(fit_stats)
  )

  values <- c(
    list(model_type), list(total_failures),
    as.list(betas), as.list(growth_rates), as.list(lambdas), as.list(fit_stats)
  )

  data.frame(Param = params, Value = I(values), stringsAsFactors = FALSE)
}

#' Extract NHPP summary as a data frame
#'
#' @param nhpp_obj Object returned by \code{ReliaGrowR::nhpp()}.
#' @param digits Number of decimal places to round numeric values.
#' @return A two-column data frame with \code{Param} and \code{Value}.
#' @export
extract_nhpp_summ <- function(nhpp_obj, digits = 4) {
  model_type  <- nhpp_obj$model_type
  n_obs       <- nhpp_obj$n_obs
  params      <- round(as.numeric(nhpp_obj$params), digits)
  param_names <- names(nhpp_obj$params)
  fit_stats   <- round(c(LogLik = nhpp_obj$logLik, AIC = nhpp_obj$AIC, BIC = nhpp_obj$BIC), digits)

  all_params <- c("Model Type", "Total Events", param_names, names(fit_stats))
  all_values <- c(model_type, as.character(n_obs), as.character(params), as.character(fit_stats))

  data.frame(Param = all_params, Value = all_values, stringsAsFactors = FALSE)
}

#' Extract ALT summary as a data frame
#'
#' @param alt_obj Object returned by \code{WeibullR.ALT::alt.fit()}.
#' @param digits Number of decimal places to round numeric values.
#' @return A two-column data frame with \code{Param} and \code{Value}.
#' @export
extract_alt_summ <- function(alt_obj, digits = 4) {
  coefs  <- round(as.numeric(alt_obj$alt_coef), digits)
  beta   <- round(alt_obj$parallel_par$P2[1], digits)
  etas   <- round(alt_obj$parallel_par$P1, digits)
  stress <- alt_obj$parallel_par$stress
  af     <- round(etas[1] / etas, digits)

  pp   <- alt_obj$parallel_par
  x_tr <- if (alt_obj$alt.model == "arrhenius") 1 / pp$stress else log(pp$stress)
  lm_f <- stats::lm(log(pp$P1) ~ x_tr, weights = pp$wt)
  lm_s <- summary(lm_f)
  r2     <- round(lm_s$r.squared,     digits)
  adj_r2 <- round(lm_s$adj.r.squared, digits)
  loglik <- round(as.numeric(stats::logLik(lm_f)), digits)
  aic    <- round(stats::AIC(lm_f), digits)
  bic    <- round(stats::BIC(lm_f), digits)

  data.frame(
    Param = c("Distribution", "ALT Model", "Intercept", "Slope", "Beta (Shape)",
              paste0("Eta @ Stress ", stress),
              paste0("AF @ Stress ", stress),
              "R\u00b2 (Life-Stress)", "Adj. R\u00b2 (Life-Stress)",
              "LogLik (Life-Stress)", "AIC (Life-Stress)", "BIC (Life-Stress)"),
    Value = c(alt_obj$dist, alt_obj$alt.model,
              as.character(coefs),
              as.character(beta),
              as.character(etas),
              as.character(af),
              as.character(r2),
              as.character(adj_r2),
              as.character(loglik),
              as.character(aic),
              as.character(bic)),
    stringsAsFactors = FALSE
  )
}

#' Extract WeibullR summary as a data frame
#'
#' @param wblr_obj Object returned by \code{WeibullR::wblr.fit()}.
#' @param digits Number of decimal places to round numeric values.
#' @return A two-column data frame with \code{Param} and \code{Value}.
#' @export
extract_wblr_summ <- function(wblr_obj, digits = 4) {
  fit_opts <- wblr_obj$fit[[1]]$options
  fit_vec  <- as.numeric(wblr_obj$fit[[1]]$fit_vec)
  gof      <- wblr_obj$fit[[1]]$gof

  model_type <- switch(
    fit_opts$dist,
    weibull   = "Weibull",
    weibull3p = "Weibull 3P",
    lognormal = "Lognormal",
    "Unknown"
  )

  if (fit_opts$dist == "lognormal") {
    params <- c("Mulog", "Sigmalog")
    values <- round(fit_vec[1:2], digits)
  } else if (fit_opts$dist == "weibull") {
    params <- c("Beta", "Eta")
    values <- round(c(fit_vec[2], fit_vec[1]), digits)
  } else if (fit_opts$dist == "weibull3p") {
    params <- c("Beta", "Eta", "Gamma")
    values <- round(c(fit_vec[2], fit_vec[1], fit_vec[3]), digits)
  } else {
    params <- character()
    values <- numeric()
  }

  methlab <- methval <- NULL
  if (!is.null(fit_opts$method.fit)) {
    if (fit_opts$method.fit == "rr-xony" && !is.null(gof$r2)) {
      methlab <- "R^2"
      methval <- round(gof$r2, digits)
    } else if (fit_opts$method.fit == "mle" && !is.null(gof$loglik)) {
      methlab <- "Log-likelihood"
      methval <- round(gof$loglik, digits)
    }
  }

  total_events      <- if (!is.null(wblr_obj$n))        wblr_obj$n        else NA
  total_failures    <- if (!is.null(wblr_obj$fail))     wblr_obj$fail     else NA
  total_intervals   <- if (!is.null(wblr_obj$interval)) wblr_obj$interval else NA
  total_suspensions <- if (!is.null(wblr_obj$cens))     wblr_obj$cens     else NA

  Param <- c(
    "Model Type", "Total Events", "Total Failures", "Total Intervals", "Total Suspensions",
    params,
    if (!is.null(methlab)) methlab
  )

  Value <- c(
    model_type,
    as.character(c(total_events, total_failures, total_intervals, total_suspensions)),
    as.character(values),
    if (!is.null(methval)) as.character(methval)
  )

  data.frame(Param = Param, Value = Value, stringsAsFactors = FALSE)
}
