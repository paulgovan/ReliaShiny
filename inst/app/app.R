
# Helper function for providing default values
`%then%` <- function(a, b) {
    if (is.null(a)) b else a
}

# Function to extract RGA summary data
extract_rga_summ <- function(rga_obj, digits = 4) {
  # Determine model type
  model_type <- if (!is.null(rga_obj$breakpoints)) "Piecewise NHPP" else "Crow-AMSAA"

  # Extract and round key stats
  total_failures <- rga_obj$n_obs
  growth_rates <- round(as.numeric(rga_obj$growth_rate), digits)
  betas <- 1 - growth_rates
  lambdas <- round(as.numeric(rga_obj$lambdas), digits)
  fit_stats <- round(c(LogLik = rga_obj$logLik, AIC = rga_obj$AIC, BIC = rga_obj$BIC), digits)

  # Helper to create indexed names if needed
  make_names <- function(base, vals) {
    if (length(vals) > 1) paste0(base, "[", seq_along(vals), "]") else base
  }

  # Assemble parameter names and values
  params <- c(
    "Model Type",
    "Total Failures",
    make_names("Beta", betas),
    make_names("Growth Rate", growth_rates),
    make_names("Lambda", lambdas),
    names(fit_stats)
  )

  values <- c(
    list(model_type),
    list(total_failures),
    as.list(betas),
    as.list(growth_rates),
    as.list(lambdas),
    as.list(fit_stats)
  )

  # Return data frame
  data.frame(Param = params, Value = I(values), stringsAsFactors = FALSE)
}

# Function to extract NHPP summary data
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

# Function to extract ALT summary data
extract_alt_summ <- function(alt_obj, digits = 4) {
  coefs  <- round(as.numeric(alt_obj$alt_coef), digits)
  beta   <- round(alt_obj$parallel_par$P2[1], digits)
  etas   <- round(alt_obj$parallel_par$P1, digits)
  stress <- alt_obj$parallel_par$stress
  af     <- round(etas[1] / etas, digits)

  # Reconstruct the life-stress linear model for GoF metrics
  pp   <- alt_obj$parallel_par
  x_tr <- if (alt_obj$alt.model == "arrhenius") 1 / pp$stress else log(pp$stress)
  lm_s <- summary(stats::lm(log(pp$P1) ~ x_tr, weights = pp$wt))
  r2     <- round(lm_s$r.squared,     digits)
  adj_r2 <- round(lm_s$adj.r.squared, digits)

  data.frame(
    Param = c("Distribution", "ALT Model", "Intercept", "Slope", "Beta (Shape)",
              paste0("Eta @ Stress ", stress),
              paste0("AF @ Stress ", stress),
              "R\u00b2 (Life-Stress)", "Adj. R\u00b2 (Life-Stress)"),
    Value = c(alt_obj$dist, alt_obj$alt.model,
              as.character(coefs),
              as.character(beta),
              as.character(etas),
              as.character(af),
              as.character(r2),
              as.character(adj_r2)),
    stringsAsFactors = FALSE
  )
}

# Function to extract WeibullR summary data
extract_wblr_summ <- function(wblr_obj, digits = 4) {

  # Extract fitting options and fitted values
  fit_opts <- wblr_obj$fit[[1]]$options
  fit_vec  <- as.numeric(wblr_obj$fit[[1]]$fit_vec)
  gof      <- wblr_obj$fit[[1]]$gof

  # Identify model type
  model_type <- switch(
    fit_opts$dist,
    weibull   = "Weibull",
    weibull3p = "Weibull 3P",
    lognormal = "Lognormal",
    "Unknown"
  )

  # Extract parameter names and values
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

  # Add goodness-of-fit statistic
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

  # Totals
  total_events      <- if (!is.null(wblr_obj$n)) wblr_obj$n else NA
  total_failures    <- if (!is.null(wblr_obj$fail)) wblr_obj$fail else NA
  total_intervals   <- if (!is.null(wblr_obj$interval)) wblr_obj$interval else NA
  total_suspensions <- if (!is.null(wblr_obj$cens)) wblr_obj$cens else NA

  # Build final key/value pairs
  Param <- c(
    "Model Type",
    "Total Events",
    "Total Failures",
    "Total Intervals",
    "Total Suspensions",
    params,
    if (!is.null(methlab)) methlab
  )

  Value <- c(
    model_type,
    as.character(c(total_events, total_failures, total_intervals, total_suspensions)),
    as.character(values),
    if (!is.null(methval)) as.character(methval)
  )

  # Return tidy two-column data.frame
  data.frame(
    Param = Param,
    Value = Value,
    stringsAsFactors = FALSE
  )
}

# Color palette constants for selectInput choices
COLORS_FULL <- c("black", "blue", "red", "yellow", "green", "orange", "violet")
COLORS_LINE <- c("blue", "red", "yellow", "green", "orange", "violet")
COLORS_GRID <- c("lightgray", "black", "blue", "red", "yellow", "green", "orange", "violet")

# Define UI for application
ui <- shinydashboard::dashboardPage(
    skin = "red",
    shinydashboard::dashboardHeader(title = "ReliaShiny"),

    ## Sidebar content
    shinydashboard::dashboardSidebar(
        shinydashboard::sidebarMenu(id = "sidebarMenu",
            shinydashboard::menuItem("Landing", tabName = "landing", icon = icon("helicopter-symbol")),
            shinydashboard::menuItem("Life Data", tabName = "ttf", icon = icon("hourglass-half"),
              shinydashboard::menuSubItem("Data", tabName = "data", icon = shiny::icon("table")),
              shinydashboard::menuSubItem("Model", tabName = "model", icon = icon("chart-line"))
            ),
            shinydashboard::menuItem("Reliability Growth", tabName = "rg", icon = icon("stairs"),
              shinydashboard::menuSubItem("Data", tabName = "growthData", icon = shiny::icon("table")),
              shinydashboard::menuSubItem("Model", tabName = "growthModel", icon = icon("chart-line"))
            ),
            shinydashboard::menuItem("Repairable Systems", tabName = "rs", icon = icon("wrench"),
              shinydashboard::menuSubItem("Data",  tabName = "rsData",  icon = shiny::icon("table")),
              shinydashboard::menuSubItem("Model", tabName = "rsModel", icon = icon("chart-line"))
            ),
            shinydashboard::menuItem("Accelerated Life Testing", tabName = "alt", icon = icon("bolt"),
              shinydashboard::menuSubItem("Data",  tabName = "altData",  icon = shiny::icon("table")),
              shinydashboard::menuSubItem("Model", tabName = "altModel", icon = icon("chart-line"))
            ),
            shiny::br(),
            shiny::bookmarkButton()
        )
    ),

    ## Body content
    shinydashboard::dashboardBody(shinydashboard::tabItems(
        # First tab content
        shinydashboard::tabItem(tabName = "landing",
                                shiny::fluidRow(
                                  shiny::column(
                                    width = 8,
                                    shinydashboard::box(
                                        width = 12,
                                        shiny::img(
                                          src = "hexSticker.png",
                                          height = 200,
                                          width = 175
                                        ),
                                        h2("ReliaShiny"),
                                        shiny::h4(
                                            "A Shiny App for Reliability Analysis"
                                            ),
                                        br(),
                                        shiny::h4(
                                          tags$b("Welcome to ReliaShiny!"),
                                            "ReliaShiny is an interactive web application for reliability analysis. The app is built using ",
                                            shiny::a(href = 'https://www.r-project.org/', 'R'),
                                            " and the ",shiny::a(href = 'https://shiny.rstudio.com/', 'shiny'
                                            )," package. ReliaShiny provides an easy-to-use interface for performing reliability analysis using the ",shiny::a(href = 'https://cran.r-project.org/web/packages/WeibullR/index.html', 'WeibullR')," and " ,shiny::a(href = 'https://cran.r-project.org/web/packages/ReliaGrowR/index.html', 'ReliaGrowR')," packages."
                                            )
                                    )
                                  ),
                                  shiny::column(
                                    width = 4,
                                    shinydashboard::box(
                                        title = "Links",
                                        width = 12,
                                        shiny::h4(
                                            "For help getting started, visit the ",
                                            shiny::a(href = 'https://paulgovan.github.io/ReliaShiny/', 'Project Site'),
                                            " for documentation and tutorials"
                                            ),
                                        shiny::h4(
                                          "To view the source code, visit the ",
                                          shiny::a(href = 'https://github.com/paulgovan/ReliaShiny/', 'GitHub Repository')
                                        ),
                                        shiny::h4(
                                            "To report bugs or request features, open a ",
                                            shiny::a(href = 'https://github.com/paulgovan/ReliaShiny/issues', 'GitHub Issue')
                                        )
                                    ),
                                    shinydashboard::box(
                                        title = "Development",
                                        width = 12,
                                        shiny::h4("Author"),
                                        shiny::h5(
                                          tags$a(href = "https://github.com/paulgovan", "Paul Govan")
                                        ),

                                        shiny::h4("License"),
                                        shiny::h5(
                                          tags$a(href = "https://creativecommons.org/licenses/by/4.0", "CC BY 4.0 License")
                                        ),
                                        shiny::h4("Citation"),
                                        shiny::h5(
                                          tags$a(href = "https://paulgovan.github.io/ReliaGrowR/authors.html#citation", "Citing ReliaGrowR")

                                    )
                                  )
                                )
                                ),
                                shiny::fluidRow(
                                  shinydashboard::infoBox(
                                    "Life Data",
                                    "Weibull analysis",
                                    icon = icon("hourglass-half"), color = "red", fill = TRUE, width = 3
                                  ),
                                  shinydashboard::infoBox(
                                    "Reliability Growth",
                                    "NHPP growth modeling",
                                    icon = icon("stairs"), color = "red", fill = TRUE, width = 3
                                  ),
                                  shinydashboard::infoBox(
                                    "Repairable Systems",
                                    "Recurrent event analysis",
                                    icon = icon("wrench"), color = "red", fill = TRUE, width = 3
                                  ),
                                  shinydashboard::infoBox(
                                    "Accelerated Life Testing",
                                    "Life-stress modeling",
                                    icon = icon("bolt"), color = "red", fill = TRUE, width = 3
                                  )
                                ),
                                shiny::fluidRow(
                                  shiny::column(width = 3, align = "center", shiny::actionButton("goToLifeData", "Take Me There", class = "btn-default btn-sm")),
                                  shiny::column(width = 3, align = "center", shiny::actionButton("goToGrowth",   "Take Me There", class = "btn-default btn-sm")),
                                  shiny::column(width = 3, align = "center", shiny::actionButton("goToRepair",   "Take Me There", class = "btn-default btn-sm")),
                                  shiny::column(width = 3, align = "center", shiny::actionButton("goToAlt",      "Take Me There", class = "btn-default btn-sm"))
                                )
        ),

        # Weibull data tab content
        shinydashboard::tabItem(tabName = "data",
                                shiny::fluidRow(
                                  shiny::column(
                                    width = 3,
                                    shinydashboard::box(
                                      title = "Data Input",
                                      width = NULL,
                                      collapsible = TRUE,
                                      shiny::helpText("Select a sample data set or upload your Time-to-Failure data:"),

                                      shinyWidgets::radioGroupButtons(inputId = "dataInput",
                                                                      choices = c("Sample Data" = 1,
                                                                                  "Upload Data" = 2),
                                                                      selected = 1,
                                                                      justified = TRUE
                                      ),

                                      # Conditional panel for sample data selection
                                      shiny::conditionalPanel(
                                        condition = "input.dataInput == 1",

                                        # Demo network input select
                                        shiny::selectInput(
                                          inputId = "dataSelect",
                                          h5("Time-to-Failure Data:"),
                                          c("End-of-Life Data" = 1,
                                            "Right Censored Data" = 2
                                          )
                                        )
                                      ),

                                      # Conditional panel for file input selection
                                      shiny::conditionalPanel(
                                        condition = "input.dataInput == 2",

                                        shiny::helpText("Upload a CSV with 'time'/'event' columns, or 'left'/'right' columns for interval data."),

                                        # File input
                                        shiny::fileInput(
                                          'file',
                                          strong('File Input:'),
                                          accept = c('text/csv',
                                                     'text/comma-separated-values',
                                                     'text/tab-separated-values',
                                                     'text/plain',
                                                     '.csv',
                                                     '.tsv'
                                          )
                                        )
                                      )
                                    ),
                                    shinydashboard::box(
                                      title = "Data Selection",
                                      width = NULL,
                                      collapsible = TRUE,
                                      shiny::helpText("Arrange your data for analysis:"),
                                      # Suspensions checkbox
                                      shiny::checkboxInput("suspensions",
                                                           label = "My data table contains suspensions"),
                                      # Intervals checkbox
                                      shiny::checkboxInput("intervals",
                                                           label = "My data table contains intervals"),
                                      # Groups checkbox
                                      shiny::checkboxInput("groups",
                                                           label = "My data table contains groups"),

                                      # Conditional panel for time column
                                      shiny::conditionalPanel(
                                        condition = "input.intervals == 0",

                                        # Time column
                                        shiny::selectizeInput(
                                          inputId = "time",
                                          h5("Time to failure column:"),
                                          c(""),
                                          selected = 1
                                        ),
                                        shiny::helpText("Time-to-Failure column must contain postive numbers (1, 2, 3 ...)")
                                      ),

                                      # Conditional panel for suspensions
                                      shiny::conditionalPanel(
                                        condition = "input.suspensions == 1",

                                        # Event column
                                        shiny::selectizeInput(
                                          inputId = "event",
                                          h5("Event type column:"),
                                          c(""),
                                          selected = 2
                                        ),
                                        shiny::helpText("Event column must be binary (e.g. 1 for Failure, 0 for Suspension)")

                                      ),

                                      # Conditional panel for groups
                                      shiny::conditionalPanel(
                                        condition = "input.groups == 1",

                                        # Quantity column
                                        shiny::selectizeInput(
                                          inputId = "qty",
                                          h5("Group column:"),
                                          c(""),
                                          selected = 3
                                        ),
                                        shiny::helpText("Group column must contain positive values (1, 2, 3 ...)")
                                      ),

                                      # Conditional panel for intervals
                                      shiny::conditionalPanel(
                                        condition = "input.intervals == 1",

                                        # Left interval column
                                        shiny::selectizeInput(
                                          inputId = "left",
                                          h5("Left-interval column:"),
                                          c(""),
                                          selected = 1
                                        ),
                                        shiny::helpText("Left column must contain positive values (1, 2, 3 ...)"),

                                        # Right interval column
                                        shiny::selectizeInput(
                                          inputId = "right",
                                          h5("Right-interval column:"),
                                          c(""),
                                          selected = 2
                                        ),
                                        shiny::helpText("Right column must contain positive values (1, 2, 3 ...)")
                                      )
                                    )
                                  ),
                                  shiny::column(
                                    width = 6,
                                    shinydashboard::box(
                                      title = "Data Table",
                                      width = NULL,
                                      shiny::tableOutput("table")
                                    )
                                  ),
                                  shiny::column(
                                    width = 3,
                                    # Events value box
                                    shiny::uiOutput("eventBox"),
                                    # Failures value box
                                    shiny::uiOutput("failBox"),
                                    # Suspensions value box
                                    shiny::uiOutput("suspBox")
                                  )
                                )),

        # Weibull model tab content
        shinydashboard::tabItem(tabName = "model",
                                shiny::fluidRow(
                                  shinydashboard::box(
                                    title = "Model Selection",
                                    width = 3,
                                    collapsible = TRUE,
                                    shiny::helpText("Select the type of model to perform:"),
                                    # Distribution input
                                    shiny::selectInput(
                                      inputId = "dist",
                                      h5("Distribution:"),
                                      c(
                                        "Weibull 2P" = "weibull",
                                        "Weibull 3P" = "weibull3p",
                                        "Lognormal" = "lognormal"
                                      ),
                                      selected = "weibull"
                                    ),
                                    # Method input
                                    shiny::selectInput(
                                      inputId = "meth",
                                      h5("Estimation Method:"),
                                      c(
                                        "Maximum Likelihood" = "mle",
                                        "Rank Regression XY" = "rr-xony"
                                      )
                                    ),
                                    # Plotting position
                                    shiny::selectInput(
                                      inputId = "pp",
                                      h5("Plotting Position Method:"),
                                      c(
                                        "Median" = "median",
                                        "Hazen" = "hazen",
                                        "Mean" = "mean",
                                        "Kaplan-Meier" = "kaplan-meier",
                                        "Blom" = "blom"
                                      )
                                    ),
                                    # Confidence Method
                                    # Conditional Panel for MLE
                                    shiny::conditionalPanel(
                                      condition = "input.meth == 'mle'",
                                      shiny::selectInput(inputId = "mleConf",
                                                         h5("Confidence Method:"),
                                                         c("LRB" = "lrb",
                                                           "FM" = "fm",
                                                           "FMbounds" = "fmbounds"))
                                    ),
                                    # Conditional Panel for RR
                                    shiny::conditionalPanel(
                                      condition = "input.meth == 'rr-xony'",
                                      shiny::selectInput(inputId = "rrConf",
                                                         h5("Confidence Method:"),
                                                         c("Pivotal-RR" = "pivotal-rr"))
                                    ),
                                    shiny::sliderInput(inputId = "cl", h5("Confidence Level: "),
                                                       min = 0, max = 0.99, value = 0.9, step = 0.1)
                                  ),
                                  shinydashboard::tabBox(
                                    title = "Model Results",
                                    # The id lets us use input$tabset1 on the server to find the current tab
                                    id = "tabset1",
                                    width = 9,
                                    shiny::tabPanel(
                                      "Probability Plot",
                                      shiny::fluidRow(
                                        shiny::column(width = 8,
                                                      shinyWidgets::dropdownButton(
                                                        h3("Plot Options"),
                                                        # Probability color
                                                        shiny::selectInput(
                                                          inputId = "probcol",
                                                          h5("Probability Points:"),
                                                          COLORS_FULL,
                                                          selected = "black"
                                                        ),
                                                        # Fit color
                                                        shiny::selectInput(
                                                          inputId = "fitcol",
                                                          h5("Fit:"),
                                                          COLORS_LINE,
                                                          selected = "blue"
                                                        ),
                                                        # CB color
                                                        shiny::selectInput(
                                                          inputId = "confcol",
                                                          h5("Confidence Bounds:"),
                                                          COLORS_LINE,
                                                          selected = "blue"
                                                        ),
                                                        # Grid color
                                                        shiny::selectInput(
                                                          inputId = "gridcol",
                                                          h5("Grid:"),
                                                          COLORS_GRID,
                                                          selected = "lightgray"
                                                        ),
                                                        # Main title
                                                        shiny::textInput(inputId = "main",
                                                                         h5("Title:"),
                                                                         value = "Probability Plot"),
                                                        # Xlab
                                                        shiny::textInput(inputId = "xlab",
                                                                         h5("X-axis Label:"),
                                                                         value = "Time to Failure"),
                                                        # Ylab
                                                        shiny::textInput(inputId = "ylab",
                                                                         h5("Y-axis Label:"),
                                                                         value = "Failure Probability (%)"),
                                                        # Significant digits
                                                        shiny::numericInput(
                                                          inputId = "signif",
                                                          h5("Significant Digits:"),
                                                          value = 3
                                                        ),
                                                        # Show suspensions plot
                                                        shiny::checkboxInput("suspPlot",
                                                                             label = "Show suspensions",
                                                                             value = TRUE),
                                                        # Show grid
                                                        shiny::checkboxInput("grid",
                                                                             label = "Show grid",
                                                                             value = TRUE),
                                                      circle = TRUE,
                                                      status = "danger",
                                                      icon = icon("gear")
                                        ),
                                        shinycssloaders::withSpinner(plotly::plotlyOutput('probPlot'), type = 6, color = "#dd4b39")
                                        ),
                                        shiny::column(width = 3,
                                                      shiny::tableOutput("wblr_results"),
                                                      shiny::downloadButton("downloadWblrResults", "Export CSV",
                                                                            style = "margin-top:8px;")
                                      )
                                      )
                                    ),
                                    shiny::tabPanel("Contour Plot",
                                                    shiny::fluidRow(
                                                      shiny::column(width = 12,
                                                                    shinyWidgets::dropdownButton(
                                                                      h3("Plot Options"),
                                                                      # Plot color
                                                                      shiny::selectInput(
                                                                        inputId = "col2",
                                                                        h5("Plot Color:"),
                                                                        COLORS_FULL,
                                                                        selected = "blue"
                                                                      ),
                                                                      # Grid color
                                                                      shiny::selectInput(
                                                                        inputId = "gridcol2",
                                                                        h5("Grid Color:"),
                                                                        COLORS_GRID,
                                                                        selected = "lightgray"
                                                                      ),
                                                                      # Show grid
                                                                      shiny::checkboxInput("grid2",
                                                                                           label = "Show grid",
                                                                                           value = TRUE),
                                                                      # Main title
                                                                      shiny::textInput(inputId = "main2",
                                                                                       h5("Title:"),
                                                                                       value = "Contour Plot"),
                                                                      # Xlab
                                                                      shiny::textInput(inputId = "xlab2",
                                                                                       h5("X-axis Label:"),
                                                                                       value = "Eta"),
                                                                      # Ylab
                                                                      shiny::textInput(inputId = "ylab2",
                                                                                       h5("Y-axis Label:"),
                                                                                       value = "Beta"),
                                                                      # Significant digits
                                                                      shiny::numericInput(
                                                                        inputId = "signif2",
                                                                        h5("Significant Digits:"),
                                                                        value = 3
                                                                      ),
                                                                      circle = TRUE,
                                                                      status = "danger",
                                                                      icon = icon("gear")
                                                                    ),
                                                                    shinycssloaders::withSpinner(plotly::plotlyOutput('contPlot'), type = 6, color = "#dd4b39")
                                                      )
                                                    )
                                    )
                                  )
                                )
        ),

        # RGA data tab content
        shinydashboard::tabItem(tabName = "growthData",
                                shiny::fluidRow(
                                  shiny::column(
                                    width = 3,
                                    shinydashboard::box(
                                      title = "Data Input",
                                      width = NULL,
                                      collapsible = TRUE,
                                      shiny::helpText("Select a sample data set or upload your Reliability Growth data:"),

                                      shinyWidgets::radioGroupButtons(inputId = "growthDataInput",
                                                                      choices = c("Sample Data" = 1,
                                                                                  "Upload Data" = 2),
                                                                      selected = 1,
                                                                      justified = TRUE
                                      ),

                                      # Conditional panel for sample data selection
                                      shiny::conditionalPanel(
                                        condition = "input.growthDataInput == 1",

                                        # Demo data input select
                                        shiny::selectInput(
                                          inputId = "growthDataSelect",
                                          h5("Data:"),
                                          c("Simple Data Set" = 1,
                                            "Large Data Set" = 2
                                          )
                                        )
                                      ),

                                      # Conditional panel for file input selection
                                      shiny::conditionalPanel(
                                        condition = "input.growthDataInput == 2",

                                        shiny::helpText("Upload a CSV with 'cumulative time' and 'failure' columns."),

                                        # File input
                                        shiny::fileInput(
                                          'growthFile',
                                          strong('File Input:'),
                                          accept = c('text/csv',
                                                     'text/comma-separated-values',
                                                     'text/tab-separated-values',
                                                     'text/plain',
                                                     '.csv',
                                                     '.tsv'
                                          )
                                        )
                                      )
                                    ),
                                    shinydashboard::box(
                                      title = "Data Selection",
                                      width = NULL,
                                      collapsible = TRUE,
                                      shiny::helpText("Arrange your data for analysis:"),

                                      # Times column
                                      shiny::selectizeInput(
                                        inputId = "times",
                                        h5("Cumulative Time Column:"),
                                        c(""),
                                        selected = 1
                                      ),
                                      shiny::helpText("Cumulative time column must contain postive numbers (1, 2, 3 ...)"),

                                      # Failures column
                                      shiny::selectizeInput(
                                        inputId = "failures",
                                        h5("Failure Column:"),
                                        c(""),
                                        selected = 2
                                      ),
                                      shiny::helpText("Failure column must must contain postive numbers (1, 2, 3 ...)")
                                    )
                                  ),
                                  shiny::column(
                                    width = 6,
                                    shinydashboard::box(
                                      title = "Data Table",
                                      width = NULL,
                                      shiny::tableOutput("growthTable")
                                    )
                                  ),
                                  shiny::column(
                                    width = 3,
                                    # Cumulative Failures value box
                                    shiny::uiOutput("failuresBox"),
                                    # Cumulative time value box
                                    shiny::uiOutput("timesBox")
                                  )
                                )
        ),

        # Growth model tab content
        shinydashboard::tabItem(tabName = "growthModel",
                                shiny::fluidRow(
                                  shinydashboard::box(
                                    title = "Model Selection",
                                    width = 3,
                                    collapsible = TRUE,
                                    shiny::helpText("Select the type of model to perform:"),
                                    # Distribution input
                                    shiny::selectInput(
                                      inputId = "growthModel",
                                      h5("Model:"),
                                      c(
                                        "Crow-AMSAA" = 1,
                                        "Piecewise Weibull NHPP" = 2,
                                        "Piecewise Weibull NHPP with Change Point Detection" = 3
                                      ),
                                      selected = "Crow-AMSAA"
                                    ),
                                    # Conditional panel for Piecewise Weibull NHPP
                                    shiny::conditionalPanel(
                                      condition = "input.growthModel == 2",

                                      shiny::helpText("Enter the breakpoint for the Piecewise Weibull:"),
                                      shiny::numericInput(
                                        inputId = "breakpoints",
                                        h5("Breakpoint:"),
                                        value = 1,
                                        min = 0.1,
                                        step = 0.1
                                      )
                                    ),

                                    shiny::sliderInput(inputId = "growthConf", h5("Confidence Level: "),
                                                       min = 0, max = 0.99, value = 0.9, step = 0.1)
                                  ),
                                  shinydashboard::tabBox(
                                    title = "Model Results",
                                    # The id lets us use input$tabset2 on the server to find the current tab
                                    id = "tabset2",
                                    width = 9,
                                    shiny::tabPanel(
                                      "Reliability Growth Plot",
                                      shiny::fluidRow(
                                        shiny::column(width = 8,
                                                      shinyWidgets::dropdownButton(
                                                        h3("Plot Options"),
                                                        # Points color
                                                        shiny::selectInput(
                                                          inputId = "pointCol",
                                                          h5("Failure Points:"),
                                                          COLORS_FULL,
                                                          selected = "black"
                                                        ),
                                                        # Fit color
                                                        shiny::selectInput(
                                                          inputId = "modelCol",
                                                          h5("Fit:"),
                                                          COLORS_LINE,
                                                          selected = "blue"
                                                        ),
                                                        # CB color
                                                        shiny::selectInput(
                                                          inputId = "growthConfCol",
                                                          h5("Confidence Bounds:"),
                                                          COLORS_LINE,
                                                          selected = "blue"
                                                        ),
                                                        # Grid color
                                                        shiny::selectInput(
                                                          inputId = "growthGridCol",
                                                          h5("Grid:"),
                                                          COLORS_GRID,
                                                          selected = "lightgray"
                                                        ),
                                                        # Breakpoint color
                                                        shiny::selectInput(
                                                          inputId = "breakCol",
                                                          h5("Breakpoints:"),
                                                          COLORS_LINE,
                                                          selected = "black"
                                                        ),
                                                        # Main title
                                                        shiny::textInput(inputId = "growthMain",
                                                                         h5("Title:"),
                                                                         value = "Reliability Growth Plot"),
                                                        # Xlab
                                                        shiny::textInput(inputId = "growthXlab",
                                                                         h5("X-axis Label:"),
                                                                         value = "Cumulative Time"),
                                                        # Ylab
                                                        shiny::textInput(inputId = "growthYlab",
                                                                         h5("Y-axis Label:"),
                                                                         value = "Cumulative Failures"),
                                                        circle = TRUE,
                                                        status = "danger",
                                                        icon = icon("gear")
                                                      ),
                                                      shinycssloaders::withSpinner(plotly::plotlyOutput('growthPlot'), type = 6, color = "#dd4b39")
                                        ),
                                        shiny::column(width = 3,
                                                      shiny::tableOutput("rga_results"),
                                                      shiny::downloadButton("downloadRgaResults", "Export CSV",
                                                                            style = "margin-top:8px;")
                                        )
                                      )
                                    ),
                                    shiny::tabPanel("Duane Plot",
                                                    shiny::fluidRow(
                                                      shiny::column(width = 12,
                                                                    shinyWidgets::dropdownButton(
                                                                      h3("Plot Options"),
                                                                      # Plot color
                                                                      shiny::selectInput(
                                                                        inputId = "pointCol2",
                                                                        h5("MTBF Points:"),
                                                                        COLORS_FULL,
                                                                        selected = "black"
                                                                      ),
                                                                      # Line color
                                                                      shiny::selectInput(
                                                                        inputId = "modelCol2",
                                                                        h5("Fit:"),
                                                                        COLORS_LINE,
                                                                        selected = "blue"
                                                                      ),
                                                                      # CB color
                                                                      shiny::selectInput(
                                                                        inputId = "growthConfCol2",
                                                                        h5("Confidence Bounds:"),
                                                                        COLORS_LINE,
                                                                        selected = "blue"
                                                                      ),
                                                                      # Grid color
                                                                      shiny::selectInput(
                                                                        inputId = "growthGridCol2",
                                                                        h5("Grid:"),
                                                                        COLORS_GRID,
                                                                        selected = "lightgray"
                                                                      ),
                                                                      # Main title
                                                                      shiny::textInput(inputId = "duaneMain",
                                                                                       h5("Title:"),
                                                                                       value = "Duane Plot"),
                                                                      # Xlab
                                                                      shiny::textInput(inputId = "duaneXlab",
                                                                                       h5("X-axis Label:"),
                                                                                       value = "Cumulative Time"),
                                                                      # Ylab
                                                                      shiny::textInput(inputId = "duaneYlab",
                                                                                       h5("Y-axis Label:"),
                                                                                       value = "Cumulative MTBF"),
                                                                      circle = TRUE,
                                                                      status = "danger",
                                                                      icon = icon("gear")
                                                                    ),
                                                                    shinycssloaders::withSpinner(plotly::plotlyOutput('duanePlot'), type = 6, color = "#dd4b39"),
                                                                    shiny::downloadButton("downloadDuane", "Export CSV",
                                                                                          style = "margin-top:8px;")
                                                      )
                                                    )
                                    )
                                  )
                                )
        ),

        # Repairable Systems data tab
        shinydashboard::tabItem(tabName = "rsData",
                                shiny::fluidRow(
                                  shiny::column(
                                    width = 3,
                                    shinydashboard::box(
                                      title = "Data Input",
                                      width = NULL,
                                      collapsible = TRUE,
                                      shiny::helpText("Select a sample data set or upload your Repairable Systems data:"),

                                      shinyWidgets::radioGroupButtons(
                                        inputId = "rsDataInput",
                                        choices = c("Sample Data" = 1, "Upload Data" = 2),
                                        selected = 1,
                                        justified = TRUE
                                      ),

                                      shiny::conditionalPanel(
                                        condition = "input.rsDataInput == 1",
                                        shiny::selectInput(
                                          inputId = "rsDataSelect",
                                          h5("Data:"),
                                          c("Simple Data Set" = 1, "Large Data Set" = 2)
                                        )
                                      ),

                                      shiny::conditionalPanel(
                                        condition = "input.rsDataInput == 2",
                                        shiny::helpText("Upload a CSV with system ID, event time, event indicator, and end time columns."),
                                        shiny::fileInput(
                                          "rsFile",
                                          strong("File Input:"),
                                          accept = c("text/csv",
                                                     "text/comma-separated-values",
                                                     "text/tab-separated-values",
                                                     "text/plain",
                                                     ".csv", ".tsv")
                                        )
                                      )
                                    ),
                                    shinydashboard::box(
                                      title = "Data Selection",
                                      width = NULL,
                                      collapsible = TRUE,
                                      shiny::helpText("Map your data columns for analysis:"),

                                      shiny::selectizeInput(
                                        inputId = "rsId",
                                        h5("System ID Column:"),
                                        c(""),
                                        selected = 1
                                      ),
                                      shiny::helpText("Column identifying each repairable system."),

                                      shiny::selectizeInput(
                                        inputId = "rsTime",
                                        h5("Event Time Column:"),
                                        c(""),
                                        selected = 2
                                      ),
                                      shiny::helpText("Time of failure or end-of-observation event."),

                                      shiny::selectizeInput(
                                        inputId = "rsEvent",
                                        h5("Event Indicator Column:"),
                                        c(""),
                                        selected = 3
                                      ),
                                      shiny::helpText("1 = failure, 0 = right-censored (end of observation)."),

                                      shiny::selectizeInput(
                                        inputId = "rsEndTime",
                                        h5("End Time Column (optional):"),
                                        c(""),
                                        selected = 4
                                      ),
                                      shiny::helpText("End of observation window per system (required for MCF).")
                                    )
                                  ),
                                  shiny::column(
                                    width = 6,
                                    shinydashboard::box(
                                      title = "Data Table",
                                      width = NULL,
                                      shiny::tableOutput("rsTable")
                                    )
                                  ),
                                  shiny::column(
                                    width = 3,
                                    shiny::uiOutput("rsSystemsBox"),
                                    shiny::uiOutput("rsEventsBox")
                                  )
                                )
        ),

        # Repairable Systems model tab
        shinydashboard::tabItem(tabName = "rsModel",
                                shiny::fluidRow(
                                  shinydashboard::box(
                                    title = "Model Selection",
                                    width = 3,
                                    collapsible = TRUE,
                                    shiny::helpText("Select the NHPP model to fit:"),

                                    shiny::selectInput(
                                      inputId = "rsNhppModel",
                                      h5("Model:"),
                                      c(
                                        "Power Law NHPP" = 1,
                                        "Log-Linear NHPP" = 2,
                                        "Piecewise NHPP" = 3,
                                        "Piecewise NHPP with Change Point Detection" = 4
                                      ),
                                      selected = 1
                                    ),

                                    shiny::conditionalPanel(
                                      condition = "input.rsNhppModel == 3",
                                      shiny::helpText("Enter the breakpoint for the Piecewise NHPP:"),
                                      shiny::numericInput(
                                        inputId = "rsBreakpoints",
                                        h5("Breakpoint:"),
                                        value = 1,
                                        min = 0.1,
                                        step = 0.1
                                      )
                                    ),

                                    shiny::selectInput(
                                      inputId = "rsMethod",
                                      h5("Estimation Method:"),
                                      c("MLE" = "MLE", "Least Squares" = "LS"),
                                      selected = "MLE"
                                    ),

                                    shiny::sliderInput(
                                      inputId = "rsConf",
                                      h5("Confidence Level:"),
                                      min = 0, max = 0.99, value = 0.9, step = 0.1
                                    )
                                  ),

                                  shinydashboard::tabBox(
                                    title = "Model Results",
                                    id = "rsTabset",
                                    width = 9,

                                    shiny::tabPanel(
                                      "NHPP Plot",
                                      shiny::fluidRow(
                                        shiny::column(
                                          width = 8,
                                          shinyWidgets::dropdownButton(
                                            h3("Plot Options"),
                                            shiny::selectInput("rsPointCol", h5("Failure Points:"), COLORS_FULL, selected = "black"),
                                            shiny::selectInput("rsModelCol", h5("Fit:"), COLORS_LINE, selected = "blue"),
                                            shiny::selectInput("rsConfCol",  h5("Confidence Bounds:"), COLORS_LINE, selected = "blue"),
                                            shiny::selectInput("rsGridCol",  h5("Grid:"), COLORS_GRID, selected = "lightgray"),
                                            shiny::selectInput("rsBreakCol", h5("Breakpoints:"), COLORS_LINE, selected = "red"),
                                            shiny::textInput("rsMain", h5("Title:"),  value = "NHPP Plot"),
                                            shiny::textInput("rsXlab", h5("X-axis Label:"), value = "Cumulative Time"),
                                            shiny::textInput("rsYlab", h5("Y-axis Label:"), value = "Cumulative Events"),
                                            circle = TRUE, status = "danger", icon = icon("gear")
                                          ),
                                          shinycssloaders::withSpinner(plotly::plotlyOutput("rsNhppPlot"), type = 6, color = "#dd4b39")
                                        ),
                                        shiny::column(
                                          width = 3,
                                          shiny::tableOutput("rs_results"),
                                          shiny::downloadButton("downloadRsResults", "Export CSV", style = "margin-top:8px;")
                                        )
                                      )
                                    ),

                                    shiny::tabPanel(
                                      "Exposure Plot",
                                      shiny::fluidRow(
                                        shiny::column(
                                          width = 12,
                                          shinyWidgets::dropdownButton(
                                            h3("Plot Options"),
                                            shiny::selectInput("rsExpFitCol",  h5("Event Rate:"), COLORS_FULL, selected = "black"),
                                            shiny::selectInput("rsExpGridCol", h5("Grid:"), COLORS_GRID, selected = "lightgray"),
                                            shiny::textInput("rsExpMain", h5("Title:"),  value = "Exposure Plot"),
                                            shiny::textInput("rsExpXlab", h5("X-axis Label:"), value = "Time"),
                                            shiny::textInput("rsExpYlab", h5("Y-axis Label:"), value = "Event Rate"),
                                            circle = TRUE, status = "danger", icon = icon("gear")
                                          ),
                                          shinycssloaders::withSpinner(plotly::plotlyOutput("rsExposurePlot"), type = 6, color = "#dd4b39"),
                                          shiny::downloadButton("downloadRsExposure", "Export CSV", style = "margin-top:8px;")
                                        )
                                      )
                                    ),

                                    shiny::tabPanel(
                                      "MCF Plot",
                                      shiny::fluidRow(
                                        shiny::column(
                                          width = 12,
                                          shinyWidgets::dropdownButton(
                                            h3("Plot Options"),
                                            shiny::selectInput("rsMcfFitCol",  h5("MCF:"), COLORS_FULL, selected = "black"),
                                            shiny::selectInput("rsMcfConfCol", h5("Confidence Bounds:"), COLORS_LINE, selected = "blue"),
                                            shiny::selectInput("rsMcfGridCol", h5("Grid:"), COLORS_GRID, selected = "lightgray"),
                                            shiny::textInput("rsMcfMain", h5("Title:"),  value = "MCF Plot"),
                                            shiny::textInput("rsMcfXlab", h5("X-axis Label:"), value = "Time"),
                                            shiny::textInput("rsMcfYlab", h5("Y-axis Label:"), value = "Mean Cumulative Function"),
                                            circle = TRUE, status = "danger", icon = icon("gear")
                                          ),
                                          shinycssloaders::withSpinner(plotly::plotlyOutput("rsMcfPlot"), type = 6, color = "#dd4b39"),
                                          shiny::downloadButton("downloadRsMcf", "Export CSV", style = "margin-top:8px;")
                                        )
                                      )
                                    )
                                  )
                                )
        ),

        # ALT data tab
        shinydashboard::tabItem(tabName = "altData",
                                shiny::fluidRow(
                                  shiny::column(
                                    width = 3,
                                    shinydashboard::box(
                                      title = "Data Input",
                                      width = NULL,
                                      collapsible = TRUE,
                                      shiny::helpText("Select a sample data set or upload your ALT data:"),

                                      shinyWidgets::radioGroupButtons(
                                        inputId = "altDataInput",
                                        choices = c("Sample Data" = 1, "Upload Data" = 2),
                                        selected = 1,
                                        justified = TRUE
                                      ),

                                      shiny::conditionalPanel(
                                        condition = "input.altDataInput == 1",
                                        shiny::selectInput(
                                          inputId = "altDataSelect",
                                          h5("Data:"),
                                          c("Nelson Data" = 1, "Meeker Data" = 2)
                                        )
                                      ),

                                      shiny::conditionalPanel(
                                        condition = "input.altDataInput == 2",
                                        shiny::helpText("Upload a CSV with 'stress', 'time', and 'event' columns."),
                                        shiny::fileInput(
                                          "altFile",
                                          strong("File Input:"),
                                          accept = c("text/csv", "text/comma-separated-values",
                                                     "text/tab-separated-values", "text/plain",
                                                     ".csv", ".tsv")
                                        )
                                      )
                                    ),
                                    shinydashboard::box(
                                      title = "Data Selection",
                                      width = NULL,
                                      collapsible = TRUE,
                                      shiny::helpText("Map your data columns for analysis:"),

                                      shiny::selectizeInput("altStress", h5("Stress Level Column:"), c(""), selected = 1),
                                      shiny::helpText("Column containing the stress level values."),

                                      shiny::selectizeInput("altTime", h5("Time to Failure Column:"), c(""), selected = 2),
                                      shiny::helpText("Column containing failure or suspension times."),

                                      shiny::selectizeInput("altEvent", h5("Event Indicator Column:"), c(""), selected = 3),
                                      shiny::helpText("1 = failure, 0 = suspension (right-censored)."),

                                      shiny::checkboxInput("altSusp", "My data contains suspensions", value = FALSE)
                                    )
                                  ),
                                  shiny::column(
                                    width = 6,
                                    shinydashboard::box(
                                      title = "Data Table",
                                      width = NULL,
                                      shiny::tableOutput("altTable")
                                    )
                                  ),
                                  shiny::column(
                                    width = 3,
                                    shiny::uiOutput("altLevelsBox"),
                                    shiny::uiOutput("altEventsBox")
                                  )
                                )
        ),

        # ALT model tab
        shinydashboard::tabItem(tabName = "altModel",
                                shiny::fluidRow(
                                  shinydashboard::box(
                                    title = "Model Selection",
                                    width = 3,
                                    collapsible = TRUE,
                                    shiny::helpText("Select the distribution and life-stress relationship:"),

                                    shiny::selectInput(
                                      inputId = "altDist",
                                      h5("Distribution:"),
                                      c("Weibull" = "weibull", "Lognormal" = "lognormal")
                                    ),
                                    shiny::selectInput(
                                      inputId = "altModel",
                                      h5("ALT Model:"),
                                      c("Arrhenius" = "arrhenius", "Power Law" = "power")
                                    )
                                  ),

                                  shinydashboard::tabBox(
                                    title = "Model Results",
                                    id = "altTabset",
                                    width = 9,

                                    shiny::tabPanel(
                                      "ALT Probability Plot",
                                      shiny::fluidRow(
                                        shiny::column(
                                          width = 8,
                                          shinyWidgets::dropdownButton(
                                            h3("Plot Options"),
                                            shiny::selectInput("altGridCol", h5("Grid:"), COLORS_GRID, selected = "lightgray"),
                                            shiny::checkboxInput("altShowConf", "Show Confidence Bounds", value = TRUE),
                                            shiny::textInput("altMain", h5("Title:"),  value = "ALT Probability Plot"),
                                            shiny::textInput("altXlab", h5("X-axis Label:"), value = "Time to Failure"),
                                            shiny::textInput("altYlab", h5("Y-axis Label:"), value = "Probability"),
                                            circle = TRUE, status = "danger", icon = icon("gear")
                                          ),
                                          shinycssloaders::withSpinner(plotly::plotlyOutput("altProbPlot"), type = 6, color = "#dd4b39")
                                        ),
                                        shiny::column(
                                          width = 3,
                                          shiny::tableOutput("alt_results"),
                                          shiny::downloadButton("downloadAltResults", "Export CSV", style = "margin-top:8px;")
                                        )
                                      )
                                    ),

                                    shiny::tabPanel(
                                      "Life-Stress Relationship",
                                      shiny::fluidRow(
                                        shiny::column(
                                          width = 12,
                                          shinyWidgets::dropdownButton(
                                            h3("Plot Options"),
                                            shiny::selectInput("altRelFitCol",  h5("Fit:"),               COLORS_LINE, selected = "red"),
                                            shiny::selectInput("altRelPtCol",   h5("Points:"),             COLORS_FULL, selected = "black"),
                                            shiny::selectInput("altRelPercCol", h5("Percentiles:"),        COLORS_LINE, selected = "blue"),
                                            shiny::selectInput("altRelGridCol", h5("Grid:"),               COLORS_GRID, selected = "lightgray"),
                                            shiny::textInput("altRelMain", h5("Title:"),  value = "Life-Stress Relationship"),
                                            shiny::textInput("altRelXlab", h5("X-axis Label:"), value = "Stress"),
                                            shiny::textInput("altRelYlab", h5("Y-axis Label:"), value = "Time to Failure"),
                                            circle = TRUE, status = "danger", icon = icon("gear")
                                          ),
                                          shinycssloaders::withSpinner(plotly::plotlyOutput("altRelPlot"), type = 6, color = "#dd4b39"),
                                          shiny::downloadButton("downloadAltRel", "Export CSV", style = "margin-top:8px;")
                                        )
                                      )
                                    )
                                  )
                                )
        )
    )
    )
)

# Server logic
server <- function(input, output, session) {

    session$onSessionEnded(stopApp)

    # Landing page navigation buttons
    shiny::observeEvent(input$goToLifeData, {
      shinydashboard::updateTabItems(session, "sidebarMenu", "data")
    })
    shiny::observeEvent(input$goToGrowth, {
      shinydashboard::updateTabItems(session, "sidebarMenu", "growthData")
    })
    shiny::observeEvent(input$goToRepair, {
      shinydashboard::updateTabItems(session, "sidebarMenu", "rsData")
    })
    shiny::observeEvent(input$goToAlt, {
      shinydashboard::updateTabItems(session, "sidebarMenu", "altData")
    })

    # Example Time-to-Failure data
    acid_gas_compressor <- read.csv(system.file("app", "data", "acid_gas_compressor.csv", package = "ReliaShiny"))

    # Time-to-Failure data handler
    output$failure_data <- shiny::downloadHandler(
        filename = "acid_gas_compressor.csv",
        content = function(file) {
            write.csv(acid_gas_compressor, file, row.names = FALSE)
        }
    )

    # Example Right Censored data
    treat6mp <- read.csv(system.file("app", "data", "treat6mp.csv", package = "ReliaShiny"))

    # Right Censored data handler
    output$censored_data <- shiny::downloadHandler(
        filename = "treat6mp.csv",
        content = function(file) {
            write.csv(treat6mp, file, row.names = FALSE)
        }
    )

    # Get the data selection from user
    dat <- shiny::reactive({
      if (input$dataInput == 1) {

        if (input$dataSelect == 1) {
          dat <- data.frame(read.csv(system.file("app", "data", "acid_gas_compressor.csv", package = "ReliaShiny")))
        } else if (input$dataSelect == 2) {
          dat <- data.frame(read.csv(system.file("app", "data", "treat6mp.csv", package = "ReliaShiny")))
        }
      } else if (input$dataInput == 2) {

        # Get the uploaded file from user
        inFile <- input$file
        if (is.null(inFile))
          return(NULL)
        dat <- data.frame(read.csv(inFile$datapath))
      }
    })

    # Create the failures value box
    output$eventBox <- shiny::renderUI({

      # Get the number of failures in the data set
      if (is.null(dat())) {
        events <- 0
      } else
        events <- nrow(dat())

      shinydashboard::valueBox(events,
                               "Events",
                               icon = shiny::icon("table"),
                               color = "blue",
                               width = 12)
    })

    # Create the failures value box
    output$failBox <- shiny::renderUI({

      # Get the number of failures in the data set
      if (is.null(dat())) {
        failures <- 0
      } else if (is.null(input$event) || input$suspensions == 0) {
        failures <- nrow(dat())
      } else if (input$suspensions == 1) {
        req(input$event)
        event <- input$event
        datsub <- dat()[dat()[[event]] == 1, ]
        failures <- nrow(datsub)
      }

      shinydashboard::valueBox(failures,
                               "Failures",
                               icon = shiny::icon("arrow-down"),
                               color = "red",
                               width = 12)
    })

    # Create the suspensions value box
    output$suspBox <- shiny::renderUI({

      # Get the number of suspensions in the data set
      if (is.null(dat())) {
        suspensions <- 0
      } else if (is.null(input$event) || input$suspensions == 0) {
          suspensions <- 0
      } else if (input$suspensions == 1) {
        req(input$event)
        event <- input$event
        datsub <- dat()[dat()[[event]] == 0, ]
        suspensions <- nrow(datsub)
      }

      shinydashboard::valueBox(suspensions,
                               "Suspensions",
                               icon = shiny::icon("arrow-up"),
                               color = "green",
                               width = 12)
    })

    # Get the column names
    coln <- shiny::reactive({
        coln <- names(dat())
    })

    # Send the column names to the user
    lapply(c("time", "event", "qty", "left", "right"), function(id) {
        shiny::observe({ shiny::updateSelectInput(session, id, choices = coln()) })
    })

    # Check for suspensions
    event <- shiny::reactive({
        if (is.null(input$event) || input$suspensions == 0) {
            event <- rep(1, length(subset(dat(), select = 1)))
        } else if (input$suspensions == 1) {
            event <- subset(dat(), select = input$event)
        }
    })

    # Check for groups
    qty <- shiny::reactive({
        if (is.null(input$qty) || input$groups == 0) {
            qty <- rep(1, length(subset(dat(), select = 1)))
        } else if (input$groups == 1) {
            qty <- subset(dat(), select = input$qty)
        }
    })

    # Arrange data for the wblr function
    wblr_dat <- shiny::reactive({
        if (is.null(dat()))
            return(NULL)

        # Check for intervals
        if (is.null(input$left) || is.null(input$right) || input$intervals == 0) {
            time <- subset(dat(), select = input$time)
            wblr_dat <- data.frame(time, event(), qty())
        } else if (input$intervals == 1) {
            time <- subset(dat(), select = input$right)
            colnames(time) <- 'time'
            wblr_dat0 <- data.frame(time, event = event(), qty = qty())
                wblr_dat0 <- subset(wblr_dat0, event == 0)
        }
    })

    # Arrange data for interval censored models
    ints_dat <- shiny::reactive({
        if (is.null(dat()))
            return(NULL)

        # Check for intervals
        if (is.null(input$left) || is.null(input$right) || input$intervals == 0) {
            ints_dat <- NULL
        } else if (input$intervals == 1) {
            left <- subset(dat(), select = input$left)
            right <- subset(dat(), select = input$right)
            ints_dat <- data.frame(left, right, event = event(), qty = qty()) %>%
                subset(event == 1) %>%
                subset(select = c(left, right, qty))
        }
    })

    # Create a table of the user dataset
    output$table = shiny::renderTable({
        if (is.null(dat()))
            return(NULL)

        shiny::validate(
            shiny::need(!is.null(dat()), message = FALSE)
        )

        dat()
    }, striped = TRUE, hover = TRUE, bordered = TRUE, align = 'c')

    # Create a table of the user dataset
    # Create a wblr object
    wblr_obj <- shiny::reactive({
        if (is.null(wblr_dat()))
            return(NULL)

        shiny::validate(
            shiny::need(
                try(is.numeric(wblr_dat()$time)),
                "Time column must be numeric"
            ) %then%
                shiny::need(
                    try(all(wblr_dat()$time>0)),
                    "Time column must contain positive numbers"
                ) %then%
                shiny::need(
                    try(all(wblr_dat()$event %in% 0:1)),
                    "Event column must be a binary variable"
                ) %then%
                shiny::need(
                    try(is.numeric(wblr_dat()$qty)),
                    "Group column must be numeric"
                ) %then%
                shiny::need(
                    try(all(wblr_dat()$qty>0)),
                    "Group column must contain positive numbers"
                )
        )

        # Get the confidence method
        if (input$meth == "mle") confMeth <- input$mleConf
        else confMeth = input$rrConf

        # Run the wblr object
        wblr_obj <- tryCatch(
            WeibullR::wblr.conf(WeibullR::wblr.fit(
                WeibullR::wblr(
                    x = wblr_dat(),
                    interval = ints_dat(),
                    pp = input$pp
                ),
                dist = input$dist,
                method.fit = input$meth
            ),
            method.conf = confMeth,
            ci = input$cl),
            error = function(e) {
                shiny::validate(shiny::need(FALSE, paste("Model fitting failed:", conditionMessage(e))))
            }
        )
    })

    # Extract results from the wblr object
    wblr_res <- shiny::reactive({
        if (is.null(wblr_obj()))
            return(NULL)

        wblr_res <- extract_wblr_summ(wblr_obj())
    })

    # Build a table of the wblr results
    output$wblr_results = shiny::renderTable({
        if (is.null(wblr_res()))
            return(NULL)
        shiny::validate(
            shiny::need(!is.null(wblr_res()), message = FALSE)
            )
        wblr_res()
        }, striped = TRUE, hover = TRUE, bordered = TRUE, align = 'c')

    output$downloadWblrResults <- shiny::downloadHandler(
        filename = function() paste0("wblr_results_", Sys.Date(), ".csv"),
        content  = function(file) write.csv(wblr_res(), file, row.names = FALSE)
    )

    # Create a suspensions vector
    susp_vec <- shiny::reactive({
        if (is.null(input$event) || input$suspensions == 0) {
            susp_vec <- NULL
        } else if (input$suspensions == 1) {
            susp_vec <- as.numeric(unlist(subset(wblr_dat(), event == 0, select = 'time')))
        }
    })

    # Build the probability plot
    output$probPlot <- plotly::renderPlotly({
        if (is.null(wblr_obj()))
            return(NULL)

        p <- ReliaPlotR::plotly_wblr(
            wblr_obj(),
            susp = susp_vec(),
            showSusp = input$suspPlot,
            main = input$main,
            xlab = input$xlab,
            ylab = input$ylab,
            probCol = input$probcol,
            fitCol = input$fitcol,
            confCol = input$confcol,
            gridCol = input$gridcol,
            showGrid = input$grid,
            signif = input$signif
        )
        plotly::config(p, toImageButtonOptions = list(format = "png", filename = "probability_plot"))

    })

    # Build the contour plot
    output$contPlot <- plotly::renderPlotly({
        if (is.null(wblr_obj()))
            return(NULL)
        shiny::validate(
            shiny::need(
                try(input$meth == "mle"),
                "Contour plots require the 'MLE' estimation method. Please switch to MLE above."
            ) %then%
                shiny::need(
                    try(input$mleConf == 'lrb'),
                    "Contour plots require the 'LRB' (Likelihood Ratio Bounds) confidence method. Please select LRB above."
                )
        )
        p <- ReliaPlotR::plotly_contour(
            wblr_obj(),
            main = input$main2,
            xlab = input$xlab2,
            ylab = input$ylab2,
            col = input$col2,
            gridCol = input$gridcol2,
            showGrid = input$grid2,
            signif = input$signif2
        )
        plotly::config(p, toImageButtonOptions = list(format = "png", filename = "contour_plot"))
    })

    # Example reliability growth data
    simpleData <- read.csv(system.file("app", "data", "simpleData.csv", package = "ReliaShiny"))

    # Reliability growth data handler
    output$growthData <- shiny::downloadHandler(
      filename = "simpleData.csv",
      content = function(growthFile) {
        write.csv(simpleData, growthFile, row.names = FALSE)
      }
    )

    # Example large data set
    largeData <- read.csv(system.file("app", "data", "largeData.csv", package = "ReliaShiny"))

    # Large data handler
    output$largeData <- shiny::downloadHandler(
      filename = "largeData.csv",
      content = function(growthFile) {
        write.csv(largeData, growthFile, row.names = FALSE)
      }
    )

    # Get the data selection from user
    growthDat <- shiny::reactive({
      if (input$growthDataInput == 1) {

        if (input$growthDataSelect == 1) {
          growthDat <- data.frame(read.csv(system.file("app", "data", "simpleData.csv", package = "ReliaShiny")))
        } else if (input$growthDataSelect == 2) {
          growthDat <- data.frame(read.csv(system.file("app", "data", "largeData.csv", package = "ReliaShiny")))
        }
      } else if (input$growthDataInput == 2) {

        # Get the uploaded file from user
        growthFile <- input$growthFile
        if (is.null(growthFile))
          return(NULL)
        growthDat <- data.frame(read.csv(growthFile$datapath))
      }
    })

    # Create the cumulative failures value box
    output$failuresBox <- shiny::renderUI({

      # Get the number of failures in the data set
      if (is.null(growthDat())) {
        maxFailures <- 0
      } else {
        req(input$failures)
        maxFailures <- sum(growthDat()[[input$failures]])
      }

      shinydashboard::valueBox(maxFailures,
                               "Cumulative Failures",
                               icon = shiny::icon("arrow-down"),
                               color = "red",
                               width = 12)
    })

    # Create the cumulative time value box
    output$timesBox <- shiny::renderUI({

      # Get the max time in the data set
      if (is.null(growthDat())) {
        maxTime <- 0
      } else {
        req(input$times)
        maxTime <- tail(growthDat()[[input$times]], 1)
      }

      shinydashboard::valueBox(maxTime,
                               "Cumulative Time",
                               icon = shiny::icon("table"),
                               color = "blue",
                               width = 12)
    })

    # Get the column names
    growthColn <- shiny::reactive({
      growthColn <- names(growthDat())
    })

    # Send the column names to the user, pre-selecting known columns
    shiny::observe({
      cols <- growthColn()
      shiny::updateSelectInput(session, "times",
        choices  = cols,
        selected = if ("times"    %in% cols) "times"    else cols[1])
      shiny::updateSelectInput(session, "failures",
        choices  = cols,
        selected = if ("failures" %in% cols) "failures" else cols[min(2, length(cols))])
    })

    # Create a table of the user dataset
    output$growthTable = shiny::renderTable({
      if (is.null(growthDat()))
        return(NULL)

      shiny::validate(
        shiny::need(!is.null(growthDat()), message = FALSE)
      )

      growthDat()
    }, striped = TRUE, hover = TRUE, bordered = TRUE, align = 'c')

    # Create a rga object
    rga_obj <- shiny::reactive({
      if (is.null(growthDat()))
        return(NULL)

      # Error handling
      shiny::validate(
        shiny::need(
          try(input$times != input$failures),
          "Time and Failure columns must be different."
        ) %then%
        shiny::need(
          try(is.numeric(growthDat()[[input$times]])),
          "Time column must be numeric."
        ) %then%
          shiny::need(
            try(all(growthDat()[[input$times]]>0)),
            "Time column must contain positive numbers."
          ) %then%
          shiny::need(
            try(is.numeric(growthDat()[[input$failures]])),
            "Failure column must be numeric."
          ) %then%
          shiny::need(
            try(all(growthDat()[[input$failures]]>0)),
            "Failure column must contain positive numbers."
          )
      )

      # Get the confidence level
      if (input$growthConf == 0) {
        conf_level <- 0.001
      } else {
        conf_level <- input$growthConf
      }

      # Run the rga object
      if (input$growthModel == 1) {
        rga_obj <- tryCatch(
          ReliaGrowR::rga(
            times = growthDat()[[input$times]],
            failures = growthDat()[[input$failures]],
            conf_level = conf_level
          ),
          error = function(e) {
            shiny::validate(shiny::need(FALSE, paste("Model fitting failed:", conditionMessage(e))))
          }
        )
      } else if (input$growthModel == 2) {

        # Error handling
        shiny::validate(
            shiny::need(
              try(input$breakpoints > min(growthDat()[[input$times]])),
              "Breakpoint must be greater than the smallest failure time."
            ) %then%
            shiny::need(
              try(input$breakpoints < max(growthDat()[[input$times]])),
              "Breakpoint must be less than the largest failure time."
            )
        )

        # Transform user-supplied breakpoints into a numeric vector
        breakpoints <- as.numeric(input$breakpoints)
        rga_obj <- tryCatch(
          ReliaGrowR::rga(
            times = growthDat()[[input$times]],
            failures = growthDat()[[input$failures]],
            model_type = "Piecewise NHPP",
            breaks = breakpoints,
            conf_level = conf_level
          ),
          error = function(e) {
            shiny::validate(shiny::need(FALSE, paste("Model fitting failed:", conditionMessage(e))))
          }
        )
      } else if (input$growthModel == 3) {
        rga_obj <- tryCatch(
          ReliaGrowR::rga(
            times = growthDat()[[input$times]],
            failures = growthDat()[[input$failures]],
            model_type = "Piecewise NHPP",
            conf_level = conf_level
          ),
          error = function(e) {
            shiny::validate(shiny::need(FALSE, paste("Model fitting failed:", conditionMessage(e))))
          }
        )
      }
    })

    # Extract results from the rga object
    rga_res <- shiny::reactive({
      if (is.null(rga_obj()))
        return(NULL)

      rga_res <- extract_rga_summ(rga_obj())

    })

    # Build a table of the rga results
    output$rga_results = shiny::renderTable({
      if (is.null(rga_res()))
        return(NULL)

      shiny::validate(
        shiny::need(!is.null(rga_res()), message = FALSE)
      )

      rga_res()
    }, striped = TRUE, hover = TRUE, bordered = TRUE, align = 'c')

    output$downloadRgaResults <- shiny::downloadHandler(
      filename = function() paste0("rga_results_", Sys.Date(), ".csv"),
      content  = function(file) write.csv(rga_res(), file, row.names = FALSE)
    )

    # Build the reliability growth plot
    output$growthPlot <- plotly::renderPlotly({
      if (is.null(rga_obj()))
        return(NULL)

      p <- ReliaPlotR::plotly_rga(
        rga_obj(),
        main = input$growthMain,
        xlab = input$growthXlab,
        ylab = input$growthYlab,
        pointCol = input$pointCol,
        fitCol = input$modelCol,
        confCol = input$growthConfCol,
        gridCol = input$growthGridCol,
        breakCol = input$breakCol
      )
      plotly::config(p, toImageButtonOptions = list(format = "png", filename = "growth_plot"))
    })

    # Create a duane object
    duane_obj <- shiny::reactive({
      if (is.null(growthDat()))
        return(NULL)

      # Error handling
      shiny::validate(
        shiny::need(
          try(input$times != input$failures),
          "Time and Failure columns must be different."
        ) %then%
        shiny::need(
          try(is.numeric(growthDat()[[input$times]])),
          "Time column must be numeric"
        ) %then%
          shiny::need(
            try(all(growthDat()[[input$times]]>0)),
            "Time column must contain positive numbers"
          ) %then%
          shiny::need(
            try(is.numeric(growthDat()[[input$failures]])),
            "Failure column must be numeric"
          ) %then%
          shiny::need(
            try(all(growthDat()[[input$failures]]>0)),
            "Failure column must contain positive numbers"
          )
      )

      # Get the confidence level
      if (input$growthConf == 0) {
        conf_level <- 0.001
      } else {
        conf_level <- input$growthConf
      }

      # Run the duane object
        duane_obj <-
          ReliaGrowR::duane(
            times = growthDat()[[input$times]],
            failures = growthDat()[[input$failures]],
            conf.level = conf_level
          )
    })

    output$downloadDuane <- shiny::downloadHandler(
      filename = function() paste0("duane_data_", Sys.Date(), ".csv"),
      content  = function(file) write.csv(growthDat(), file, row.names = FALSE)
    )

    # Build the duane plot
    output$duanePlot <- plotly::renderPlotly({
      if (is.null(duane_obj()))
        return(NULL)

      p <- ReliaPlotR::plotly_duane(
        duane_obj(),
        pointCol = input$pointCol2,
        fitCol = input$modelCol2,
        confCol = input$growthConfCol2,
        gridCol = input$growthGridCol2,
        main = input$duaneMain,
        xlab = input$duaneXlab,
        ylab = input$duaneYlab
      )
      plotly::config(p, toImageButtonOptions = list(format = "png", filename = "duane_plot"))
    })

    # ---- Repairable Systems module ----

    # Load repairable systems data
    rsDat <- shiny::reactive({
      if (input$rsDataInput == 1) {
        path <- if (input$rsDataSelect == 1) "simpleRepairData.csv" else "largeRepairData.csv"
        data.frame(read.csv(system.file("app", "data", path, package = "ReliaShiny")))
      } else {
        rsFile <- input$rsFile
        if (is.null(rsFile)) return(NULL)
        data.frame(read.csv(rsFile$datapath))
      }
    })

    # Get column names
    rsColn <- shiny::reactive({ names(rsDat()) })

    # Populate column selectors, pre-selecting known columns
    shiny::observe({
      cols <- rsColn()
      shiny::updateSelectInput(session, "rsId",
        choices  = cols,
        selected = if ("id"       %in% cols) "id"       else cols[1])
      shiny::updateSelectInput(session, "rsTime",
        choices  = cols,
        selected = if ("time"     %in% cols) "time"     else cols[min(2, length(cols))])
      shiny::updateSelectInput(session, "rsEvent",
        choices  = cols,
        selected = if ("event"    %in% cols) "event"    else cols[min(3, length(cols))])
      shiny::updateSelectInput(session, "rsEndTime",
        choices  = cols,
        selected = if ("end_time" %in% cols) "end_time" else cols[min(4, length(cols))])
    })

    # Number of unique systems value box
    output$rsSystemsBox <- shiny::renderUI({
      n <- if (is.null(rsDat()) || !nzchar(input$rsId)) 0L
           else length(unique(rsDat()[[input$rsId]]))
      shinydashboard::valueBox(n, "Systems", icon = shiny::icon("server"), color = "blue", width = 12)
    })

    # Total failure events value box
    output$rsEventsBox <- shiny::renderUI({
      n <- if (is.null(rsDat()) || !nzchar(input$rsEvent)) 0L
           else sum(rsDat()[[input$rsEvent]] == 1, na.rm = TRUE)
      shinydashboard::valueBox(n, "Total Failures", icon = shiny::icon("arrow-down"), color = "red", width = 12)
    })

    # Data table
    output$rsTable <- shiny::renderTable({
      if (is.null(rsDat())) return(NULL)
      rsDat()
    }, striped = TRUE, hover = TRUE, bordered = TRUE, align = "c")

    # NHPP model object
    nhpp_obj <- shiny::reactive({
      if (is.null(rsDat())) return(NULL)

      shiny::validate(
        shiny::need(
          try(input$rsTime != input$rsEvent),
          "Time and Event columns must be different."
        ) %then%
        shiny::need(
          try(is.numeric(rsDat()[[input$rsTime]])),
          "Time column must be numeric."
        ) %then%
        shiny::need(
          try(all(rsDat()[[input$rsTime]] > 0)),
          "Time column must contain positive numbers."
        ) %then%
        shiny::need(
          try(is.numeric(rsDat()[[input$rsEvent]])),
          "Event column must be numeric."
        )
      )

      conf_level <- if (input$rsConf == 0) 0.001 else input$rsConf

      model_type <- switch(input$rsNhppModel,
        "1" = "Power Law",
        "2" = "Log-Linear",
        "3" = "Power Law",
        "4" = "Power Law"
      )
      breaks <- if (input$rsNhppModel == "3") {
        shiny::validate(
          shiny::need(
            try(input$rsBreakpoints > min(rsDat()[[input$rsTime]])),
            "Breakpoint must be greater than the smallest event time."
          ) %then%
          shiny::need(
            try(input$rsBreakpoints < max(rsDat()[[input$rsTime]])),
            "Breakpoint must be less than the largest event time."
          )
        )
        as.numeric(input$rsBreakpoints)
      } else NULL

      sorted_dat <- rsDat()[order(rsDat()[[input$rsTime]]), ]
      tryCatch(
        ReliaGrowR::nhpp(
          time       = input$rsTime,
          event      = input$rsEvent,
          data       = sorted_dat,
          model_type = model_type,
          breaks     = breaks,
          method     = input$rsMethod,
          conf_level = conf_level
        ),
        error = function(e) {
          shiny::validate(shiny::need(FALSE, paste("NHPP fitting failed:", conditionMessage(e))))
        }
      )
    })

    # Exposure object
    exposure_obj <- shiny::reactive({
      if (is.null(rsDat())) return(NULL)
      shiny::validate(
        shiny::need(nzchar(input$rsId),   "Please select a System ID column."),
        shiny::need(nzchar(input$rsTime), "Please select an Event Time column.")
      )
      tryCatch(
        ReliaGrowR::exposure(
          id   = input$rsId,
          time = input$rsTime,
          data = rsDat()
        ),
        error = function(e) {
          shiny::validate(shiny::need(FALSE, paste("Exposure calculation failed:", conditionMessage(e))))
        }
      )
    })

    # MCF object
    mcf_obj <- shiny::reactive({
      if (is.null(rsDat())) return(NULL)
      shiny::validate(
        shiny::need(nzchar(input$rsId),    "Please select a System ID column."),
        shiny::need(nzchar(input$rsTime),  "Please select an Event Time column."),
        shiny::need(nzchar(input$rsEvent), "Please select an Event Indicator column.")
      )
      conf_level <- if (input$rsConf == 0) 0.001 else input$rsConf
      end_time_col <- if (nzchar(input$rsEndTime) && input$rsEndTime != input$rsTime) input$rsEndTime else NULL
      end_times_vec <- if (!is.null(end_time_col)) {
        tapply(rsDat()[[end_time_col]], as.character(rsDat()[[input$rsId]]), max)
      } else NULL
      tryCatch(
        ReliaGrowR::mcf(
          id         = input$rsId,
          time       = input$rsTime,
          event      = input$rsEvent,
          end_time   = end_times_vec,
          data       = rsDat(),
          conf_level = conf_level
        ),
        error = function(e) {
          shiny::validate(shiny::need(FALSE, paste("MCF calculation failed:", conditionMessage(e))))
        }
      )
    })

    # NHPP results table
    output$rs_results <- shiny::renderTable({
      if (is.null(nhpp_obj())) return(NULL)
      extract_nhpp_summ(nhpp_obj())
    }, striped = TRUE, hover = TRUE, bordered = TRUE, align = "c")

    output$downloadRsResults <- shiny::downloadHandler(
      filename = function() paste0("nhpp_results_", Sys.Date(), ".csv"),
      content  = function(file) write.csv(extract_nhpp_summ(nhpp_obj()), file, row.names = FALSE)
    )

    # NHPP plot
    output$rsNhppPlot <- plotly::renderPlotly({
      if (is.null(nhpp_obj())) return(NULL)
      p <- ReliaPlotR::plotly_nhpp(
        nhpp_obj(),
        main     = input$rsMain,
        xlab     = input$rsXlab,
        ylab     = input$rsYlab,
        pointCol = input$rsPointCol,
        fitCol   = input$rsModelCol,
        confCol  = input$rsConfCol,
        gridCol  = input$rsGridCol,
        breakCol = input$rsBreakCol
      )
      plotly::config(p, toImageButtonOptions = list(format = "png", filename = "nhpp_plot"))
    })

    # Exposure plot
    output$rsExposurePlot <- plotly::renderPlotly({
      if (is.null(exposure_obj())) return(NULL)
      p <- ReliaPlotR::plotly_exposure(
        exposure_obj(),
        main    = input$rsExpMain,
        xlab    = input$rsExpXlab,
        ylab    = input$rsExpYlab,
        fitCol  = input$rsExpFitCol,
        gridCol = input$rsExpGridCol
      )
      plotly::config(p, toImageButtonOptions = list(format = "png", filename = "exposure_plot"))
    })

    output$downloadRsExposure <- shiny::downloadHandler(
      filename = function() paste0("exposure_data_", Sys.Date(), ".csv"),
      content  = function(file) write.csv(rsDat(), file, row.names = FALSE)
    )

    # MCF plot
    output$rsMcfPlot <- plotly::renderPlotly({
      if (is.null(mcf_obj())) return(NULL)
      p <- ReliaPlotR::plotly_mcf(
        mcf_obj(),
        main    = input$rsMcfMain,
        xlab    = input$rsMcfXlab,
        ylab    = input$rsMcfYlab,
        fitCol  = input$rsMcfFitCol,
        confCol = input$rsMcfConfCol,
        gridCol = input$rsMcfGridCol
      )
      plotly::config(p, toImageButtonOptions = list(format = "png", filename = "mcf_plot"))
    })

    output$downloadRsMcf <- shiny::downloadHandler(
      filename = function() paste0("mcf_data_", Sys.Date(), ".csv"),
      content  = function(file) write.csv(rsDat(), file, row.names = FALSE)
    )

    # ---- Accelerated Life Testing module ----

    # Load ALT data
    altDat <- shiny::reactive({
      if (input$altDataInput == 1) {
        path <- if (input$altDataSelect == 1) "nelsonData.csv" else "meekerData.csv"
        data.frame(read.csv(system.file("app", "data", path, package = "ReliaShiny")))
      } else {
        f <- input$altFile
        if (is.null(f)) return(NULL)
        data.frame(read.csv(f$datapath))
      }
    })

    # Populate column selectors
    shiny::observe({
      req(altDat())
      cols <- names(altDat())
      shiny::updateSelectInput(session, "altStress", choices = cols,
        selected = if ("stress" %in% cols) "stress" else cols[1])
      shiny::updateSelectInput(session, "altTime",   choices = cols,
        selected = if ("time"   %in% cols) "time"   else cols[min(2, length(cols))])
      shiny::updateSelectInput(session, "altEvent",  choices = cols,
        selected = if ("event"  %in% cols) "event"  else cols[min(3, length(cols))])
    })

    # Stress levels value box
    output$altLevelsBox <- shiny::renderUI({
      n <- if (is.null(altDat()) || !nzchar(input$altStress)) 0L
           else length(unique(altDat()[[input$altStress]]))
      shinydashboard::valueBox(n, "Stress Levels", icon = shiny::icon("bolt"), color = "blue", width = 12)
    })

    # Total failure events value box
    output$altEventsBox <- shiny::renderUI({
      n <- if (is.null(altDat()) || !nzchar(input$altEvent)) 0L
           else sum(altDat()[[input$altEvent]] == 1, na.rm = TRUE)
      shinydashboard::valueBox(n, "Total Failures", icon = shiny::icon("arrow-down"), color = "red", width = 12)
    })

    # Data table
    output$altTable <- shiny::renderTable({
      if (is.null(altDat())) return(NULL)
      altDat()
    }, striped = TRUE, hover = TRUE, bordered = TRUE, align = "c")

    # Fit ALT model object
    alt_obj <- shiny::reactive({
      req(altDat(), input$altStress, input$altTime)

      shiny::validate(
        shiny::need(
          try(is.numeric(altDat()[[input$altStress]])),
          "Stress column must be numeric."
        ) %then%
        shiny::need(
          try(is.numeric(altDat()[[input$altTime]])),
          "Time column must be numeric."
        ) %then%
        shiny::need(
          try(length(unique(altDat()[[input$altStress]])) >= 2),
          "At least two stress levels are required."
        )
      )

      dat           <- altDat()
      stress_levels <- sort(unique(dat[[input$altStress]]))


      alt_data_list <- lapply(stress_levels, function(s) {
        sub   <- dat[dat[[input$altStress]] == s, ]
        times <- sub[[input$altTime]]
        event <- if (input$altSusp && nzchar(input$altEvent)) sub[[input$altEvent]]
                 else rep(1, nrow(sub))
        fail_times <- times[event == 1]
        susp_times <- times[event == 0]
        if (length(susp_times) > 0) {
          WeibullR.ALT::alt.data(fail_times, s = susp_times, stress = s)
        } else {
          WeibullR.ALT::alt.data(fail_times, stress = s)
        }
      })

      tryCatch({
        make_obj <- WeibullR.ALT::alt.make(alt_data_list, dist = input$altDist,
                                           alt.model = input$altModel,
                                           view_dist_fits = FALSE)
        WeibullR.ALT::alt.fit(
          WeibullR.ALT::alt.parallel(make_obj, view_parallel_fits = FALSE)
        )
      }, error = function(e) {
        shiny::validate(shiny::need(FALSE, paste("ALT fitting failed:", conditionMessage(e))))
      })
    })

    # Results table
    output$alt_results <- shiny::renderTable({
      if (is.null(alt_obj())) return(NULL)
      extract_alt_summ(alt_obj())
    }, striped = TRUE, hover = TRUE, bordered = TRUE, align = "c")

    output$downloadAltResults <- shiny::downloadHandler(
      filename = function() paste0("alt_results_", Sys.Date(), ".csv"),
      content  = function(file) write.csv(extract_alt_summ(alt_obj()), file, row.names = FALSE)
    )

    # ALT probability plot
    output$altProbPlot <- plotly::renderPlotly({
      if (is.null(alt_obj())) return(NULL)
      p <- ReliaPlotR::plotly_alt(
        alt_obj(),
        main     = input$altMain,
        xlab     = input$altXlab,
        ylab     = input$altYlab,
        gridCol  = input$altGridCol,
        showConf = input$altShowConf
      )
      plotly::config(p, toImageButtonOptions = list(format = "png", filename = "alt_prob_plot"))
    })

    # Life-stress relationship plot
    output$altRelPlot <- plotly::renderPlotly({
      if (is.null(alt_obj())) return(NULL)
      p <- ReliaPlotR::plotly_rel(
        alt_obj(),
        main    = input$altRelMain,
        xlab    = input$altRelXlab,
        ylab    = input$altRelYlab,
        fitCol  = input$altRelFitCol,
        ptCol   = input$altRelPtCol,
        percCol = input$altRelPercCol,
        gridCol = input$altRelGridCol
      )
      plotly::config(p, toImageButtonOptions = list(format = "png", filename = "alt_rel_plot"))
    })

    output$downloadAltRel <- shiny::downloadHandler(
      filename = function() paste0("alt_data_", Sys.Date(), ".csv"),
      content  = function(file) write.csv(altDat(), file, row.names = FALSE)
    )
}

# Run the application
shiny::shinyApp(ui, server, enableBookmarking = "url")
