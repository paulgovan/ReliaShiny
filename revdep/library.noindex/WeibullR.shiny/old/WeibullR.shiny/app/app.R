`%then%` <- function(a, b) {
    if (is.null(a)) b else a
}

ui <- shinydashboard::dashboardPage(
    skin = "red",
    shinydashboard::dashboardHeader(title = "WeibullR.shiny"),

    ## Sidebar content
    shinydashboard::dashboardSidebar(
        shinydashboard::sidebarMenu(
            shinydashboard::menuItem("Landing", tabName = "landing", icon = icon("helicopter-symbol")),
            shinydashboard::menuItem("Data", tabName = "data", icon = icon("table")),
            shinydashboard::menuItem("Modeling", tabName = "model", icon = icon("chart-line")),
            shiny::br(),
            shinydashboard::menuItem("Help", icon = icon("info-circle"), href = "https://paulgovan.github.io/WeibullR.shiny/"),
            shinydashboard::menuItem("Source Code", icon = icon("github"), href = "https://github.com/paulgovan/WeibullR.shiny"),
            shiny::br(),
            shiny::bookmarkButton()
        )
    ),

    ## Body content
    shinydashboard::dashboardBody(shinydashboard::tabItems(
        # First tab content
        shinydashboard::tabItem(tabName = "landing",
                                shiny::fluidRow(
                                    shinydashboard::box(
                                        title = "",
                                        width = 12,
                                        shiny::img(
                                            src = "WeibullR.png",
                                            height = 200,
                                            width = 173
                                        ),
                                        shiny::h2("WeibullR.shiny"),
                                        shiny::h3(
                                            "A ",
                                            shiny::a(href = 'http://shiny.rstudio.com', 'Shiny'),
                                            "app for Weibull Analysis based on ",
                                            shiny::a(href = 'http://www.openreliability.org/weibull-r-weibull-analysis-on-r/', 'WeibullR')
                                        ),
                                        shiny::br(),
                                        shiny::h4(
                                            "For help getting started, view the ",
                                            shiny::a(href = 'https://paulgovan.github.io/WeibullR.shiny/', 'ReadMe'),
                                            "."
                                        ),
                                        shiny::h4(
                                            "To download an example data set, choose from the following: ",
                                            tags$ul(
                                                tags$li(shiny::downloadLink('failure_data', 'Time-to-Failure Data')),
                                                tags$li(shiny::downloadLink('censored_data', 'Right Censored Data')),
                                                tags$li('More coming soon!')
                                            )
                                        ),
                                        shiny::h4(
                                            "To upload your own data set, click",
                                            shiny::a("Data", href = "#shiny-tab-data", "data-toggle" = "tab"),
                                            " in the sidepanel to get started."
                                        ),
                                        shiny::br(),
                                        shiny::h4(
                                            shiny::HTML('&copy'),
                                            '2023 By Paul Govan. ',
                                            shiny::a(href = 'http://www.apache.org/licenses/LICENSE-2.0', 'Terms of Use.')
                                        ),
                                    )
                                )),

        # Second tab content
        shinydashboard::tabItem(tabName = "data",
                                shiny::fluidRow(
                                    shiny::column(
                                        width = 3,
                                        shinydashboard::box(
                                            title = "Data Input",
                                            width = NULL,
                                            collapsible = TRUE,
                                            shiny::helpText("Upload your Time-to-Failure data:"),
                                            # File input
                                            shiny::fileInput(
                                                'file',
                                                strong('File Input:'),
                                                accept = c(
                                                    'text/csv',
                                                    'text/comma-separated-values',
                                                    'text/tab-separated-values',
                                                    'text/plain',
                                                    '.csv',
                                                    '.tsv'
                                                )
                                            ),
                                            shiny::helpText(
                                                "Your data must be either a csv file containing at least 'time'
                  and 'event' columns and optionally a 'qty' column or a csv file
                  of interval data containing 'left' and 'right' columns."
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

                                                # Failure code
                                                # shiny::selectizeInput(
                                                #   inputId = "fail_code",
                                                #   h5("Failure code:"),
                                                #   c(""),
                                                #   selected = 2
                                                # ),
                                                # shiny::helpText("Common failure codes include 1, `F`, etc.")
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
                                        width = 9,
                                        shinydashboard::box(
                                            title = "Data Table",
                                            width = NULL,
                                            collapsible = TRUE,
                                            shiny::tableOutput("table")
                                        )
                                    )
                                )),

        # third tab content
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
                                                # "Weibayes" = "weibayes",
                                                "Weibull 2P" = "weibull",
                                                "Weibull 3P" = "weibull3p",
                                                "Lognormal" = "lognormal"
                                            ),
                                            selected = "weibull"
                                        ),
                                        # Conditional panel for 1P Weibull
                                        shiny::conditionalPanel(
                                            condition = "input.dist == 'weibayes'",

                                            # 1P Weibull Beta
                                            shiny::numericInput(
                                                inputId = "beta",
                                                h5("Beta:"),
                                                value = 1,
                                                min = 0.1,
                                                max = 10,
                                                step = 0.1
                                            )
                                        ),
                                        # Method input
                                        shiny::selectInput(
                                            inputId = "meth",
                                            h5("Estimation Method:"),
                                            c(
                                                "Maximum Likelihood" = "mle",
                                                # "Rank Regression YX" = "rr-yonx,"
                                                "Rank Regression XY" = "rr-xony"
                                            )
                                        ),
                                        # Plotting position
                                        shiny::selectInput(
                                            inputId = "pp",
                                            h5("Plotting Position Method:"),
                                            c(
                                                "Median" = "median",
                                                # "Bernard" = "bernard",
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
                                            plotly::plotlyOutput('probPlot'),
                                            shiny::br(),
                                            shiny::fluidRow(
                                                shinydashboard::box(
                                                    title = "Plot Options",
                                                    collapsed = TRUE,
                                                    collapsible = TRUE,
                                                    solidHeader = TRUE,
                                                    # Probability color
                                                    shiny::selectInput(
                                                        inputId = "probcol",
                                                        h5("Probability Points Color:"),
                                                        c("black",
                                                          "blue",
                                                          "red",
                                                          "yellow",
                                                          "green",
                                                          "orange",
                                                          "violet"
                                                        ),
                                                        selected = "black"
                                                    ),
                                                    # Fit color
                                                    shiny::selectInput(
                                                        inputId = "fitcol",
                                                        h5("Fit Color:"),
                                                        c("blue",
                                                          "red",
                                                          "yellow",
                                                          "green",
                                                          "orange",
                                                          "violet"
                                                        ),
                                                        selected = "blue"
                                                    ),
                                                    # CB color
                                                    shiny::selectInput(
                                                        inputId = "confcol",
                                                        h5("Confidence Bounds Color:"),
                                                        c("blue",
                                                          "red",
                                                          "yellow",
                                                          "green",
                                                          "orange",
                                                          "violet"
                                                        ),
                                                        selected = "blue"
                                                    ),
                                                    # Grid color
                                                    shiny::selectInput(
                                                        inputId = "gridcol",
                                                        h5("Grid Color:"),
                                                        c("lightgray",
                                                          "black",
                                                          "blue",
                                                          "red",
                                                          "yellow",
                                                          "green",
                                                          "orange",
                                                          "violet"
                                                        ),
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
                                                                     value = "Unreliability (%)"),
                                                    # Significant digits
                                                    shiny::numericInput(
                                                        inputId = "signif",
                                                        h5("Significant Digits:"),
                                                        value = 3
                                                    ),
                                                    # Show CB
                                                    # shiny::checkboxInput("confBounds",
                                                    #                      label = "Show confidence bounds?",
                                                    #                      value = TRUE),
                                                    # Show suspensions plot
                                                    shiny::checkboxInput("suspPlot",
                                                                         label = "Show suspensions plot?",
                                                                         value = TRUE),
                                                    # Show results table
                                                    shiny::checkboxInput("resTab",
                                                                         label = "Show results table?",
                                                                         value = TRUE),
                                                    # Show grid
                                                    shiny::checkboxInput("grid",
                                                                         label = "Show grid?",
                                                                         value = TRUE),
                                                )
                                            )
                                        ),
                                        shiny::tabPanel("Contour Plot", plotly::plotlyOutput('contPlot'),
                                                        shiny::br(),
                                                        shiny::fluidRow(
                                                            shinydashboard::box(
                                                                title = "Plot Options",
                                                                collapsed = TRUE,
                                                                collapsible = TRUE,
                                                                solidHeader = TRUE,
                                                                # Plot color
                                                                shiny::selectInput(
                                                                    inputId = "col2",
                                                                    h5("Plot Color:"),
                                                                    c("black",
                                                                      "blue",
                                                                      "red",
                                                                      "yellow",
                                                                      "green",
                                                                      "orange",
                                                                      "violet"
                                                                    ),
                                                                    selected = "blue"
                                                                ),
                                                                # Grid color
                                                                shiny::selectInput(
                                                                    inputId = "gridcol2",
                                                                    h5("Grid Color:"),
                                                                    c("lightgray",
                                                                      "black",
                                                                      "blue",
                                                                      "red",
                                                                      "yellow",
                                                                      "green",
                                                                      "orange",
                                                                      "violet"
                                                                    ),
                                                                    selected = "lightgray"
                                                                ),
                                                                # Show grid
                                                                shiny::checkboxInput("grid2",
                                                                                     label = "Show grid?",
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
                                                                )
                                                            )))
                                    )
                                ))
    ))
)

server <- function(input, output, session) {

    session$onSessionEnded(stopApp)

    # Example Time-to-Failure data
    acid_gas_compressor <- read.csv('data/acid_gas_compressor.csv')

    # Time-to-Failure data handler
    output$failure_data <- shiny::downloadHandler(
        filename = "acid_gas_compressor.csv",
        content = function(file) {
            write.csv(acid_gas_compressor, file, row.names = FALSE)
        }
    )

    # Example Right Censored data
    treat6mp <- read.csv('data/treat6mp.csv')

    # Right Censored data handler
    output$censored_data <- shiny::downloadHandler(
        filename = "treat6mp.csv",
        content = function(file) {
            write.csv(treat6mp, file, row.names = FALSE)
        }
    )

    # Get the data selection from the user
    dat <- shiny::reactive({

        # Get the uploaded file from the user
        inFile <- input$file
        if (is.null(inFile))
            return(NULL)

        dat <- data.frame(read.csv(inFile$datapath))

    })

    # Get the column names
    coln <- shiny::reactive({
        coln <- names(dat())
    })

    # Send the column names to the user
    shiny::observe({
        shiny::updateSelectInput(session, "time", choices = coln())
    })
    shiny::observe({
        shiny::updateSelectInput(session, "event", choices = coln())
    })
    shiny::observe({
        shiny::updateSelectInput(session, "qty", choices = coln())
    })
    shiny::observe({
        shiny::updateSelectInput(session, "left", choices = coln())
    })
    shiny::observe({
        shiny::updateSelectInput(session, "right", choices = coln())
    })

    # Get the event names when the data is right-censored
    # eventn <- shiny::reactive({
    #   if (is.null(input$event) || input$suspensions == 0) {
    #     eventn <- NULL
    #   } else if (input$suspensions == 1) {
    #     eventn <- unique(dat()[, input$event])
    #   }
    # })

    # Send the event names to the user
    # shiny::observe({
    #   shiny::updateSelectInput(session, "fail_code", choices = eventn())
    # })

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
                wblr_dat0 <- subset(event == 0)
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
    # output$table <- DT::renderDT({
    #     if (is.null(dat()))
    #         return(NULL)
    #
    #     shiny::validate(
    #         shiny::need(!is.null(dat()), message = FALSE)
    #     )
    #
    #     DT::datatable(dat(), rownames = FALSE,
    #                   options = list(columnDefs = list(
    #                       list(className = 'dt-center', targets = "_all")
    #                   ))
    #                   # , editable = TRUE
    #     )
    # })

    # Get data edits from the user
    # observeEvent(input$table_cell_edit, {
    #   dat <<- editData(dat(), input$table_cell_edit, 'table', rownames = FALSE)
    # })

    # Create a wblr object
    wblr_obj <- shiny::reactive({
        if (is.null(wblr_dat()))
            return(NULL)

        # Error handling
        # Need error handling for intervals
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

        # Run the wblr object (Weibayes)
        if (input$dist == "weibayes") {

            wblr_obj <-
                WeibullR::wblr.fit(
                    WeibullR::wblr(
                        x = wblr_dat(),
                        interval = ints_dat(),
                        pp = input$pp
                    ),
                    method.fit = input$dist,
                    weibayes.beta = input$beta

                )
        }

        # Run the wblr object (non-Weibayes)
        else {

            wblr_obj <-
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
                ci = input$cl)

        }
    })

    # Create a suspensions vector
    susp_vec <- shiny::reactive({
        if (is.null(input$event) || input$suspensions == 0) {
            susp_vec <- NULL
        } else if (input$suspensions == 1) {
            # susp_vec <- NULL
            susp_vec <- as.numeric(unlist(subset(wblr_dat(), event == 0, select = 'time')))
        }
    })

    # Build the probability plot
    output$probPlot <- plotly::renderPlotly({
        if (is.null(wblr_obj()))
            return(NULL)

        WeibullR.plotly::plotly_wblr(
            wblr_obj(),
            susp = susp_vec(),
            showSusp = input$suspPlot,
            showRes = input$resTab,
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

    })

    # Build the contour plot
    output$contPlot <- plotly::renderPlotly({
        if (is.null(wblr_obj()))
            return(NULL)
        shiny::validate(
            shiny::need(
                try(input$meth == "mle"),
                "Contour plots are only available for the ''MLE' estimation method..."
            ) %then%
                shiny::need(
                    try(input$mleConf == 'lrb'),
                    "Contour plots are only available for the 'LRB' confidence method..."
                )
        )
        WeibullR.plotly::plotly_contour(
            wblr_obj(),
            main = input$main2,
            xlab = input$xlab2,
            ylab = input$ylab2,
            col = input$col2,
            gridCol = input$gridcol2,
            showGrid = input$grid2,
            signif = input$signif2
        )
    })

}

shiny::shinyApp(ui, server, enableBookmarking = "url")
