#' Interactive Probability Plot.
#'
#' @param wblr_obj An object of class 'wblr'.
#' @param susp An optional numeric vector of suspension data.
#' @param main Main title.
#' @param xlab X-axis label.
#' @param ylab Y-axis label.
#' @param col Color of the model fit and confidence bounds
#' @param signif Significant digits of results
#' @param grid Show grid (TRUE) or hide grid (FALSE).
#' @param gridcol Color of the grid.
#' @param intcol Color of the intervals for interval censored models.
#' @examples
#' \dontrun{
#' failures<-c(30, 49, 82, 90, 96)
#' obj<-wblr.conf(wblr.fit(wblr(failures)))
#' plotly_wblr(obj)
#' }
#' @import WeibullR
#' @import plotly
#' @importFrom graphics text
#' @importFrom stats runif
#' @export
plotly_wblr <- function(wblr_obj,
                        susp=NULL,
                        main=NULL,
                        xlab=NULL,
                        ylab=NULL,
                        col=NULL,
                        signif=NULL,
                        grid=NULL,
                        gridcol=NULL,
                        intcol=NULL) {

  # Check for a wblr object
  if(!identical(class(wblr_obj),"wblr")){
    stop("Argument \"wblr_obj\" is not of class \"wblr\".")
  }

  # Check for a numeric vector of suspensions
  if(all(!is.null(susp), !is.numeric(susp))) {
    stop("Argument \"susp\" must be a numeric vector")
  }

  # Extract layout options
  col <- if(missing(col)) 'black' else col
  fillcolor <- plotly::toRGB(col, 0.2)
  main <- if(missing(main)) 'Probaility Plot' else main
  xlab <- if(missing(xlab)) 'Time to Failure' else xlab
  ylab <- if(missing(ylab)) 'Unreliability' else ylab
  signif <- if(missing(signif)) 3 else signif
  xgrid <- ifelse(is.null(grid) || isTRUE(grid), TRUE, FALSE)
  ygrid <- xgrid
  gridcol <- if(missing(gridcol)) 'lightgray' else gridcol
  intcol <- if(missing(intcol)) 'black' else intcol

  ## Extract data from the wblr object

  # Check for intervals and get the probability values
  if(wblr_obj$interval==0) {
    time <- wblr_obj$data$dpoints$time
    time_sd <- round(time, signif)
    ints <- NULL
    probability <- wblr_obj$data$dpoints$ppp
    prob_sd <- round(probability, signif)
  }
  else if(wblr_obj$interval>0) {
    time <- (wblr_obj$data$dlines$t2+wblr_obj$data$dlines$t1)/2
    time_sd <- round(time, signif)
    ints <- log(wblr_obj$data$dlines$t2-wblr_obj$data$dlines$t1)
    probability <- wblr_obj$data$dlines$ppp
    prob_sd <- round(probability, signif)
  }

  # Check for a fit method
  if(is.null(wblr_obj$fit)) {
    datum <- NULL
    unrel <- NULL
    lower <- NULL
    upper <- NULL
  }

  # Check for confidence bounds
  else if(is.null(wblr_obj$fit[[1]]$conf)) {
    datum <- NULL
    unrel <- NULL
    lower <- NULL
    upper <- NULL
  }

  # Get the fit and upper/lower confidence bounds
  else {
    datum <- wblr_obj$fit[[1]]$conf[[1]]$bounds$Datum
    unrel <- wblr_obj$fit[[1]]$conf[[1]]$bounds$unrel
    lower <- wblr_obj$fit[[1]]$conf[[1]]$bounds$Lower
    upper <- wblr_obj$fit[[1]]$conf[[1]]$bounds$Upper
    datum_sd <- round(datum, signif)
    unrel_sd <- round(unrel, signif)
    lower_sd <- round(lower, signif)
    upper_sd <- round(upper, signif)
  }

  # Check the distribution transform probability/unreliability
  if(is.null(wblr_obj$fit)) {
    param1 <- NULL
    param2 <- NULL
    param3 <- NULL
    paramval1 <- NULL
    paramval2 <- NULL
    paramval3 <- NULL
    prob_trans <- NULL
    unrel_trans <- NULL
  }
  else if(wblr_obj$fit[[1]]$options$dist=='lognormal') {
    param1 <- 'Mulog'
    param2 <- 'Sigmalog'
    param3 <- NULL
    paramval1 <- round(as.numeric(wblr_obj$fit[[1]]$fit_vec[1]), signif)
    paramval2 <- round(as.numeric(wblr_obj$fit[[1]]$fit_vec[2]), signif)
    paramval3 <- NULL
    prob_trans <- log(1/(1-probability))
    unrel_trans <- log(1/(1-unrel))
  }
  else if(wblr_obj$fit[[1]]$options$dist=='weibull'){
    param1 <- 'Beta'
    param2 <- 'Eta'
    param3 <- NULL
    paramval1 <- round(as.numeric(wblr_obj$fit[[1]]$fit_vec[2]), signif)
    paramval2 <- round(as.numeric(wblr_obj$fit[[1]]$fit_vec[1]), signif)
    paramval3 <- NULL
    prob_trans <- log(1/(1-probability))
    unrel_trans <- log(1/(1-unrel))
  }
  else if(wblr_obj$fit[[1]]$options$dist=='weibull3p'){
    param1 <- 'Beta'
    param2 <- 'Eta'
    param3 <- 'Gamma'
    paramval1 <- round(as.numeric(wblr_obj$fit[[1]]$fit_vec[2]), signif)
    paramval2 <- round(as.numeric(wblr_obj$fit[[1]]$fit_vec[1]), signif)
    paramval3 <- round(as.numeric(wblr_obj$fit[[1]]$fit_vec[3]), signif)
    prob_trans <- log(1/(1-probability))
    unrel_trans <- log(1/(1-unrel))
  }

  # Check the fit method and get the GOF metrics
  if(is.null(wblr_obj$fit)) {
    methlab <- NULL
    methval <- NULL
  }
  else if(wblr_obj$fit[[1]]$options$method.fit=='rr-xony') {
    methlab <- 'R^2'
    methval <-  round(wblr_obj$fit[[1]]$gof$r2, signif)
  }
  else if(wblr_obj$fit[[1]]$options$method.fit=='mle') {
    methlab <- 'Loglikelihood'
    methval <- round(wblr_obj$fit[[1]]$gof$loglik, signif)
  }

  # Draw random values to represent the y-axis positions for the suspensions plot
  if(is.null(susp)) {
    susp <- NULL
    susp_sd <- NULL
    ry <- NULL
  } else {
    susp_sd <- round(susp, signif)
    ry=stats::runif(length(susp))
  }

  # Build the results table
  res <- data.frame(Parameter=c('Ranks',
                                'n',
                                'Failures',
                                'Intervals',
                                'Suspensions',
                                'Distribution',
                                'Method',
                                param1,
                                param2,
                                param3,
                                methlab,
                                'CI',
                                'Type'),
                    Value = c(wblr_obj$options$pp,
                              wblr_obj$n,
                              wblr_obj$fail,
                              wblr_obj$interval,
                              wblr_obj$cens,
                              wblr_obj$options$dist,
                              wblr_obj$options$method.fit,
                              paramval1,
                              paramval2,
                              paramval3,
                              methval,
                              wblr_obj$options$ci,
                              wblr_obj$options$method.conf)
  )

  # Setup the main probability plot
  yticks <- c(0.000001,0.00001,0.0001,0.001,0.01,0.05,0.1,0.2,0.5,1,2,5,10,20,50,90,99,99.999)
  yticks_trans<- log(1/(1-yticks/100))
  ymin <- min(log10(unrel))
  ymax <- max(log10(yticks_trans))
  xmin <- min(log10(datum))
  xmax <- max(log10(datum))

  # Create the main probability plot
  probPlot <- plotly::plot_ly(x=time, y=prob_trans, type='scatter', mode='markers',
                      marker=list(color='black'), showlegend=FALSE, error_x=list(array=~ints, color=intcol),
                      name="", text=~paste0("Probability: (",time_sd,", ",prob_sd,")"), hoverinfo = 'text'
  ) %>%

    # Specify the main probability plot layout
    plotly::layout(title=main,
           xaxis = list(type='log', title=xlab, showline=TRUE, mirror='ticks',
                        showgrid=xgrid, gridcolor=gridcol, range=list(xmin, xmax)),
           yaxis = list(type='log', title=ylab, showline=TRUE, mirror = 'ticks',
                        size=text, showgrid=ygrid, gridcolor=gridcol, range=list(ymin, ymax),
                        tickvals=yticks_trans, ticktext=yticks)
    ) %>%

    # Add best fit
    plotly::add_trace(x=datum, y=unrel_trans, mode='lines',
              marker=list(color='transparent'), line = list(color = col),
              error_x=list(array=NULL),
              text=~paste0("Fit: ",datum_sd,", ",unrel_sd,")"), hoverinfo = 'text'
    ) %>%

    # Add lower confidence bound
    plotly::add_trace(x=lower, y=unrel_trans, mode='lines',
              marker=list(color='transparent'), line=list(color='transparent'),
              error_x=list(array=NULL),
              text=~paste0("Upper: ",lower_sd,", ",unrel_sd,")"), hoverinfo = 'text'

    ) %>%

    # Add upper confidence bound
    plotly::add_trace(x=upper, y=unrel_trans, mode='lines',
              fill='tonexty',
              fillcolor=fillcolor,
              marker=list(color='transparent'), line=list(color='transparent'),
              error_x=list(array=NULL),
              text=~paste0("Lower: ",upper_sd,", ",unrel_sd,")"), hoverinfo='text'
    )

  # Create the suspension plot
  suspPlot <- plotly::plot_ly(x=susp, y=ry, type='scatter', mode='markers',
                      marker=list(color='black'), showlegend=FALSE,
                      text=~paste0("Suspension: ",susp_sd), hoverinfo='text'
  ) %>%

    # Create the suspension plot layout
    plotly::layout(
      xaxis = list(type='log', title=NULL, zeroline=FALSE, showline=TRUE, mirror='ticks',
                   showticklabels=FALSE, showgrid=FALSE, range=list(xmin, xmax)
      ),
      yaxis = list(title=NULL, zeroline=FALSE, showline=TRUE, mirror='ticks',
                   showticklabels=FALSE, showgrid=FALSE
      )
    )

  # Create the results table
  resTab <- plotly::plot_ly(type='table',
                    domain = list(x = c(0.8, 1), y = c(0, 0.85)),
                    # columnwidth=c(50, 50),
                    header=list(values=names(res), align=c('center','center'),
                                line=list(width=1, color='black'),
                                fill=list(color=c("grey", "grey")),
                                font = list(family="Arial", color="white")
                    ),
                    cells=list(values=rbind(res$Param, res$Value),
                               align=c('center', 'center'),
                               line=list(color="black", width = 1),
                               font=list(family="Arial", color=c("black"))
                    )
  )

  # Build the combination plot
  plotly::subplot(probPlot, suspPlot, resTab, nrows=2, titleX=TRUE, titleY=TRUE) %>%
    plotly::layout(xaxis=list(domain=c(0, 0.75)), xaxis2=list(domain=c(0, 0.75)),
           xaxis3=list(domain=c(0.775, 1)), yaxis=list(domain=c(0, 0.875)),
           yaxis2=list(domain=c(0.9, 1)), yaxis3=list(domain=c(0, 0.85))
    )

}
