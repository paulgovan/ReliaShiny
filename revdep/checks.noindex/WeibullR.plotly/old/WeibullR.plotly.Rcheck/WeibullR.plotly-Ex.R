pkgname <- "WeibullR.plotly"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('WeibullR.plotly')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("plotly_contour")
### * plotly_contour

flush(stderr()); flush(stdout())

### Name: plotly_contour
### Title: Interactive Contour Plot
### Aliases: plotly_contour

### ** Examples

library(WeibullR)
library(WeibullR.plotly)

failures1 <- c(30, 49, 82, 90, 96)
failures2 <- c(20, 40, 60, 80, 100)
obj1 <- wblr.conf(wblr.fit(wblr(failures1), method.fit = 'mle'), method.conf = 'lrb')
obj2 <- wblr.conf(wblr.fit(wblr(failures2), method.fit = 'mle'), method.conf = 'lrb')
plotly_contour(list(obj1, obj2), main = "Overlayed Contours")




cleanEx()
nameEx("plotly_duane")
### * plotly_duane

flush(stderr()); flush(stdout())

### Name: plotly_duane
### Title: Interactive Duane Plot.
### Aliases: plotly_duane

### ** Examples

library(ReliaGrowR)
times<-c(100, 200, 300, 400, 500)
failures<-c(1, 2, 1, 3, 2)
fit<-duane_plot(times, failures)
plotly_duane(fit)



cleanEx()
nameEx("plotly_rga")
### * plotly_rga

flush(stderr()); flush(stdout())

### Name: plotly_rga
### Title: Interactive Reliability Growth Plot.
### Aliases: plotly_rga

### ** Examples

library(ReliaGrowR)
times<-c(100, 200, 300, 400, 500)
failures<-c(1, 2, 1, 3, 2)
rga<-rga(times, failures)
plotly_rga(rga)



cleanEx()
nameEx("plotly_wblr")
### * plotly_wblr

flush(stderr()); flush(stdout())

### Name: plotly_wblr
### Title: Interactive Probability Plot.
### Aliases: plotly_wblr

### ** Examples

library(WeibullR)
library(WeibullR.plotly)
failures<-c(30, 49, 82, 90, 96)
obj<-wblr.conf(wblr.fit(wblr(failures)))
plotly_wblr(obj)



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
