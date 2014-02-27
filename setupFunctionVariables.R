library(qcc)
my.xmr.raw <- c(5045,4350,4350,3975,4290,4430,4485,4285,3980,3925,3645,3760,3300,3685,3463,5200)
my.xmr.new <- round(c(runif(5, 3500, 4000)))
x <- qcc(my.xmr.raw, type = "xbar.one")
x <- qcc(my.xmr.raw, type = "xbar.one", newdata = my.xmr.new)
x <- qcc(matrix(cbind(my.xmr.raw[1:length(my.xmr.raw)-1], my.xmr.raw[2:length(my.xmr.raw)]), ncol = 2), type = "R")
add.stats <- TRUE
chart.all <- TRUE
label.limits <- c("LCL", "UCL", "CL")
axes.las <- 0
digits <-  getOption("digits")
restore.par <- TRUE
font.size <- 12
plot.new <- TRUE
digits <- getOption("digits")
title <- NULL
xlab <- NULL
ylab <- NULL
ylim <- NULL
