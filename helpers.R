# 
source("gmpe.R")
plot_gmpe <- function(Mw, R.max, R.min, model) {
  ML <- MW2ML(Mw)
  R <- seq(R.min, R.max, length.out=300)
  plot(R, CB.NPP4(ML,R), log="xy", xlab="Distance(km)", ylab="PGA(g)", ylim=c(0.0001,4),type="n")
  lines(R, CB.NPP4(ML,R), col="red")
}