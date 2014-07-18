# 
library(ggplot2)
source("gmpe.R")

plot_gmpe <- function(Mw, R.max, R.min, model) {
  ML <- MW2ML(Mw)
  R <- seq(R.min, R.max, length.out=300)
  #get all GMPE prediction first
  GMPEs <- c("WU2001", "CB2004", "JB2004", "JR2004", "KA2004", "NCREE2006", "TS2006", "LIN2009", "LIN2011")
  GMPE.names <- rep(GMPEs, each=length(R) )
  Rs <- rep(R, length(GMPEs))
  GMPE.pga <- c(WU2001(Mw, R), CB.NPP4(ML, R), JB.NPP4(ML, R), JR.NPP4(ML, R), KA.NPP4(ML, R), NCREE2006(ML, R), TS2006(ML, R),
                  PS2009(Mw, R, pri="PGA", Vs30=760, NM=0, RV=0), Lin2011.PGA(Mw, R, site="BC"))
  GMPE.prediction <- data.frame(Distance=Rs, PGA=GMPE.pga, GMPE=GMPE.names)
  #subset selected GMPE
  GMPE.plot <- subset(GMPE.prediction, GMPE %in% model)
  p <- ggplot(GMPE.plot, aes(x=Distance, y=PGA, group=GMPE, colour=GMPE)) + labs(x="Distance(km)", y="PGA(g)", xlim(R.min,R.max)) 
  p <- p + geom_line() + scale_x_log10() + scale_y_log10()
  p 
}