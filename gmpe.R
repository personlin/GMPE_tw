# GMPEs

# Wu et. al., 2001
# ML-MW relation
MW2ML <- function(MW){
  ML <- 4.533*log(MW)-2.091
  return(ML)
}
ML2MW <- function(ML){
  MW <- exp((ML+2.091)/4.533)
  return(MW)
}

# ---------------------------------------------------------------------
#    ***  Wu et al.(2001) for Taiwan PGA ***
# ---------------------------------------------------------------------

WU2001 <- function (mag, rrup ){
  
  lnY <- 0.00215 + 0.581 * mag - log10(rrup + 0.00871 * 10^(0.5 * mag)) - 0.00414 * rrup
  Y <-10^lnY
  Y
}

#Campbell form
CB.NPP4 <- function(M, R){
  b1 <- 0.003324
  b2 <- 1.7331
  b3 <- 2.0639
  b4 <- 0.09992
  b5 <- 0.7718
  y <- b1*exp(b2*M)*(R+b4*exp(b5*M))^-b3
  return(y)
}

#Joyner and Boore
JB.NPP4 <- function(M, R){
  b1 <- -0.976
  b2 <- 0.3759
  b3 <- -1.1535
  b4 <- 0.768
  b5 <- 10.0029
  y <- 10^(b1+b2*M+b3*log10((R^2+b5^2)^b4))
  return(y)
}

#Japan rock site
JR.NPP4 <- function(M, R){
  b1 <- 4.82277
  b2 <- 0.6995
  b3 <- 0.0197
  b4 <- 97.6056
  b5 <- 134.999
  y <- (10^(((R+b4)/b5)*(-b1+b2*M-b3*M^2)))/0.981
  return(y)
}

#Kanai
KA.NPP4 <- function(M, R){
  b1 <- 1.77181
  b2 <- 1.0557
  b3 <- 2.3478
  b4 <- 40.7041
  y <- b1*exp(b2*M)*(R+b4)^-b3
  return(y)
}

#-------------------------------------------------------
#  簡文郁
#
#  Period       b1      b2      b3      b4      b5      σlnErr
#  PGA          0.0028  1.7331  2.0639  0.0999  0.7719  0.7815
#  Sas (T=0.3s) 0.0079  1.7253  2.0489  0.1199  0.7850  0.7201
#  Sa1 (T=1.0s) 0.0027  1.7731  2.0419  0.1154  0.7714  0.7018
#   Jean, W.Y., Chang, Y. W., Wen, K. L., and Loh, C. H., 2006. Early Estimation of  Seismic  Hazard  for  Strong  Earthquakes  in  Taiwan,  
#   Natural  Hazards, 37(1-2), 39-53. (引用出處，但未見係數表)
#   張毓文、簡文郁、邱世彬，2010，金、馬及澎湖地區之設計地震研擬，財團法人國家實驗研究院國家地震工程研究中心NCREE-10-016  (有係數表)
#-------------------------------------------------------
NCREE2006 <- function (mag, R, site="Rock"){
  pga <- 0.0028*exp(1.7331*mag)*(R+0.0999*exp(0.7719*mag))^-2.0639    #堅硬地盤, ML? ,震央距以及距斷層面最短距離(for Chi-Chi earthquake)
  pga
}
