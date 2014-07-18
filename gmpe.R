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
#    ***  Wu et al.(2001) for Taiwan PGA ***  (Mw)
# ---------------------------------------------------------------------

WU2001 <- function(mag,R) {
  pga <- 10**(0.00215+0.581*mag-log10(R+0.00871*10**(0.5*mag))-0.00414*R)/978.88
  pga
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

#-------------------------------------------------------
#
#   The Path Effect in Ground-Motion Variability: An Application of the Variance-Components Technique
#   Tsai et al.
#   BSSA 2006 96: 1170-1176.
#   蔡主權等人
#  ML Hypocentral distance
#-------------------------------------------------------
TS2006 <- function(mag, R){
  # mag = local magnitude ML
  # R = hypocentral distance
  c0 <- 0.4063
  c1 <- 0.7936
  c2 <- -0.02146
  c3 <- 0.0004183
  c4 <- -1.7056
  c5 <- 5.7814
  c6 <- -0.05656
  log.PGA <- c0 + c1*mag + c2*mag^2 + c3*R + c4*log10(R + c5*10^(c6*mag))
  PGA <- (10^log.PGA)/978.88
  PGA
}

#-------------------------------------------------------
#   Po-Shen Lin PhD Thesis 
#
#-------------------------------------------------------
PS2009 <- function(Mag, ClstD, pri="0.01", Vs30=760, NM=0, RV=0){
  
  #--- import parameters
  pars <- read.table("data/PS2009_filter.txt", row.names=13, header=TRUE)
  
  c1T<-pars[pri,]$c1
  c2T<-pars[pri,]$c2
  c3T<-pars[pri,]$c3
  c4T<-pars[pri,]$c4
  c5T<-pars[pri,]$c5
  c6T<-pars[pri,]$c6
  c7T<-pars[pri,]$c7
  H<-pars[pri,]$H
  phi1<-pars[pri,]$c8
  
  lnY <-c1T+c2T*(Mag-6.3)*ifelse(Mag<=6.3, 1, 0)+(-H*c5T)*(Mag-6.3)*ifelse(Mag<=6.3, 0, 1)+c3T*(8.5-Mag)^2+(c4T+c5T*(Mag-6.3))*log(sqrt(ClstD^2 + exp(H)^2))+c6T*NM+c7T*RV+phi1*log(Vs30/1130)
  Y <- exp(lnY)
  Y
}

#-------------------------------------------------------
#  Lin et al.(2011) 
# Po-Shen Lin, Chyi-Tyi Lee, Chin-Tung Cheng, Chih-Hsuan Sung, (2011).
# Response spectral attenuation relations for shallow crustal earthquakes in Taiwan,
# Engineering Geology, Vol. 121, 150?V164.
#-------------------------------------------------------

Lin2011.PGA <- function(mag,R,site="BC"){
  if(site == "BC") CHW <- c(-3.279, 1.035, -1.651, 0.1520, 0.623) else C <- c(-3.248, 0.943, -1.471, 0.1000, 0.648)
  if(site == "BC") CFW <- c(-3.232, 1.047, -1.662, 0.1920, 0.630) else C <- c(-3.218, 0.935, -1.464, 0.1250, 0.650)
  sa.hw=exp(CHW[1]+CHW[2]*mag+CHW[3]*log(R+CHW[4]*exp(CHW[5]*mag)))
  sa.fw=exp(CFW[1]+CFW[2]*mag+CFW[3]*log(R+CFW[4]*exp(CFW[5]*mag)))
  sa <- sqrt(sa.hw*sa.fw)
  sa
}
