suppressPackageStartupMessages(library(rjd3highfreq))

ga_week<-function(y, period=7){
  jrslt<-rjd3highfreq::fractionalAirlineDecomposition.raw(y, period)
  jssf<-rjd3highfreq::fractionalAirlineDecomposition.ssf(jrslt)
  return(jssf)
}

ga_week_ucm<-function(y, period=7){
  rslt<-rjd3highfreq::fractionalAirlineDecomposition(y, period)
  
  return(rslt)
}

traffic<-read.csv("./Data/traffic.csv")
y<-log(traffic[-(1:5844),2])

a<-ga_week(y)
b<-ga_week(y[-(1:3738)])
c<-ga_week(y[1:3738])

fa<-result(a, "filtered.states")
sa<-result(a, "smoothing.states")
pos<-result(a, "cmppos")

fb<-result(b, "filtered.states")
sb<-result(b, "smoothing.states")

fc<-result(c, "filtered.states")
sc<-result(c, "smoothing.states")


sm_plot<-function(day){
  qb<-c(rep(NA, 3738), sb[,pos[2]+1])
  m<-min(fa[seq(3524+day, 3968, 7),pos[2]+1], qb[seq(3524+day, 3968, 7)], na.rm = T)
  M<-max(fa[seq(3524+day, 3968, 7),pos[2]+1], qb[seq(3524+day, 3968, 7)], na.rm = T)
  plot(qb[seq(3524+day, 3968, 7)], ylim=c(m, M), type='l', col="green")
  lines(fa[seq(3524+day, 3968, 7), pos[2]+1], col="black")
  lines(sa[seq(3524+day, 3968, 7), pos[2]+1], col="red")
  lines(sc[seq(3524+day, 3738, 7), pos[2]+1], col="blue")
  
}

sm_plot(1)
sm_plot(2)
sm_plot(3)
sm_plot(4)
sm_plot(5)
sm_plot(6)
sm_plot(7)
