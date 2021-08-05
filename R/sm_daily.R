suppressPackageStartupMessages(library(rjd3sts))

sm_week<-function(y, seasonal="HarrisonStevens"){
  # create the model
  sm<-rjd3sts::model()
  eq<-rjd3sts::equation("eq")
  # create the components and add them to the model
  rjd3sts::add(sm, rjd3sts::noise("n"))
  rjd3sts::add(eq, "n")
  rjd3sts::add(sm, rjd3sts::locallineartrend("ll"))
  rjd3sts::add(eq, "ll")
  rjd3sts::add(sm, rjd3sts::seasonal("s", 7, type=seasonal))
  rjd3sts::add(eq, "s")
  rjd3sts::add(sm, eq)
  #estimate the model
  rslt<-rjd3sts::estimate(sm, y, marginal=F, initialization="SqrtDiffuse", optimizer="LevenbergMarquardt", concentrated=TRUE, precision = 1e-10)
  return(rslt)
}


traffic<-read.csv("./Data/traffic.csv")
y<-log(traffic[-(1:5844),2])

a<-sm_week(y)
b<-sm_week(y[-(1:3738)])
c<-sm_week(y[1:3738])

fa<-result(a, "ssf.filtered.states")
sa<-result(a, "ssf.smoothing.states")
pos<-result(a, "ssf.cmppos")

fb<-result(b, "ssf.filtered.states")
sb<-result(b, "ssf.smoothing.states")

fc<-result(c, "ssf.filtered.states")
sc<-result(c, "ssf.smoothing.states")

sm_plot<-function(day){
  m<-min(fa[seq(3524+day, 3968, 7),pos[3]+1], na.rm = T)
  M<-max(fa[seq(3524+day, 3968, 7),pos[3]+1], na.rm = T)
  plot(fa[seq(3524+day, 3968, 7), pos[3]+1], ylim=c(m, M), type='l', panel.first = abline(v=30.5, col="gray", lwd=10) )
  lines(sa[seq(3524+day, 3968, 7), pos[3]+1], col="red")
  lines(sc[seq(3524+day, 3738, 7), pos[3]+1], col="blue")
}


sm_plot2<-function(day){
  qb<-c(rep(NA, 3738), sb[,pos[3]+1])
  m<-min(fa[seq(3524+day, 3968, 7),pos[3]+1], qb[seq(3524+day, 3968, 7)], na.rm = T)
  M<-max(fa[seq(3524+day, 3968, 7),pos[3]+1], qb[seq(3524+day, 3968, 7)], na.rm = T)
  plot(qb[seq(3524+day, 3968, 7)], ylim=c(m, M), type='l', col="green")
  lines(fa[seq(3524+day, 3968, 7), pos[3]+1], col="black")
  lines(sa[seq(3524+day, 3968, 7), pos[3]+1], col="red")
  lines(sc[seq(3524+day, 3738, 7), pos[3]+1], col="blue")
  
}

sm_plot(1)
sm_plot(2)
sm_plot(3)
sm_plot(4)
sm_plot(5)
sm_plot(6)
sm_plot(7)

b<-sm_week(traffic[-(1:9582), 2])