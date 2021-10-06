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

sm_weekt<-function(y, seasonal="HarrisonStevens", pos){
  len<-length(y)
  t<-matrix(c(rep(1, pos), rep(0, len-pos)), len, 1 )
  # create the model
  sm<-rjd3sts::model()
  eq<-rjd3sts::equation("eq")
  # create the components and add them to the model
  rjd3sts::add(sm, rjd3sts::locallineartrend("ll"))
  rjd3sts::add(eq, "ll")
  rjd3sts::add(sm, rjd3sts::seasonal("s", 7, type=seasonal))
  rjd3sts::add(eq, "s")
  rjd3sts::add(sm, rjd3sts::reg("x", t))
  rjd3sts::add(eq, "x")
  rjd3sts::add(sm, rjd3sts::noise("n"))
  rjd3sts::add(eq, "n")
  rjd3sts::add(sm, eq)
  #estimate the model
  rslt<-rjd3sts::estimate(sm, y, marginal=F, initialization="SqrtDiffuse", optimizer="LevenbergMarquardt", concentrated=TRUE, precision = 1e-10)
  return(rslt)
}


sm_var<-function(y, period, std_t, std_w, std_n, seasonal="HarrisonStevens"){
  # create the model
  sm<-rjd3sts::model()
  eq<-rjd3sts::equation("eq")
  # create the components and add them to the model
  rjd3sts::add(sm, rjd3sts::varlocallineartrend("ll", lstd = std_t))
  rjd3sts::add(eq, "ll")
  rjd3sts::add(sm, rjd3sts::varseasonal("s", period, type=seasonal, std = std_w))
  rjd3sts::add(eq, "s")
  rjd3sts::add(sm, rjd3sts::varnoise("n", std=std_n))
  rjd3sts::add(eq, "n")
  rjd3sts::add(sm, eq)
  #estimate the model
  rslt<-rjd3sts::estimate(sm, y, initialization="SqrtDiffuse")
}


traffic<-read.csv("./Data/traffic.csv")
y<-log(traffic[-(1:5844),2])
std_t<-rep(1, length(y))
std_t[3738]<-100
std_w<-rep(1, length(y))
std_w[3738:3744]<-1

rslt<-sm_var(y, 7, std_t, std_w, std_w)
states<-result(rslt, "ssf.smoothing.states")

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

d<-rslt

fd<-result(d, "ssf.filtered.states")
sd<-result(d, "ssf.smoothing.states")
dpos<-result(d, "ssf.cmppos")

d1<-sm_var(y[1:3768], 7, std_t, std_w, std_t)

fd1<-result(d1, "ssf.filtered.states")
sd1<-result(d1, "ssf.smoothing.states")

d0<-sm_var(y[1:3745], 7, std_t, std_w, std_t)

fd0<-result(d0, "ssf.filtered.states")
sd0<-result(d0, "ssf.smoothing.states")

sm_plot<-function(day){
  m<-min(fa[seq(3524+day, 3968, 7),pos[3]+1], na.rm = T)
  M<-max(fa[seq(3524+day, 3968, 7),pos[3]+1], na.rm = T)
  plot(fa[seq(3524+day, 3968, 7), pos[3]+1], ylim=c(m, M), type='l', panel.first = abline(v=30.5, col="gray", lwd=10) )
  lines(sa[seq(3524+day, 3968, 7), pos[3]+1], col="red")
  lines(sc[seq(3524+day, 3738, 7), pos[3]+1], col="blue")
  lines(sd[seq(3524+day, 3968, 7), dpos[2]+1], col="magenta")
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

wsa<-sa[3524:3968]

smt_compare<-function(y, var, pos){
  std_t<-rep(1, length(y))
  std_t[pos]<-var
  std_w<-rep(1, length(y))
  rslt<-sm_var(y, 7, std_t, std_w, std_w)
  vstates<-result(rslt, "ssf.smoothing.states")
  vpos<-result(rslt, "ssf.cmppos")
  vt<-vstates[,vpos[1]+1]
  
  rslt<-sm_weekt(y, pos=pos)
  tstates<-result(rslt, "ssf.smoothing.states")
  tpos<-result(rslt, "ssf.cmppos")
  t<-c(rep(1, pos), rep(0, length(y)-pos))
  tt<-tstates[,tpos[1]+1]+tstates[,tpos[3]+1]*t
  
  return (cbind(vt,tt))
}
