suppressPackageStartupMessages(library(rjd3sts))

har<-1:11

sm_daily<-function(y){
  # create the model
  sm<-rjd3sts::model()
  eq<-rjd3sts::equation("eq")
  # create the components and add them to the model
  rjd3sts::add(sm, rjd3sts::noise("n"))
  rjd3sts::add(eq, "n")
  rjd3sts::add(sm, rjd3sts::locallevel("ll"))
  rjd3sts::add(eq, "ll")
  rjd3sts::add(sm, rjd3sts::seasonal("s", 7, type="HarrisonStevens"))
  rjd3sts::add(eq, "s")
  rjd3sts::add(sm, rjd3sts::periodic("y", period=365.25, harmonics=har))
  rjd3sts::add(eq, "y")
  rjd3sts::add(sm, eq)
  #estimate the model
  rslt<-rjd3sts::estimate(sm, y, marginal=F, initialization="Augmented_NoCollapsing", optimizer="LevenbergMarquardt", concentrated=TRUE, precision = 1e-5)
  return(rslt)
}


births<-read.table("./Data/births.txt")
deaths<-read.table("./Data/deaths.txt")
edf<-read.table("./Data/edf.txt")

y<-edf[,1]
#traffic<-read.csv("./Data/traffic.csv")
#y<-traffic[,2]

a<-sm_daily(log(y))
sa<-result(a, "ssf.smoothing.states")
pos<-result(a, "ssf.cmppos")
plot(rowSums(sa[, pos[4]+seq(1, length(har)*2-1,2)]), type='l')
