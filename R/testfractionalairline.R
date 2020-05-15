library(rjdhf)
source("./R/bias.R")
usclaims<-read.table("./Data/usclaims.txt")
y=usclaims[,1]
rslt<-rjdhf::fractionalAirlineEstimation(log(y), periods=c(365.25/7), outliers=c("ao"), criticalValue = 6)

m1<-fractionalAirlineDecomposition(rslt$model$linearized, 365.25/7)
m2<-fractionalAirlineDecomposition(rslt$model$linearized, 365.25/7, FALSE)
n<-length(y)
idx<-(n-206):n
plot(idx, m1$decomposition$s[idx], "l")
lines(idx, m2$decomposition$s[idx], col="red")

plot(1:n, m1$decomposition$y, "l", col="gray")
lines(m1$decomposition$t, col="red")
lines(m2$decomposition$t, col="blue")

y=usclaims[,2]
rslt<-rjdhf::fractionalAirlineEstimation(log(y), periods=c(365.25/7), outliers=c("ao"), criticalValue = 6)

m1<-fractionalAirlineDecomposition(rslt$model$linearized, 365.25/7)
m2<-fractionalAirlineDecomposition(rslt$model$linearized, 365.25/7, FALSE)

plot(idx, m1$decomposition$s[idx], "l")
lines(idx, m2$decomposition$s[idx], col="red")

plot(1:n, m1$decomposition$y, "l", col="gray")
lines(m1$decomposition$t, col="red")
lines(m2$decomposition$t, col="blue")

test_ll<-function(series, low, high, step){
  p<-seq(low, high, by=step)
  ll<-rep(NA, length(p))
  i<-1
  for (q in p){
    m<-fractionalAirlineDecomposition(series, q, TRUE)
    cat(q)
    cat('\t')
    cat(m$likelihood$ll)
    cat('\n')    
    ll[i]<-m$likelihood$ll
    i<-i+1
  }
  return (ll)
}

## Maximum is reached very near the actual weekly periodicity (365.25/7 = 52.1786)
#plot(test_ll(usclaims$V1, 52.15, 52.20, 0.001), type='l')

#bias correction
decomp<-biasCorrection(exp(rslt$model$linearized), m1$decomposition$t, m1$decomposition$s, m1$decomposition$i, 365.25/7)

par(mfrow=c(2,1))

plot(decomp$y, type='l', col="gray")
lines(decomp$sa, col="blue")
lines(decomp$t, col="red")
plot(decomp$s, type="l", col="magenta")
lines(decomp$i, col="green")

print(summary(decomp$y-decomp$t*decomp$s*decomp$i))

par(0)
