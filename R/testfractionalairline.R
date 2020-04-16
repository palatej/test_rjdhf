library(rjdhf)

usclaims<-read.table("./Data/usclaims.txt")

m1<-fractionalAirlineDecomposition(log(usclaims[,1]), 365.25/7)
m2<-fractionalAirlineDecomposition(log(usclaims[,1]), 365.25/7, FALSE)
n<-dim(usclaims)[1]
idx<-(n-206):n
plot(idx, m1$decomposition$s[idx], "l")
lines(idx, m2$decomposition$s[idx], col="red")

plot(1:n, m1$decomposition$y, "l", col="gray")
lines(m1$decomposition$t, col="red")
lines(m2$decomposition$t, col="blue")

m1<-fractionalAirlineDecomposition(log(usclaims[,2]), 365.25/7)
m2<-fractionalAirlineDecomposition(log(usclaims[,2]), 365.25/7, FALSE)

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

# Maximum is reached very near the actual weekly periodicity (365.25/7 = 52.1786)
plot(test_ll(usclaims$V2, 52.15, 52.20, 0.001), type='l')