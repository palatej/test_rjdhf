library(rjdhf)
uspetroleum<-read.table("./Data/uspetroleum.txt")

y<-uspetroleum[,2]

# X11 applied with exact weekly periodicity
m1<-rjdhf::x11(y, 365.25/7,seas.s1="S3X15")

# X11 applied with approximate weekly periodicity
m2<-rjdhf::x11(y, 52)
n<-dim(uspetroleum)[1]

#plot seasonal components
idx<-(n-53):n
plot(idx, m1$decomposition$s[idx], "l")
lines(idx, m2$decomposition$s[idx], col="red")

# canonical decomposition of a fractional airline model with exact weekly periodicity
a1<-rjdhf::fractionalAirlineDecomposition(log(y), 365.25/7)
lines(idx, exp(a1$decomposition$s[idx]), col="blue")

# plot original series and trend 
plot(1:n, m1$decomposition$y, "l", col="gray")
lines(m2$decomposition$t, col="red")
lines(m1$decomposition$t, col="blue")

# long (3 years) Henderson smoothing of the seasonally adjusted series
plot(rjdhf::henderson(a1$decomposition$sa, length=157), type="l")

