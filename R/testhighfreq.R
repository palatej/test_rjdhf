suppressPackageStartupMessages(library(rjd3highfreq))

births<-read.table("./Data/births.txt")
deaths<-read.table("./Data/deaths.txt")
edf<-read.table("./Data/edf.txt")

y<-edf[,1]

# sa of daily series with X11
a<-rjd3highfreq::x11(y, period=7, mul = TRUE, seas.s0="S3X15", seas.s1="S3X15", trend.horizon = 9)
a1<-rjd3highfreq::x11(a$decomposition$sa, period=365.25/12, mul = TRUE, seas.s0="S3X15", seas.s1="S3X15", trend.horizon = 65)
a2<-rjd3highfreq::x11(a1$decomposition$sa, period=365.25, mul = TRUE, trend.horizon = 367)
wa2<-a2$decomposition$s

# The final decomposition is given by
w<-a$decomposition$s
m<-a1$decomposition$s
t<-a2$decomposition$t
sa<-a2$decomposition$sa
s<-a2$decomposition$s
i<-a2$decomposition$i
x11decomp<-cbind(y,t,sa,w,m, s,i)
##

plot(wa2[2000:2500], type="l")

a2<-rjd3highfreq::x11(a1$decomposition$sa, period=365, mul = TRUE, trend.horizon = 367)
wa2<-a2$decomposition$s

plot(wa2[2000:2500], type="l")

b<-rjd3highfreq::stl(y, period=7, multiplicative = FALSE, swindow=15, twindow=75)
b1<-rjd3highfreq::stl(b$decomposition$sa, period=365, multiplicative = FALSE)
wb<-b1$decomposition$s
plot(wb[2000:2500], type="l")

# The final decomposition is given by
w<-b$decomposition$s
t<-b1$decomposition$t
sa<-b1$decomposition$sa
s<-b1$decomposition$s
i<-b1$decomposition$i
stldecomp<-cbind(y,t,sa,w, s,i)

c<-rjd3highfreq::fractionalAirlineDecomposition(log(y), period=7)
c1<-rjd3highfreq::fractionalAirlineDecomposition(c$decomposition$sa, period=365.25/12, adjust = FALSE)
c2<-rjd3highfreq::fractionalAirlineDecomposition(c1$decomposition$sa, period=365.25, adjust = FALSE)
wc<-c2$decomposition$s
plot(wc[2000:2500], type="l")

# The final decomposition is given by
w<-c$decomposition$s
m<-c1$decomposition$s
t<-c2$decomposition$t
sa<-c2$decomposition$sa
s<-c2$decomposition$s
i<-c2$decomposition$i
seatsdecomp<-cbind(y,t,sa,w,m, s,i)
   