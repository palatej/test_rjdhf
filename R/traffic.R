library(rjdhf)
traffic<-read.csv("./Data/traffic.csv")
y<-traffic[,2]

# Create holidays. See also testholidays.R for other examples and for weekly variables.
jhol<-rjdhf::Holidays("NewYear",c(3, 21), "GoodFriday", "EasterMonday", "MayDay", c(6,16),
                      c(8, 9), c(9, 24), c(12, 16), "Christmas", list("Christmas", offset=1))

hol<-rjdhf::HolidaysMatrix(jhol, "1994-01-01", length = length(y), type = "Default")
vars<-hol$ptr

# RegArima (fractional airline), using the pre-specified regression variables (X), any periodicities (all are considered together)

rslt<-rjdhf::fractionalAirlineEstimation(y, x=vars, periods=c(7, 365.25))

# some output (will be improved in future releases)
print(rslt$estimation$parameters)
print(rslt$model$variables)
print(rslt$model$b)
print(rslt$model$b/sqrt(diag(rslt$model$bcov)))

# linearized series (y-Xb)      
lin<-rslt$model$linearized

c<-rjdhf::fractionalAirlineDecomposition(lin, period=7, TRUE)
c1<-rjdhf::fractionalAirlineDecomposition(c$decomposition$sa, period=365.25, adjust = FALSE)
# final decomposition (w=weekly component, s=annual component. Final seasonal component is w+s (+calendar effets))
w<-c$decomposition$s
t<-c1$decomposition$t
sa<-c1$decomposition$sa
s<-c1$decomposition$s
i<-c1$decomposition$i
seatsdecomp<-cbind(y,t,sa,w,s,i)

# Some charts. 
n<-length(y)
plot(seatsdecomp[(n-400):n, "w"], type="l")
lines(seatsdecomp[(n-400):n, "s"], col="red")
plot(y[(n-400):n], type="l", col="gray")
lines(seatsdecomp[(n-400):n, "sa"], col="blue")
lines(seatsdecomp[(n-400):n, "t"], col="red")

elin<-lin[(n-228):n]
ec<-rjdhf::fractionalAirlineDecomposition(elin, period=7, TRUE)
# final decomposition (w=weekly component, s=annual component. Final seasonal component is w+s (+calendar effets))
ew<-ec$decomposition$s
et<-ec$decomposition$t
esa<-ec$decomposition$sa

# Some charts. 
plot(seatsdecomp[(n-228):n, "w"], type="l")
lines(ew, col="red")
plot(y[(n-228):n], type="l", col="gray")
lines(esa, col="blue")
lines(et, col="red")
lines(seatsdecomp[(n-228):n, "sa"], col="green")