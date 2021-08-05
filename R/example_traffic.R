suppressPackageStartupMessages(library(rjd3highfreq))
traffic<-read.csv("./Data/traffic.csv")
y<-log(traffic[-(1:5844),2])

# Create holidays. See also testholidays.R for other examples and for weekly variables.
jhol<-rjd3highfreq::Holidays("NewYear",c(3, 21), "GoodFriday", "EasterMonday", c(4, 27), "MayDay", c(6,16),
                             c(8, 9), c(9, 24), c(12, 16), "Christmas", list("Christmas", offset=1))

hol<-rjd3highfreq::HolidaysMatrix(jhol, "2010-01-01", length = length(y), type = "Default")
vars<-hol$ptr

# RegArima (fractional airline), using the pre-specified regression variables (X), any periodicities (all are considered together)
# and automatic outlier detection. Possible outliers are additive outliers (ao = ...0 0 1 0 0 ...), 
# level shifts (ls = ...0 0 1 1 1...) and "shift" outliers (wo = 0 0 1 -1 0 ...)

rslt0<-rjd3highfreq::fractionalAirlineEstimation(y, x=vars, periods=c(7))

# some output (will be improved in future releases)
print(rslt0$estimation$parameters)
print(rslt0$model$variables)
print(rslt0$model$b)
print(rslt0$model$b/sqrt(diag(rslt0$model$bcov)))

rslt<-rjd3highfreq::fractionalAirlineEstimation(y, x=vars, periods=c(7, 365.25), ndiff=2)

# some output (will be improved in future releases)
print(rslt$estimation$parameters)
print(rslt$model$variables)
print(rslt$model$b)
print(rslt$model$b/sqrt(diag(rslt$model$bcov)))

# linearized series (y-Xb)      
lin<-rslt$model$linearized

c<-rjd3highfreq::fractionalAirlineDecomposition(lin, period=7)
c1<-rjd3highfreq::multiAirlineDecomposition(c$decomposition$sa, periods=365, ndiff=2)
# final decomposition (w=weekly component, s=annual component. Final seasonal component is w+s (+calendar effets))
w<-c$decomposition$s
t<-c1$decomposition$t
sa<-c1$decomposition$sa
s<-c1$decomposition$s
i<-c1$decomposition$i

seatsdecomp<-cbind(lin,t,sa,w,s,i)

c2<-rjd3highfreq::multiAirlineDecomposition(lin, periods=c(7, 365.25), ndiff=2)
w<-c2$decomposition[[2]]
t<-c2$decomposition[[1]]
s<-c2$decomposition[[3]]
i<-c2$decomposition[[4]]
sa<-t+i
seatsdecomp2<-cbind(lin,t,sa,w,s,i)


# Some charts. In this example, we can see that weekly component and annual component are not independent.
plot(exp(seatsdecomp[1:1097, "s"]), type="l")
lines(exp(seatsdecomp[1:1097, "w"]), col="red")

plot(exp(seatsdecomp2[1:1097, "s"]), type="l")
lines(exp(seatsdecomp2[1:1097, "w"]), col="red")

plot(exp(seatsdecomp[1:140, "w"]), type='l')
lines(exp(seatsdecomp2[1:140, "w"]), col="red")

plot(exp(seatsdecomp[, "lin"]), type='l')
lines(exp(seatsdecomp[, "sa"]), col='blue')
lines(exp(seatsdecomp[, "t"]), col='red')
plot(exp(seatsdecomp2[, "lin"]), type='l')
lines(exp(seatsdecomp2[, "sa"]), col='blue')
lines(exp(seatsdecomp2[, "t"]), col='red')


# RegArima (fractional airline), using the pre-specified regression variables (X), any periodicities (all are considered together)
# and automatic outlier detection. Possible outliers are additive outliers (ao = ...0 0 1 0 0 ...), 
# level shifts (ls = ...0 0 1 1 1...) and "shift" outliers (wo = 0 0 1 -1 0 ...)

rslt0_nout<-rjd3highfreq::fractionalAirlineEstimation(y, x=vars, periods=c(7), outliers = NULL)

# some output (will be improved in future releases)
print(rslt0_nout$estimation$parameters)
print(rslt0_nout$model$variables)
print(rslt0_nout$model$b)
print(rslt0_nout$model$b/sqrt(diag(rslt0_nout$model$bcov)))

rslt_nout<-rjd3highfreq::fractionalAirlineEstimation(y, x=vars, periods=c(7, 365.25), ndiff=-1, outliers = NULL)

# some output (will be improved in future releases)
print(rslt_nout$estimation$parameters)
print(rslt_nout$model$variables)
print(rslt_nout$model$b)
print(rslt_nout$model$b/sqrt(diag(rslt_nout$model$bcov)))

# create the model
sm<-rjd3sts::model()
eq<-rjd3sts::equation("eq")
# create the components and add them to the model
rjd3sts::add(sm, rjd3sts::noise("n"))
rjd3sts::add(eq, "n")
rjd3sts::add(sm, rjd3sts::locallineartrend("ll"))
rjd3sts::add(eq, "ll")
rjd3sts::add(sm, rjd3sts::seasonal("s", 7, type="HarrisonStevens"))
rjd3sts::add(eq, "s")
rjd3sts::add(sm, rjd3sts::reg("x", vars))
rjd3sts::add(eq, "x")
rjd3sts::add(sm, eq)
#estimate the model
smrslt<-rjd3sts::estimate(sm, y, marginal=F, initialization="Diffuse", optimizer="LevenbergMarquardt", concentrated=TRUE, precision = 1e-10)
pos<-result(smrslt, "ssf.cmppos")
smfstates<-result(smrslt, "ssf.smoothing.states")
smw<-smfstates[,pos[3]+1]
smt<-smfstates[,pos[2]+1]
smi<-smfstates[,pos[1]+1]
plot(exp(seatsdecomp[1:140, "w"]), type='l')
lines(exp(seatsdecomp2[1:140, "w"]), col="red")
lines(exp(smw[1:140]), col="blue")

plot(exp(seatsdecomp[seq(7,2000,7), "w"]), type='l')
lines(exp(seatsdecomp2[seq(7,2000,7), "w"]), col="red")
lines(exp(smw[seq(7,2000,7)]), col="blue")

sm<-rjd3sts::model()
eq<-rjd3sts::equation("eq")
# create the components and add them to the model
rjd3sts::add(sm, rjd3sts::noise("n"))
rjd3sts::add(eq, "n")
rjd3sts::add(sm, rjd3sts::locallineartrend("ll"))
rjd3sts::add(eq, "ll")
rjd3sts::add(sm, rjd3sts::seasonal("s", 365, type="HarrisonStevens"))
rjd3sts::add(eq, "s")
rjd3sts::add(sm, eq)
#estimate the model
smrslt2<-rjd3sts::estimate(sm, smfstates[,pos[1]+1]+smfstates[,pos[2]+1], marginal=F, initialization="SqrtDiffuse", optimizer="LevenbergMarquardt", concentrated=TRUE, precision = 1e-10)
pos2<-result(smrslt2, "ssf.cmppos")
smfstates2<-result(smrslt2, "ssf.smoothing.states")
smy<-smfstates2[,pos[3]+1]
plot(exp(smy[1:1097]), type="l")
lines(exp(smw[1:1097]), col="red")


sm<-rjd3sts::model()
eq<-rjd3sts::equation("eq")
# create the components and add them to the model
rjd3sts::add(sm, rjd3sts::noise("n"))
rjd3sts::add(eq, "n")
rjd3sts::add(sm, rjd3sts::locallineartrend("ll"))
rjd3sts::add(eq, "ll")
rjd3sts::add(sm, rjd3sts::periodic("s1", 365, as.integer(seq(1,9))))
rjd3sts::add(eq, "s1")
rjd3sts::add(sm, eq)
#estimate the model
smrslt3<-rjd3sts::estimate(sm, lin-smw, marginal=F, initialization="Augmented_NoCollapsing", optimizer="LevenbergMarquardt", concentrated=TRUE, precision = 1e-10)
pos3<-result(smrslt3, "ssf.cmppos")
smfstates3<-result(smrslt3, "ssf.smoothing.states")
smy3<-rowSums(smfstates3[,pos3[3]+seq(1,17,2)])
smt<-smfstates3[,pos3[2]+1]
plot(exp(smy3[1:1097]), type="l")
lines(exp(smw[1:1097]), col="red")
