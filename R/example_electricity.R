suppressPackageStartupMessages(library(rjd3highfreq))
#edf<-read.table("./Data/edf.txt")

edf2<-read.csv("./Data/edf.csv")

y<-log(edf2$y)

# Complete "two-steps" seasonal adjustment.
# Step 1. RegArima pre-processing
# Step 2. Decomposition of the linearized series (in this example, canonical decomposition is used. 
# It could be replaced by X11 or STL (or others). see testall.R for examples
# The final result (combining step 1 and step 2) is not automated, but is trivial (regressors and coefficients
# are provided in the output). 

# Create holidays. See also testholidays.R for other examples and for weekly variables.
jhol<-rjd3highfreq::Holidays("NewYear", list("NewYear", offset=1), 
                             "EasterMonday", "MayDay", "Ascension", "WhitMonday", c(5,8), c(7,14),
                             "Assumption", "AllSaintsDay", "Armistice",
                             list("Christmas", offset=-1), "Christmas", list("Christmas", offset=1), c(12,31)
                      )

hol<-rjd3highfreq::HolidaysMatrix(jhol, "1996-01-01", length = length(y), type = "Default")

# adding some user-defined variables. Dummies for end of months (first obs at 1/1/leapyear)
months<-c(31,28,31,30,31,30,31,31,30,31,30,31)
lpmonths<-c(31,29,31,30,31,30,31,31,30,31,30,31)
y4<-c(lpmonths,months,months,months)
cy40<-cumsum(c(y4, y4, y4, y4, y4,y4, y4, y4, y4, y4))
idx<-cy40[1:(Position(function(x){x>length(y)}, cy40)-1)]

endofmonth1<-array(0,dim=length(y))
endofmonth2<-array(0,dim=length(y))

endofmonth1[idx]<-1
endofmonth2[idx-1]<-1

vars<-cbind(hol$ptr, endofmonth1, endofmonth2)


# RegArima (fractional airline), using the pre-specified regression variables (X), any periodicities (all are considered together)
# and automatic outlier detection. Possible outliers are additive outliers (ao = ...0 0 1 0 0 ...), 
# level shifts (ls = ...0 0 1 1 1...) and "shift" outliers (wo = 0 0 1 -1 0 ...)

rslt0<-rjd3highfreq::fractionalAirlineEstimation(y, x=vars, periods=c(7), outliers=c("ao", "ls"), criticalValue = 6)

# some output (will be improved in future releases)
print(rslt0$estimation$parameters)
print(rslt0$model$variables)
print(rslt0$model$b)
print(rslt0$model$b/sqrt(diag(rslt0$model$bcov)))

rslt<-rjd3highfreq::fractionalAirlineEstimation(y, x=vars, periods=c(7, 365.25), ndiff=3, outliers=c("ao", "ls"), criticalValue = 6)

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

m3<-fractionalAirlineEstimation(lin, periods=c(7, 365), ndiff=3)


c3<-rjd3highfreq::multiAirlineDecomposition(lin, periods=c(7, 365.25), ndiff=3)
w<-c3$decomposition[[2]]
t<-c3$decomposition[[1]]
s<-c3$decomposition[[3]]
#i<-c3$decomposition[[4]]
#sa<-t+i
seatsdecomp3<-cbind(lin,t,w,s)


# Some charts. In this example, we can see that weekly component and annual component are not independent.
plot(exp(seatsdecomp[1:1097, "s"]), type="l")
lines(exp(seatsdecomp[1:1097, "w"]), col="red")

plot(exp(seatsdecomp2[1:1097, "s"]), type="l")
lines(exp(seatsdecomp2[1:1097, "w"]), col="red")

plot(exp(seatsdecomp[1:140, "w"]), type='l')
lines(exp(seatsdecomp2[1:140, "w"]), col="red")

plot(exp(seatsdecomp[-(1:6000), "lin"]), type='l')
lines(exp(seatsdecomp[-(1:6000), "sa"]), col='blue')
lines(exp(seatsdecomp[-(1:6000), "t"]), col='red')
plot(exp(seatsdecomp2[-(1:6000), "lin"]), type='l')
lines(exp(seatsdecomp2[-(1:6000), "sa"]), col='blue')
lines(exp(seatsdecomp2[-(1:6000), "t"]), col='red')




# Create holidays. See also testholidays.R for other examples and for weekly variables.
jhol<-rjd3highfreq::Holidays("NewYear", list("NewYear", offset=1), 
                             "EasterMonday", "MayDay", "Ascension", "WhitMonday", c(5,8), c(7,14),
                             "Assumption", "AllSaintsDay", "Armistice",
                             list("Christmas", offset=-1), "Christmas", list("Christmas", offset=1), c(12,31)
)

hol<-rjd3highfreq::HolidaysMatrix(jhol, "1996-01-01", length = length(y), type = "Default")
vars<-hol$ptr


# RegArima (fractional airline), using the pre-specified regression variables (X), any periodicities (all are considered together)
# and automatic outlier detection. Possible outliers are additive outliers (ao = ...0 0 1 0 0 ...), 
# level shifts (ls = ...0 0 1 1 1...) and "shift" outliers (wo = 0 0 1 -1 0 ...)

rslt0_nout<-rjd3highfreq::fractionalAirlineEstimation(y, x=vars, periods=c(7), outliers = NULL)

# some output (will be improved in future releases)
print(rslt0_nout$estimation$parameters)
print(rslt0_nout$model$variables)
print(rslt0_nout$model$b)
print(rslt0_nout$model$b/sqrt(diag(rslt0_nout$model$bcov)))

rslt_nout<-rjd3highfreq::fractionalAirlineEstimation(y, x=vars, periods=c(7, 365.25), ndiff=3, outliers = NULL)

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
