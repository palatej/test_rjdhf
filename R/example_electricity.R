suppressPackageStartupMessages(library(rjd3highfreq))
edf<-read.table("./Data/edf.txt")
y<-log(edf[,1])

# Complete "two-steps" seasonal adjustment.
# Step 1. RegArima pre-processing
# Step 2. Decomposition of the linearized series (in this example, canonical decomposition is used. 
# It could be replaced by X11 or STL (or others). see testall.R for examples
# The final result (combining step 1 and step 2) is not automated, but is trivial (regressors and coefficients
# are provided in the output). 

# Create holidays. See also testholidays.R for other examples and for weekly variables.
jhol<-rjd3highfreq::Holidays(list("Christmas", offset=-1), "Christmas", list("Christmas", offset=1),
                      c(12,31), "NewYear", list("NewYear", offset=1),
                      "EasterMonday", "MayDay", "Ascension", "WhitMonday", "Assumption", 
                      "AllSaintsDay", "Armistice",
                      c(5,8), c(7,14))

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

rslt<-rjd3highfreq::fractionalAirlineEstimation(y, x=vars, periods=c(7, 365.25), outliers=c("ao", "ls","wo"), criticalValue = 6)

# some output (will be improved in future releases)
print(rslt$estimation$parameters)
print(rslt$model$variables)
print(rslt$model$b)
print(rslt$model$b/sqrt(diag(rslt$model$bcov)))

# linearized series (y-Xb)      
lin<-rslt$model$linearized

c<-rjd3highfreq::fractionalAirlineDecomposition(lin, period=7, TRUE)
c1<-rjd3highfreq::fractionalAirlineDecomposition(c$decomposition$sa, period=365.25, adjust = FALSE)
# final decomposition (w=weekly component, s=annual component. Final seasonal component is w+s (+calendar effets))
w<-c$decomposition$s
t<-c1$decomposition$t
sa<-c1$decomposition$sa
s<-c1$decomposition$s
i<-c1$decomposition$i
seatsdecomp<-cbind(y,t,sa,w,s,i)

# Some charts. In this example, we can see that weekly component and annual component are not independent.
plot(exp(seatsdecomp[1:1097, "s"]), type="l")
lines(exp(seatsdecomp[1:1097, "w"]), col="red")
