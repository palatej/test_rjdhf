library(rjdhf)
edf<-read.table("./Data/edf.txt")
y<-log(edf[,1])

# Complete "two-steps" seasonal adjustment.
# Step 1. RegArima pre-processing
# Step 2. Decomposition of the linearized series (in this example, canonical decomposition is used. 
# It could be replaced by X11 or STL (or others). see testall.R for examples
# The final result (combining step 1 and step 2) is not automated, but is trivial (regressors and coefficients
# are provided in the output). 

# Create holidays. See also testholidays.R for other examples and for weekly variables.
jhol<-rjdhf::Holidays(list("Christmas", offset=-1), "Christmas", list("Christmas", offset=1),
                      c(12,31), "NewYear", list("NewYear", offset=1),
                      "EasterMonday", "MayDay", "Ascension", "WhitMonday", "Assumption", 
                      "AllSaintsDay", "Armistice",
                      c(5,8), c(7,14))

hol<-rjdhf::HolidaysMatrix(jhol, "1996-01-01", length = length(y), type = "Default")

# RegArima (fractional airline), using the pre-specified regression variables (X), any periodicities (all are considered together)
# and automatic outlier detection. Possible outliers are additive outliers (ao = ...0 0 1 0 0 ...), 
# level shifts (ls = ...0 0 1 1 1...) and "shift" outliers (wo = 0 0 1 -1 0 ...)

rslt<-rjdhf::fractionalAirlineEstimation(y, x=hol$ptr, periods=c(7, 365.25), outliers=c("ao", "ls","wo"), criticalValue = 6)

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

# Some charts. In this example, we can see that weekly component and annual component are not independent.
plot(exp(seatsdecomp[1:1097, "s"]), type="l")
lines(exp(seatsdecomp[1:1097, "w"]), col="red")