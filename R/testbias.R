library(rjd3highfreq)
source("./R/bias.R")

y<-c(750,9970,9000,8620,8630,8360,1010,790,9730,9120,8500,8970,8250,1040,850,10360,10380,11130,
     13050,12250,1580,940,13640,14760,15660,17390,17810,8040,3150,22590,24400,30000,30720,36700,
     21110,9420,39990,37250,34920,32990,23220,13140,6020,19060,33040,28850,26930,24090,11350,5780,
     25170,21800,20530,18290,16930,6660,4530,20930,18540,16570,15470,14980,5070,2970,17920,14670)

cleaning <- rjd3highfreq::fractionalAirlineEstimation(log(y),
                                               period=7,
                                               outliers = c("ao","ls","wo"),criticalValue = 4)

y_clean <- cleaning$model$linearized
c1 <- rjd3highfreq::fractionalAirlineDecomposition(y_clean, period=7)

decomp1<-biasCorrection(exp(y_clean), c1$decomposition$t, c1$decomposition$s, c1$decomposition$i, period = 7)
decomp2<-biasCorrection2(exp(y_clean), c1$decomposition$t, c1$decomposition$s, c1$decomposition$i, period = 7)

plot(y, type='l', col="gray")
lines(decomp1$sa, col="red" )
lines(decomp2$sa, col="blue" )

plot(y, type='l', col="gray")
lines(decomp1$t, col="red" )
lines(decomp2$t, col="blue" )

plot(decomp1$s, type='l', col="red")
lines(decomp2$s, col="blue" )

lam <- 1/2
z <- y^lam
cleaning <- rjd3highfreq::fractionalAirlineEstimation(z,
                                               period=7,
                                               outliers = c("ao","ls","wo"),
                                               criticalValue = 5)

tval_b <- cleaning$model$b/sqrt(diag(cleaning$model$bcov))
data.frame(cleaning$model$variables,tval_b)

y_clean <- cleaning$model$linearized
c2 <- rjd3highfreq::fractionalAirlineDecomposition(y_clean, period=7)
