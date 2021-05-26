suppressPackageStartupMessages(library(rjd3highfreq))

bedeath<-read.csv("./Data/tf_deaths.csv")

# Belgium daily births
y<-bedeath[,2]

# when a series contains multiple periodicities, we remove them iteratively (using in one step the 
# seasonally adjusted series of the previous step), starting with the highest frequency. The final seasonal component is the sum/prodicut of the different 
# seasonal components; the final trend/irregular/sa are the trend/irregular/sa of the final step.
# That approach is the simplest one and the most robust.

# Apply first multiplicative STL for weekly periodicity. To be noted that multiplicative STL is not provided
# in usual packages
sa1<-rjd3highfreq::stl(y, period=7, multiplicative = TRUE, swindow=53, twindow=9)

# Apply then multiplicative STL for annual (approximate) periodicity on the "seasonally adjusted" series
sa2<-rjd3highfreq::stl(sa1$decomposition$sa, period=365, multiplicative = T, swindow=5)

# irregular + global seasonal component
plot(sa1$decomposition$i, type='l')
lines(sa1$decomposition$s*sa2$decomposition$s, col="red")


plot(sa2$decomposition$s[1:730], type='l')
lines(sa1$decomposition$s[1:730], col="red")

# Apply first multiplicative X11L for weekly periodicity
sa3<-rjd3highfreq::x11(y, period=7)

# Apply then multiplicative X11 for annual periodicity on the "seasonally adjusted" series
sa4<-rjd3highfreq::x11(sa3$decomposition$sa, period=365.25)

# The main problem with STL and X11 is that we don't know which filters we should apply (-> future research).
# Huge differences are possible.
# With the canonical decomposition, optimal filters are automatically obtained. The trend and the seasonal
# components are as smooth as possible (all the noise is in the irregular)
# If we want compute still smoother trend, we should apply - for instance - a long Henderson filter. See testx11
# for an example


plot(sa4$decomposition$s[1:730], type='l')
lines(sa3$decomposition$s[1:730], col="red")

# same with the canonical decomposition (=SEATS-like) of exact fractional airline models
sa5<-rjd3highfreq::fractionalAirlineDecomposition(log(y), period=7)
sa6<-rjd3highfreq::fractionalAirlineDecomposition(sa5$decomposition$sa, period=365.25)


plot(exp(sa6$decomposition$s)[1:730], type='l')
lines(exp(sa5$decomposition$s)[1:730], col="red")

