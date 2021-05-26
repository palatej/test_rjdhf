suppressPackageStartupMessages(library(rjd3highfreq))

# Holidays is a versatile funtion for creating calendars (= list of (weighted) Holidays) 
calendar<-rjd3highfreq::Holidays("Easter", list("Easter", offset=1))

#creates a daily matrix with the different holidays (1 or "weight" for the holiday, 0 elsewhere)
hol<-rjd3highfreq::HolidaysMatrix(calendar, startingDate = "1980-01-01", length = as.integer(600))

# more complete calendar

be_calendar<-rjd3highfreq::Holidays("NewYear", "Easter", "EasterMonday", "MayDay", "Ascension", "WhitMonday", c(7, 21), 
                             "Assumption", "AllSaintsDay", "Armistice")

be_hol<-rjd3highfreq::HolidaysMatrix(be_calendar, startingDate = "1980-01-01", length = as.integer(6000))


# Matrix for weeks (startingDate/length should be adapted)
n<-dim(be_hol$ptr)[1]
whol<-rowsum(be_hol$ptr, 1+floor((0:(n-1))/7))

plot(rowSums(whol), type='l')


