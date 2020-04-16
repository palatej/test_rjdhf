library(rjdhf)

# Holidays is a versatile funtion for creating calendars (= list of (weighted) Holidays) 
calendar<-rjdhf::Holidays("Easter", list("Easter", offset=1))

#creates a daily matrix with the different holidays (1 or "weight" for the holiday, 0 elsewhere)
hol<-rjdhf::HolidaysMatrix(calendar, startingDate = "1980-01-01", length = as.integer(600))

# more complete calendar

be_calendar<-rjdhf::Holidays("NewYear", "Easter", "EasterMonday", "MayDay", "Ascension", "WhitMonday", c(7, 21), 
                             "Assumption", "AllSaintsDay", "Armistice")

be_hol<-rjdhf::HolidaysMatrix(be_calendar, startingDate = "1980-01-01", length = as.integer(6000))


# Matrix for weeks (startingDate/length should be adapted)
n<-dim(be_hol$ptr)[1]
whol<-rowsum(be_hol$ptr, 1+floor((0:(n-1))/7))

plot(rowSums(whol), type='l')


