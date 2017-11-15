library(unbalanced)
library(lubridate)
noshows = read.csv("noshows.csv")
levels(noshows$No.show) = 0:1 
output = noshows$No.show
n = ncol(noshows)
input = noshows[ ,-n] #Without the No.show
data = ubUnder(X=input, Y= output) 
newData = cbind(data$X, data$Y)
summary(newData)


#normalize AppointmentDay and Scheduled by week day
newData$AppointmentDay = wday(as.Date(newData$AppointmentDay), label = T)
levels(newData$AppointmentDay) = 0:6

newData$ScheduledDay = wday(as.Date(newData$ScheduledDay), label = T)
levels(newData$ScheduledDay) = 0:6

#normalize gender and neighbourhood
levels(newData$Gender) = 0:1
levels(newData$Neighbourhood) = 1:81

