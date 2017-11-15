library(unbalanced)
library(lubridate)
data(crabs, package = "MASS")
noshows = read.csv("noshows.csv")

preprocessCrabs = function(d){
  #normalize crabs remove species and index
  d = d[,-1]
  d = d[,-2]
  return(d)
}

preprocessed_crabs = preprocessCrabs(crabs)

normalize = function(x){
  return( (as.numeric(x)-min(as.numeric(x))) / (max(as.numeric(x))- min(as.numeric(x))) )
}

#normalize crabs data set including sex
normalized_crabs = as.data.frame(lapply(preprocessed_crabs[,c(1,2,3,4,5,6)], normalize))
str(normalized_crabs)

preprocessNoshows = function(d){
  levels(d$No.show) = 0:1 
  output = d$No.show
  n = ncol(d)
  input = noshows[ ,-n]  #Without the No.show label
  input = input[,-1] #removed PatientID 
  input = input[,-1] #removed AppointmentID
  data = ubUnder(X=input, Y= output)
  newData = cbind(data$X, data$Y)
  
  #normalize AppointmentDay and Scheduled by week day
  newData$AppointmentDay = wday(as.Date(newData$AppointmentDay), label = T)
  levels(newData$AppointmentDay) = 1:7
  
  newData$ScheduledDay = wday(as.Date(newData$ScheduledDay), label = T)
  levels(newData$ScheduledDay) = 1:7
  
  #normalize gender and neighbourhood
  levels(newData$Gender) = as.factor(c(1,2))
  levels(newData$Neighbourhood) = 1:81
  return(newData)
}

preprocessed_noshows = preprocessNoshows(noshows)

#normalize noshows dataset only (scheduledday, appointment, age, neighbourhood) 
normalized_noshows = cbind( preprocessed_noshows[,c(6,7,8,9,10,11)], as.data.frame(lapply(preprocessed_noshows[,c(1,2,3,4,5)], normalize)))

