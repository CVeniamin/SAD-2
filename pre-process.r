library(unbalanced)
library(lubridate)

#If planing to test this file it's needed to import dataset before anything else
#data(crabs, package = "MASS")
#noshows = read.csv("noshows.csv")

preprocessCrabs = function(d){
  #normalize crabs remove index
  #d = d[,-1]
  d = d[,-3]
  return(d)
}

normalize = function(x){
  return( (as.numeric(x)-min(as.numeric(x))) / (max(as.numeric(x))- min(as.numeric(x))) )
}

#Usage example
#preprocessed_crabs = preprocessCrabs(crabs)
#normalize crabs data set including sex
#normalized_crabs = as.data.frame(lapply(preprocessed_crabs[,c(1,2,3,4,5,6)], normalize))
#summary(normalized_crabs)

preprocessNoshows = function(d, percentage, overSample=F){
  set.seed(4232)
  levels(d$No.show) = 0:1
  output = d$No.show
  n = ncol(d)
  input = d[,-c(1,2,n)] #removed PatientID, AppointmentID and noshow columns
  if(overSample){
    data = ubOver(X=input, Y= output)
  }else{
    data = ubUnder(X=input, Y= output, perc = percentage, method = "percPos")  
  }
  
  #data$Y represents noshowing yes/no
  newData = cbind(data$X, ShowUp = data$Y)
  
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


#Usage example
#preprocessed_noshows = preprocessNoshows(noshows)
#normalize noshows dataset only (scheduledday, appointment, age, neighbourhood) 
#normalized_noshows = cbind(as.data.frame(lapply(preprocessed_noshows[,c(1,2,3,4,5)], normalize)), preprocessed_noshows[,c(6,7,8,9,10,11,12)])
#summary(normalized_noshows)

