library(knn)
library(class)
library(MASS)
library(lubridate)
library(unbalanced)
data(crabs, package = "MASS")
noshows = read.csv("noshows.csv")
levels(noshows$No.show) = 0:1
output = noshows$No.show
n = ncol(noshows)
input = noshows[ ,-n] #Without the No.show
data = ubUnder(X=input, Y= output) 
newData = cbind(data$X, data$Y)
newData$AppointmentDay = wday(as.Date(newData$AppointmentDay), label = T)
levels(newData$AppointmentDay) = 0:6
iidx = sample(1:dim(newData)[1], 100)
noShowsTrain = newData[-iidx, ]
noShowsTest = newData[iidx, ]
noShowsModel = knn(noShowsTrain[, -n], noShowsTest[,-n], noShowsTrain[,n], 1)
table(noShowsModel, noShowsTest[,n])