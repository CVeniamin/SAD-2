library(unbalanced)
noshows = read.csv("noshows.csv")
levels(noshows$No.show) = 0:1 
output = noshows$No.show
n = ncol(noshows)
input = noshows[ ,-n] #Without the No.show
data = ubUnder(X=input, Y= output) 
newData = cbind(data$X, data$Y)
summary(newData)