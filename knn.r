library(class)
library(MASS)
library(unbalanced)
library(lubridate)
data(crabs, package = "MASS")
noshows = read.csv("noshows.csv")

#import function from pre-process file
source("./pre-process.r")

#applying knn to crabs
shuffleData = function(d){
  shuffledData<-d[sample(nrow(d)),]
  shuffledData<-shuffledData[sample(nrow(shuffledData)),]
  return(shuffledData)
}

applyKNN = function(data, testSize = 30, k = 4,  shuffle = TRUE){
  set.seed(42)
  sample_size = 0
  if(shuffle){
    sample_size = sample(1:dim(shuffleData(data))[1], testSize)
  }else{
    sample_size = sample(1:dim(data)[1], testSize)
  }
  n = ncol(data)
  train = data[-sample_size, -n]
  test = data[sample_size, -n]
  
  trainTarget = data[-sample_size, n]
  testTarget = data[sample_size, n]
  
  model = knn(train, test, cl=trainTarget, k)
  table(model, testTarget)
}

#normalize crabs data set including sex
preprocessed_crabs = preprocessCrabs(crabs)
normalized_crabs = cbind(as.data.frame(lapply(preprocessed_crabs[,c(2,3,4,5,6)], normalize)), sp = preprocessed_crabs[,1])
summary(normalized_crabs)

#apply knn to crabs
applyKNN(normalized_crabs, 45, k = 2, shuffle = T)


#normalize noshows dataset only (scheduledday, appointment, age, neighbourhood) 
preprocessed_noshows = preprocessNoshows(noshows)
normalized_noshows = cbind(as.data.frame(lapply(preprocessed_noshows[,c(1,2,3,4,5)], normalize)), preprocessed_noshows[,c(6,7,8,9,10,11,12)])
summary(normalized_noshows)

#apply knn to noshows
applyKNN(normalized_noshows, 400, k = 4, shuffle = T)
