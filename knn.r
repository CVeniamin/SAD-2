library(class)
library(MASS)
library(unbalanced)
library(lubridate)
library(caret) 

data(crabs, package = "MASS")
noshows = read.csv("noshows.csv")

#import function from pre-process file
source("./pre-process.r")
set.seed(4232)
#applying knn to crabs
shuffleData = function(d){
  set.seed(4232)
  shuffledData<-d[sample(nrow(d)),]
  summary(shuffledData)
  shuffledData<-shuffledData[sample(nrow(shuffledData)),]
  return(shuffledData)
}


applyKNN = function(data, testPercentage = 0.3, k = 3,  shuffle = TRUE){
  set.seed(4232)
  sample_size = 0
  size = nrow(data) * testPercentage
  if(shuffle){
    sample_size = sample(1:dim(shuffleData(data))[1], size)
  }else{
    sample_size = sample(1:dim(data)[1], size)
  }
  n = ncol(data)
  
  if(testPercentage <= 0.5 & testPercentage > 0){
    train = data[-sample_size, -n]
    test = data[sample_size, -n]
    trainTarget = data[-sample_size, n]
    testTarget = data[sample_size, n]
    model = knn(train, test, cl=trainTarget, k)
    res = table(model, testTarget)
    #table(model, testTarget)
    confusionMatrix(res)
  }
  
}

#normalize crabs data set including sex
preprocessed_crabs = preprocessCrabs(crabs)
normalized_crabs = cbind(as.data.frame(lapply(preprocessed_crabs[,c(2,3,4,5,6)], normalize)), sp = preprocessed_crabs[,1])
summary(normalized_crabs)

#apply knn to crabs
applyKNN(normalized_crabs, 0.1, k = 3, shuffle = T)


#normalize noshows dataset only (scheduledday, appointment, age, neighbourhood) 
preprocessed_noshows = preprocessNoshows(noshows, 47)
normalized_noshows = cbind(as.data.frame(lapply(preprocessed_noshows[,c(1,2,3,4,5)], normalize)), preprocessed_noshows[,c(6,7,8,9,10,11,12)])
summary(normalized_noshows)

#apply knn to noshows
applyKNN(normalized_noshows, 0.3, k = 11, shuffle = T)
