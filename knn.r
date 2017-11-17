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

applyKNN = function(data, testPercentage = 0.1, k = 3,  shuffle = TRUE){
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
    #res = table(model, test = testTarget)
    table(model, test = testTarget)
    #confusionMatrix(res)
  }
  
}

#normalize crabs data set including sex
preprocessed_crabs = preprocessCrabs(crabs)
normalized_crabs = cbind(as.data.frame(lapply(preprocessed_crabs[,c(2,3,4,5,6,7)], normalize)), sp = preprocessed_crabs[,1])
summary(normalized_crabs)

#apply knn to crabs
applyKNN(normalized_crabs, testPercentage =  0.20, k = 3, shuffle = T)


#normalize noshows dataset only (scheduledday, appointment, age, neighbourhood) 
preprocessed_noshows = preprocessNoshows(noshows, 50)
normalized_noshows = cbind(as.data.frame(lapply(preprocessed_noshows[,c(1,2,3,4)], normalize)), preprocessed_noshows[,c(6,7,8,9,10,11,12)])
summary(normalized_noshows)

#apply knn to noshows
applyKNN(normalized_noshows, testPercentage = 0.1, k = 31, shuffle = T)
