library(class)
library(MASS)
library(unbalanced)
library(lubridate)
library(caret)
library(kernlab)

data(crabs, package = "MASS")
noshows = read.csv("noshows.csv")

set.seed(4232)
source('./pre-process.r')

#normalize crabs data set including sex
preprocessed_crabs = preprocessCrabs(crabs)
normalized_crabs = cbind(as.data.frame(lapply(preprocessed_crabs[,c(2,3,4,5,6,7)], normalize)), sp = preprocessed_crabs[,1])
summary(normalized_crabs)
m = ncol(normalized_crabs)


applyLVQ = function(data, testPercentage = 0.1, k = 3,  shuffle = TRUE){
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
    
    cd = lvqinit(train, trainTarget)
    cd0 = olvq1(train, trainTarget, cd)
    model = lvqtest(cd0, test)
    #res = table(model, test = testTarget)
    table(model, test = testTarget)
    #confusionMatrix(res)
  }
  
}

#normalize crabs data set including sex
preprocessed_crabs = preprocessCrabs(crabs)
levels(preprocessCrabs$sex) = 0:1
normalized_crabs = cbind(as.data.frame(lapply(preprocessed_crabs[,c(2,3,4,5,6,7)], normalize)), sp = preprocessed_crabs[,1])
summary(normalized_crabs)

#apply knn to crabs
applyLVQ(normalized_crabs, testPercentage =  0.20, shuffle = T)





