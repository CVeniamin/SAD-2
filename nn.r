library(class)
library(MASS)
library(unbalanced)
library(lubridate)
library(nnet)

data(crabs, package = "MASS")
noshows = read.csv("noshows.csv")

#import function from pre-process file
source("./pre-process.r")

preprocessed_noshows <- preprocessNoshows(noshows)
#normalize noshows dataset only (scheduledday, appointment, age, neighbourhood) 
normalized_noshows <- cbind(as.data.frame(lapply(preprocessed_noshows[,c(1,2,3,4,5)], normalize)), preprocessed_noshows[,c(6,7,8,9,10,11,12)])

shuffleData = function(d){
  shuffledData<-d[sample(nrow(d)),]
  shuffledData<-shuffledData[sample(nrow(shuffledData)),]
  return(shuffledData)
}

data <- normalized_noshows
shuffle <- TRUE
testSize <- nrow(data) * 0.1

set.seed(42)
sample_size = 0
if(shuffle){
  sample_size = sample(1:dim(shuffleData(data))[1], testSize)
}else{
  sample_size = sample(1:dim(data)[1], testSize)
}
train = data[-sample_size]
n = ncol(data)
proto_train = data[-sample_size, -(n+1)]
proto_test = data[sample_size, -(n+1)]

results90 <- train[,ncol(train)]
results10 <- proto_test[,ncol(proto_test)]

nnetwork.nn2 <- nnet(ShowUp ~ ., data = proto_train, size=10, decay = 5e-4, maxit = 200)
prediction <- predict(nnetwork.nn2, proto_test, type = "class")

table(prediction, results10)
                 
                 