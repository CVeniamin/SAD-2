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
summary(normalized_noshows)

shuffleData = function(d){
  shuffledData<-d[sample(nrow(d)),]
  shuffledData<-shuffledData[sample(nrow(shuffledData)),]
  return(shuffledData)
}

data <- normalized_noshows
shuffle <- TRUE
testSize <- nrow(data) * 0.3

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




idxTrain <- sample(nrow(normalized_noshows), as.integer(nrow(normalized_noshows)*0.7))
idxTest <- which(! 1:nrow(normalized_noshows) %in% idxTrain)

test = normalized_noshow[,-1]s
