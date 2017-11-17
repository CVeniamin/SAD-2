library(class)
library(MASS)
library(unbalanced)
library(lubridate)
library(caret) 
library(nnet)

# -----------------------------NOSHOWS--------------------------------------- #
noshows = read.csv("noshows.csv")

#import function from pre-process file
source("./pre-process.r")

preprocessed_noshows <- preprocessNoshows(noshows, 50)


#normalize noshows dataset only (scheduledday, appointment, age, neighbourhood) 
normalized_noshows <- cbind(as.data.frame(lapply(preprocessed_noshows[,c(1,2,3,4,5)], normalize)), preprocessed_noshows[,c(6,7,8,9,10,11,12)])

shuffleData = function(d){
  shuffledData<-d[sample(nrow(d)),]
  shuffledData<-shuffledData[sample(nrow(shuffledData)),]
  return(shuffledData)
}

#DIVIDE AND SHUFFLE dataset into a training set of 90% and 10% test set -----**
data <- normalized_noshows
shuffle <- TRUE
testSize <- nrow(data) * 0.05

#random seed = meaning of life
set.seed(42)
sample_size = 0
if(shuffle){
  sample_size = sample(1:dim(shuffleData(data))[1], testSize)
}else{
  sample_size = sample(1:dim(data)[1], testSize)
}
train = data[-sample_size]
n = ncol(data)

#training set that goes into training of network
proto_train = data[-sample_size, ]
proto_test = data[sample_size, ]

#results90 unused (probably wrongly calculated)
#results10: Column ShowUp of the last 10%
results90 <- train[,ncol(train)]
results10 <- proto_test[,ncol(proto_test)]

#Train Neural Networl, Formula = ShowUp, input data, number of hidden layers, etc
nnetwork.nn2 <- nnet(ShowUp ~ ., data = proto_train, size=9, decay = 0, maxit = 300)
#Use previous network to classify ShowUp column of proto_test
prediction <- predict(nnetwork.nn2, proto_test, type = "class")

#compare predicted results with real ShowUp column from test set
t <- table(prediction, results10)
confusionMatrix(t)
                 
# ------------------------------------CRABS----------------------------------------- #

data(crabs, package = "MASS")

