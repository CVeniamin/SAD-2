library(class)
library(MASS)
library(unbalanced)
library(lubridate)
library(caret) 
library(nnet)

# -----------------------------NO-SHOWS--------------------------------------- #
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

n = ncol(data)

#training set that goes into training of network
proto_train = data[-sample_size, ]
proto_test = data[sample_size, ]

#results10: Column ShowUp of the last 10%
results10 <- proto_test[,ncol(proto_test)]

#Train Neural Network, Formula = ShowUp, input data, number of hidden layers, etc
neuralnet.noshows <- nnet(ShowUp ~ ., data = proto_train, size=9, decay = 0, maxit = 300)

#Use previous network to classify ShowUp column of proto_test
prediction <- predict(neuralnet.noshows, proto_test, type = "class")

#compare predicted results with real ShowUp column from test set
t <- table(prediction, results10)
confusionMatrix(t)
                 
# ------------------------------------CRABS----------------------------------------- #

data(crabs, package = "MASS")
preprocessed_crabs <- preprocessCrabs(crabs)
normalized_crabs = cbind(as.data.frame(lapply(preprocessed_crabs[,c(2,3,4,5,6)], normalize)), sp = preprocessed_crabs[,1])

#DIVIDE dataset into a training set of 75% and 25% test set -----**
testSize_crabs <- nrow(normalized_crabs) * 0.25

#random seed 42 = meaning of life
set.seed(42)
sample_size_crab = 0
sample_size_crab = sample(1:dim(shuffleData(normalized_crabs))[1], testSize_crabs)

#define sets
trainCrabs = normalized_crabs[-sample_size_crab, ]
testCrabs = normalized_crabs[sample_size_crab, ]
resultsCrab = testCrabs[, ncol(testCrabs)]

#feed NN
neuralnet.crabs = nnet(sp ~., data = trainCrabs, size = 1, decay = 0, maxit = 220)

pred_crabs = predict(neuralnet.crabs, testCrabs, type = "class")

tc = table(pred_crabs, resultsCrab)
confusionMatrix(tc)














