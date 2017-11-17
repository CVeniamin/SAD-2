library(rpart)
library(MASS)
library(rattle)
library(caret)
library(rpart.plot)
library(RColorBrewer)

data(crabs, package = "MASS")
noshows = read.csv("noshows.csv")

set.seed(4232)
source('./pre-process.r')

applyDT = function(x, y, testPercentage = 0.1, shuffle = TRUE, noShow = F){
  set.seed(4232)
  sample_size = 0 #training sample size
  data = cbind(x, y = y)
  size = nrow(data) * testPercentage
  if(shuffle){
    sample_size = sample(1:dim(shuffleData(data))[1], size)
  }else{
    sample_size = sample(1:dim(data)[1], size)
  }
  n = ncol(data)
  
  if(testPercentage <= 0.5 & testPercentage > 0){
    train = data[-sample_size, ]
    test = data[sample_size, ]
    trainTarget = data[-sample_size, n]
    testTarget = data[sample_size, n]
    
    # if(noShow){
    #   model = rpart(y ~ Gender + Handcap + Alcoholism + Diabetes + Age +  AppointmentDay  + ScheduledDay + Neighbourhood,
    #               data=train,
    #               method="class")
    #   
    #   # grow tree 
    #   #xy = cbind(x, ShowUp = y)
    #   fancyRpartPlot(model)
    # }else{
    #   model = rpart(y ~ sex + FL + CW + CL + BD + RW,
    #               data=train,
    #               method="class")
    #   fancyRpartPlot(model)
    # }
    
    DTmodel <- rpart(y ~ ., data = train, method="class", cost = 2^(2:n))
    fancyRpartPlot(DTmodel)
    
    #Predict Output 
    print(DTmodel)
    predicted= predict(DTmodel, test, type = "class")
    
    res = table(predicted, test = testTarget)
    confusionMatrix(res)
  }
  
}

#normalize crabs data set including sex
preprocessed_crabs = preprocessCrabs(crabs)
normalized_crabs = cbind(as.data.frame(lapply(preprocessed_crabs[,c(2,3,4,5,6,7)], normalize)))
summary(normalized_crabs)
sp = preprocessed_crabs[,1]

#apply DT to crabs
applyDT(normalized_crabs, sp, testPercentage = 0.2, shuffle = T)

#normalize noshows dataset only (scheduledday, appointment, age, neighbourhood) 
preprocessed_noshows = preprocessNoshows(noshows, 50)
normalized_noshows = cbind(as.data.frame(lapply(preprocessed_noshows[,c(1,2,3,4,5)], normalize)), preprocessed_noshows[,c(6,7,8,9,10,11)])

#apply DT to noshows
ShowUp = preprocessed_noshows[,12]
applyDT(normalized_noshows, ShowUp, testPercentage = 0.1, shuffle = T)

