library(kernlab)
library(e1071)
data(crabs, package = "MASS")
set.seed(4232)
source('./pre-process.r')
processedCrabs = preprocessCrabs((crabs))
crabModel = svm(processedCrabs$sp ~., data=processedCrabs)
summary(crabModel)
levels(processedCrabs$sex) = 0:1
tm = tune(svm, sp ~. , data = processedCrabs, ranges= list(epsilon = seq(0,1,0.1), cost= 2^(2:9)))

#plot(tm)
mymodel = tm$best.model
summary(mymodel)
#plot(crabModel, data = processedCrabs, sex ~.)
#plot(mymodel, dsata = processedCrabs, sex ~., slice = list(sex = 0, sex = 1))
tab = predict(crabModel, processedCrabs[,-1])
table(model = tab, test = processedCrabs[,1])


mytab = predict(mymodel, processedCrabs[,-1])
table(model = mytab , test = processedCrabs[,1])

#data(iris)
#crabModel = svm(iris$Species ~., data=iris)
#plot(crabModel, data = iris, iris$Petal.Length ~iris$Petal.Width, slice = list(iris$Sepal.Length = 3, iris$Sepal.Width = 4))
