##A skeleton for implementing Naive Bayes Classifier in R
## Author: Salem 

##install.packages("matrixStats")

library(matrixStats)

##You can use setwd("C:/YourPATH here/") to set the working directory..
setwd("C:/Users/james.raboin/Desktop/NaiveBayesR-Master/")

trainingFile = "irisTraining.txt"
testingFile = "irisTesting.txt"
Xtrain = as.matrix(read.table(trainingFile))
n = dim(Xtrain)[1]
d = dim(Xtrain)[2] - 1 # the last attribute is the class label, so it does not count.
##Training... Collect mean and standard deviation for each dimension for each class..
##Also, calculate P(C+) and P(C-)
idp = which(Xtrain[,d+1] ==1) # points that have 1 as the class label
np = length(idp)
Xpositive = Xtrain[idp,1:d]

##Testing .....
Xtest=as.matrix(read.table(testingFile))
nn = dim(Xtest)[1] # Number of points in the testing data.

tp = 0 #True Positive
fp = 0 #False Positive
tn = 0 #True Negative
fn = 0 #False Negative

cm <- colMeans(Xtrain)

csd <- colSds(Xtrain)

posClass = Xtrain[idp, 1:d]
posClassLength = length(posClass)
posMeans = colMeans(posClass)
posStdDev = colSds(posClass)
posTotalProb = length(posClass[,"V1"])/length(Xtrain[,"V1"])

negClass = Xtrain[-idp, 1:d]
negClassLength = length(negClass)
negMeans = colMeans(negClass)
negStdDev = colSds(negClass)
negTotalProb = length(negClass[,"V1"])/length(Xtrain[,"V1"])

prediction = rowSums(exp(dnorm(Xtrain, posMeans, posStdDev) * posTotalProb))

negprediction = rowSums(exp(dnorm(Xtrain, negMeans, negStdDev) * negTotalProb))



print("Num of 1's:")
print(sum(prediction > negprediction))
print("Num of -1's:")
print(sum(prediction < negprediction))




