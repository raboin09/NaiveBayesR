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
posMeans = colMeans(posClass)
posStdDev = colSds(posClass)
posTotalProb = length(posClass[,"V1"])/length(Xtrain[,"V1"])

negClass = Xtrain[-idp, 1:d]
negMeans = colMeans(negClass)
negStdDev = colSds(negClass)
negTotalProb = length(negClass[,"V1"])/length(Xtrain[,"V1"])

#print(posMeans[1])
#print(posStdDev[1])

#print(negMeans[1])
#print(negStdDev[1])

#print(negTotalProb)
#print(posTotalProb)

#print(posClass[1, "V1"])

#testFunc <- function(a, b) a + b
#apply(dat[,c('x','z')], 1, function(x) testFunc(x[1],x[2]))

singleColFunc <- function(colVal, colStd, colMean) dnorm(colVal, colMean, colStd)
totalColFunc <- function(col1, col2, col3, col4) prod(col1, col2, col3, col4)

prob <- singleColFunc(posClass[1, "V1"], posStdDev[1], posMeans[1])

print(prob)

#posprob <- dnorm(posClass[1, "V1"], posMeans["V1"], posStdDev[1]) * dnorm(posClass[1, "V2"], posMeans["V2"], posStdDev[2]) * dnorm(posClass[1, "V3"], posMeans["V3"], posStdDev[3]) * dnorm(posClass[1, "V4"], posMeans["V4"], posStdDev[4])

#negprob <- dnorm(posClass[1, "V1"], negMeans["V1"], negStdDev[1]) * dnorm(posClass[1, "V2"], negMeans["V2"], negStdDev[1]) * dnorm(posClass[1, "V3"], negMeans["V3"], negStdDev[1]) * dnorm(posClass[1, "V4"], negMeans["V4"], negStdDev[1])

#applyProbs <- apply(posClass[,"V1"], 2, function(x) singleColFunc(posClass, posStdDev, posMeans))

#print(applyProbs)

#print(posprob)

#prob <- dnorm(Xtrain[,"V1"], cm[1], csd[1])


