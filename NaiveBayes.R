##A skeleton for implementing Naive Bayes Classifier in R
## Author: Salem 

library(matrixStats)

##You can use setwd("C:/YourPATH here/") to set the working directory..
setwd("C:/Users/james.raboin/Desktop/NB/")

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

print(colnames(Xtrain))

cm <- colMeans(Xtrain)

print(cm)

csd <- colSds(Xtrain)

print(csd)

posClass = 0

negClass = 0

totalClass = 0

for(currentInt in Xtrain[, "V5"]){
  if(currentInt > 0){ posClass = posClass+1 }
  else { negClass = negClass+1 }
  totalClass = totalClass + 1
}

print(idp)

print(posClass/100) #P(C+)

print(negClass/100) #P(C-)

print(totalClass)

print(csd[1])

inv <- ((1/(sqrt(2*pi)*csd[1])))*exp(((4.6 - cm[1])^2)/(2*(csd[1]^2)))

print(inv)

print(Xtrain[58, "V1"])

prob <- dnorm(Xtrain[,"V1"], cm[1], csd[1])

print(prob)


