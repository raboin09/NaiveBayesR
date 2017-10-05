##A skeleton for implementing Naive Bayes Classifier in R
## Author: Salem 

##install.packages("matrixStats")

##install.packages("e1071")

library(matrixStats)

library(e1071)

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
Xnegative = Xtrain[-idp,1:d]

##Testing .....
Xtest=as.matrix(read.table(testingFile))
nn = dim(Xtest)[1] # Number of points in the testing data.

tp = 0 #True Positive
fp = 0 #False Positive
tn = 0 #True Negative
fn = 0 #False Negative

cm <- colMeans(Xtrain)

csd <- colSds(Xtrain)

posClass = Xpositive
posMeans = colMeans(posClass[,1:4])
posStdDev = apply(posClass, 2, sd)
posTotalProb = length(posClass[,"V1"])/length(Xtrain[,"V1"])

negClass = Xnegative
negMeans = colMeans(negClass[,1:4])
negStdDev = apply(negClass, 2, sd)
negTotalProb = length(negClass[,"V1"])/length(Xtrain[,"V1"])

pospred = rowProds(dnorm(Xtest[,1:4], posMeans, posStdDev))

negpred = rowProds(dnorm(Xtest[,1:4], negMeans, negStdDev))

#pospred = rowProds(pospred)
#negpred = rowProds(negpred)

#print(pospred[20:30])
#print(negpred[20:30])

for(i in 1:length(pospred)){
  if(pospred[i] > negpred[i]){
    totalpred[i] = 1
  }
  else{
    totalpred[i] = -1
  }
}

for(i in 1:length(totalpred)){
  if(totalpred[i]==1 && Xtest[i,"V5"]==1)
    tp = tp + 1
  if(totalpred[i]==-1 && Xtest[i,"V5"]==1)
    fn = fn + 1
  if(totalpred[i]==1 && Xtest[i,"V5"]==-1)
    fp = fp + 1
  if(totalpred[i]==-1 && Xtest[i,"V5"]==-1)
    tn = tn + 1
}

print("True pos: ")
print(tp)
print("True neg: ")
print(tn)
print("False pos: ")
print(fp)
print("False neg: ")
print(fn)