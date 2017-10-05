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



posMeans <- colMeans(Xpositive[,1:d])
posStdDev <- apply(Xpositive, 2, sd)
posTotalProb <- dim(Xpositive)[1]/n

negMeans <- colMeans(Xnegative[,1:d])
negStdDev <- apply(Xnegative, 2, sd)
negTotalProb <- dim(Xnegative)[1]/n

for(i in 1:nn){
  posScore <- 1
  negScore <- 1
  predictedScore <- 0
  
  for(j in 1:d){
    posScore <- posScore * dnorm(Xtest[i,j], posMeans[j], posStdDev[j])
    negScore <- negScore * dnorm(Xtest[i,j], negMeans[j], negStdDev[j])
  }
  
  posScore <- posScore * posTotalProb
  negScore <- negScore * negTotalProb
  
  if(posScore > negScore){
    predictedScore <- 1
  }
  else{
    predictedScore <- -1
  }
  
  realScore = Xtest[i,d+1]
  
  if(is.null(predictedScore)){
    print("predicted score is null")
  }
  if(is.null(realScore)){
    print("test score is null")
  }
  if(predictedScore == 1 && realScore == 1){
    tp = tp + 1
  }
  else if(predictedScore == 1 && realScore == -1){
    fp = fp + 1
  }
  else if(predictedScore == -1 && realScore == -1){
    tn = tn + 1
  }
  else if(predictedScore == -1 && realScore == 1){
    fn = fn + 1
  }
  
}

print("True pos: ")
print(tp)
print("True neg: ")
print(tn)
print("False pos: ")
print(fp)
print("False neg: ")
print(fn)

precision <- (tp+tn)/nn

recall <- (tp)/(tp+fn)

print("Precision:")
print(precision)
print("Recall:")
print(recall)