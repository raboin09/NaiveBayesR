
#A skeleton for implementing Naive Bayes Classifier in Python.
## Author: Salem 
from __future__ import division
import numpy
import scipy.stats as stats



trainingFile = "buyTraining.txt"
testingFile = "buyTesting.txt"


Xtrain = numpy.loadtxt(trainingFile)
numOfRows = Xtrain.shape[0]
numOfAttributes = Xtrain.shape[1]-1

Xtest = numpy.loadtxt(testingFile, dtype=int)

numTestRows = Xtest.shape[0]

tp = 0 #True Positive
fp = 0 #False Positive
tn = 0 #True Negative
fn = 0 #False Negative

label = [0, 0]

#CREATE A LIST OF LISTS THAT HOLDS THE TRUE/FALSE VALUES OF EACH TYPE OF ATTRIBUTE FOR EACH COLUMN

att1Stats = [[0,0],[0,0],[0,0]]
att2Stats = [[0,0],[0,0],[0,0]]
att3Stats = [[0,0],[0,0]]
att4Stats = [[0,0],[0,0]]

#ITERATE OVER LIST TO INCREMENT VALUES

for x in range(0, numOfRows):
    if Xtrain[x][0]==1 and Xtrain[x][4]==1:
        att1Stats[0][1] += 1
    elif Xtrain[x][0]==1 and Xtrain[x][4]==-1:
        att1Stats[0][0] += 1
    if Xtrain[x][0]==2 and Xtrain[x][4]==1:
        att1Stats[1][1] += 1
    elif Xtrain[x][0]==2 and Xtrain[x][4]==-1:
        att1Stats[1][0] += 1
    if Xtrain[x][0]==3 and Xtrain[x][4]==1:
        att1Stats[2][1] += 1
    elif Xtrain[x][0]==3 and Xtrain[x][4]==-1:
        att1Stats[2][0] += 1

for x in range(0, numOfRows):
    if Xtrain[x][1]==1 and Xtrain[x][4]==1:
        att2Stats[0][1] += 1
    elif Xtrain[x][1]==1 and Xtrain[x][4]==-1:
        att2Stats[0][0] += 1
    if Xtrain[x][1]==2 and Xtrain[x][4]==1:
        att2Stats[1][1] += 1
    elif Xtrain[x][1]==2 and Xtrain[x][4]==-1:
        att2Stats[1][0] += 1
    if Xtrain[x][1]==3 and Xtrain[x][4]==1:
        att2Stats[2][1] += 1
    elif Xtrain[x][1]==3 and Xtrain[x][4]==-1:
        att2Stats[2][0] += 1

for x in range(0, numOfRows):
    if Xtrain[x][2]==1 and Xtrain[x][4]==1:
        att3Stats[0][1] += 1
    elif Xtrain[x][2]==1 and Xtrain[x][4]==-1:
        att3Stats[0][0] += 1
    if Xtrain[x][2]==2 and Xtrain[x][4]==1:
        att3Stats[1][1] += 1
    elif Xtrain[x][2]==2 and Xtrain[x][4]==-1:
        att3Stats[1][0] += 1

for x in range(0, numOfRows):
    if Xtrain[x][3]==1 and Xtrain[x][4]==1:
        att4Stats[0][1] += 1
    elif Xtrain[x][3]==1 and Xtrain[x][4]==-1:
        att4Stats[0][0] += 1
    if Xtrain[x][3]==2 and Xtrain[x][4]==1:
        att4Stats[1][1] += 1
    elif Xtrain[x][3]==2 and Xtrain[x][4]==-1:
        att4Stats[1][0] += 1

for x in range(0, numOfRows):
    if Xtrain[x][4]==-1:
        label[0]+=1
    if Xtrain[x][4]==1:
        label[1]+=1

labelTotal = label[0] + label[1]

#UTILIZE LAPLACE METHOD TO INCREMENT 0 VALUES

for x in range(0,2):
    if(att1Stats[x][0]==0):
        att1Stats[x][0]+=1
    if (att1Stats[x][1] == 0):
        att1Stats[x][1] += 1

for x in range(0,2):
    if(att2Stats[x][0]==0):
        att2Stats[x][0]+=1
    if (att2Stats[x][1] == 0):
        att2Stats[x][1] += 1

for x in range(0,1):
    if(att3Stats[x][0]==0):
        att3Stats[x][0]+=1
    if (att3Stats[x][1] == 0):
        att3Stats[x][1] += 1

for x in range(0,1):
    if(att4Stats[x][0]==0):
        att4Stats[x][0]+=1
    if (att4Stats[x][1] == 0):
        att4Stats[x][1] += 1

def findTrueProb(passedList = []):
     firstProb = att1Stats[(passedList[0]-1)][1]/\
                 (att1Stats[(passedList[0]-1)][0] + att1Stats[(passedList[0]-1)][1])

     secondProb = att2Stats[(passedList[1]-1)][1]/\
                  (att2Stats[(passedList[1]-1)][0] + att2Stats[(passedList[1]-1)][1])

     thirdProb = att3Stats[(passedList[2] - 1)][1]/\
                 (att3Stats[(passedList[2] - 1)][0] + att3Stats[(passedList[2] - 1)][1])

     fourthProb = att4Stats[(passedList[3] - 1)][1]/\
                  (att4Stats[(passedList[3] - 1)][0] + att4Stats[(passedList[3] - 1)][1])

     return (firstProb * secondProb * thirdProb * fourthProb) * (label[1]/labelTotal)


def findFalseProb(passedList=[]):
    firstProb = att1Stats[(passedList[0] - 1)][0] /\
                 (att1Stats[(passedList[0] - 1)][0] + att1Stats[(passedList[0] - 1)][1])

    secondProb = att2Stats[(passedList[1] - 1)][0] /\
                 (att2Stats[(passedList[1] - 1)][0] + att2Stats[(passedList[1] - 1)][1])

    thirdProb = att3Stats[(passedList[2] - 1)][0] /\
                 (att3Stats[(passedList[2] - 1)][0] + att3Stats[(passedList[2] - 1)][1])

    fourthProb = att4Stats[(passedList[3] - 1)][0] /\
                 (att4Stats[(passedList[3] - 1)][0] + att4Stats[(passedList[3] - 1)][1])

    return (firstProb * secondProb * thirdProb * fourthProb) * (label[0] / labelTotal)

predictions = []

for x in range(0, numTestRows):
    thisFalse = findFalseProb(Xtest[x])
    thisTrue = findTrueProb(Xtest[x])
    if thisTrue>thisFalse:
        predictions.append(1)
    elif thisFalse>thisTrue:
        predictions.append(-1)

for x in range(0, len(predictions)):
    if predictions[x]==1 and Xtest[x][4]==1:
        tp+=1
    if predictions[x]==1 and Xtest[x][4]==-1:
        fp+=1
    if predictions[x]==-1 and Xtest[x][4]==1:
        fn+=1
    if predictions[x]==-1 and Xtest[x][4]==-1:
        tn+=1

print("True Pos:")
print(tp)
print("False Pos:")
print(fp)
print("True Neg:")
print(tn)
print("False Neg:")
print(fn)