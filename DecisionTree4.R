#This code describes the process for building a classification decision tree which accurately
#predicts delays of 20 minutes or more

#setwd("C:/Users/p-mcl/OneDrive/Documents/Masters/ID5059/Practical2/")

#Load libraries
library(tidyverse)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(rpart)
library(car)
library(compare)
library(ModelMetrics)
library(caret)

#Read in data
trainData <- read_csv("NewData/Train.csv")
testData <- read_csv("NewData/Test.csv")

#Build tree with everything
tree1 <- rpart(DelayTime ~ .-X1, method = "class", data = trainData,
               control=rpart.control(minsplit=1, minbucket=1, cp=0.001))
summary(tree1)
fancyRpartPlot(tree1, cex = 0.5)

#Make predictions
preds1 <- as.data.frame(predict(tree1, newdata = testData, type = "class"))

#View the confusion matrix
confMat1 <- table(preds1$`predict(tree1, newdata = testData, type = "class")`, testData$DelayTime)
confusionMatrix(confMat1, positive = "1")

#Accuracy = 0.84
#sensitivity = 0.09
#specificity = 0.99
#We want sensitivity so this model is not good. There is clearly an issue with the dataset as there
#are many more 0s than 1s

#Create a balanced data set with equal 0s and 1s
sum(trainData$DelayTime == 1) / nrow(trainData) #17% of observations are delays
trainDataDelays <- trainData[trainData$DelayTime == 1, ]
trainDataOnTime <- trainData[trainData$DelayTime == 0, ]
trainDataOnTime <- sample_n(trainDataOnTime, nrow(trainDataDelays))
trainDataBal <- rbind(trainDataDelays, trainDataOnTime)

#We now have a dataset with equal 0s and 1s
#Build the same model on the balanced dataset
tree2 <- rpart(DelayTime ~ .-X1, method = "class", data = trainDataBal,
               control=rpart.control(minsplit=1, minbucket=1, cp=0.001))

#Make predictions
preds2 <- as.data.frame(predict(tree2, newdata = testData, type = "class"))

#View the confusion matrix
confMat2 <- table(preds2$`predict(tree2, newdata = testData, type = "class")`, testData$DelayTime)
confusionMatrix(confMat2, positive = "1")

#Accuracy = 0.67
#Sensitivity = 0.6867
#Specificity = 0.66
#This is a much better model as we are focusing on sensitivity

summary(tree2)
#Distance is not as important so what happens if we remove it?

tree3 <- rpart(DelayTime ~ .-X1-Distance, method = "class", data = trainDataBal,
                        control=rpart.control(minsplit=1, minbucket=1, cp=0.001))

#Make predictions
preds3 <- as.data.frame(predict(tree3, newdata = testData, type = "class"))

#View the confustion matrix
confMat3 <- table(preds3$`predict(tree3, newdata = testData, type = "class")`, testData$DelayTime)
confusionMatrix(confMat3, positive = "1")

#Accuracy = 0.6583
#Sensitivity = 0.6955
#Specificity = 0.6508
#This is an even better model as it has a higher sensitivity

summary(tree3)

#Let's see if we can prune the tree
plotcp(tree3)
#This plot suggests that we could increase the complexity parameter

tree4 <- prune(tree3, 
             cp= tree3$cptable[which.min(tree3$cptable[,"xerror"]),"CP"])

#Make predictions
preds4 <- as.data.frame(predict(tree4, newdata = testData, type = "class"))

#View the confustion matrix
confMat4 <- table(preds4$`predict(tree4, newdata = testData, type = "class")`, testData$DelayTime)
confusionMatrix(confMat4, positive = "1")

#Accuracy = 0.6583
#Sensitivity = 0.6955
#Specificity = 0.6508
#The results are the same

#Plot:
fancyRpartPlot(tree4, cex = 0.3)

#Final tree that will be used for boosting:
#Predicting delays using departure hour, unique carrier, destination, origin and month
