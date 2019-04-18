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

#Read in data
trainData <- read_csv("NewData/Train.csv")
testData <- read_csv("NewData/Test.csv")

#Build tree with everything
tree1 <- rpart(DelayTime ~ ., method = "class", data = trainData,
               control=rpart.control(minsplit=1, minbucket=1, cp=0.001))
summary(tree1)
fancyRpartPlot(tree1, cex = 0.5)

#That tree was too complex so try adjusting cp
tree2 <- rpart(DelayTime ~ ., method = "class", data = trainData,
               control=rpart.control(minsplit=1, minbucket=1, cp=0.002))
summary(tree2)
fancyRpartPlot(tree2, cex = 0.5)

#Trees are complicated with origin and dest so try without
#DepHour, UniqueCarrier and Distance were the only important ones
tree3 <- rpart(DelayTime ~ DepHour + UniqueCarrier + Distance, method = "class", data = trainData,
               control=rpart.control(minsplit=1, minbucket=1, cp=0.001))
summary(tree3)
fancyRpartPlot(tree3, cex = 0.5)

#This looks simple enough, check it's accuracy on the test data
preds3 <- as.data.frame(predict(tree3, newdata = testData, type = "class"))
acc3 <- preds3 == testData[, "DelayTime"]
sum(acc3 == 1) / nrow(acc3) #83.9% accurate

#Plot the complexity parameter
plotcp(tree3)

#Doesn't get any smaller after 0.0016 CP so try a model with that
tree4 <- rpart(DelayTime ~ DepHour + UniqueCarrier + Distance, method = "class", data = trainData,
               control=rpart.control(minsplit=1, minbucket=1, cp=0.0016))
plotcp(tree4)

#Check accuracy
preds4 <- as.data.frame(predict(tree4, newdata = testData, type = "class"))
acc4 <- preds4 == testData[, "DelayTime"]
sum(acc4 == 1) / nrow(acc4) #83.9% accurate, almost identical

#Try with CP of 0.0018 as per plotcp
tree5 <- rpart(DelayTime ~ DepHour + UniqueCarrier + Distance, method = "class", data = trainData,
               control=rpart.control(minsplit=1, minbucket=1, cp=0.0018))
plotcp(tree5)

#Check accuracy
preds5 <- as.data.frame(predict(tree5, newdata = testData, type = "class"))
acc5 <- preds5 == testData[, "DelayTime"]
sum(acc5 == 1) / nrow(acc5) #83.9% accurate, exactly the same

confMat5 <- table(preds5$`predict(tree5, newdata = testData, type = "class")`, testData$DelayTime)
confMat5

1053911/(1053911+200624) #84% correct 0 rate
12428/(12428+4064) #75.4% correct 1 rate

#Remove unique carrier
tree6 <- rpart(DelayTime ~ DepHour + Distance, method = "class", data = trainData,
               control=rpart.control(minsplit=1, minbucket=1, cp=0.0018))

#Check accuracy
preds6 <- as.data.frame(predict(tree6, newdata = testData, type = "class"))
acc6 <- preds6 == testData[, "DelayTime"]
sum(acc6 == 1) / nrow(acc6) #83.8%

fancyRpartPlot(tree6, cex = 0.45)

confMat6 <- table(preds6$`predict(tree6, newdata = testData, type = "class")`, testData$DelayTime)
confMat6

1053455/(1053455+201642) #83.9% correct 0 rate
11410/(11410+4520) #71.6% correct 1 rate

plotcp(tree6)

#Change CP to 0.0023
tree7 <- rpart(DelayTime ~ DepHour + Distance, method = "class", data = trainData,
               control=rpart.control(minsplit=1, minbucket=1, cp=0.0023))

#Check accuracy
preds7 <- as.data.frame(predict(tree7, newdata = testData, type = "class"))
acc7 <- preds7 == testData[, "DelayTime"]
sum(acc7 == 1) / nrow(acc7) #83.8%

fancyRpartPlot(tree7, cex = 0.45)

confMat7 <- table(preds7$`predict(tree7, newdata = testData, type = "class")`, testData$DelayTime)
confMat7

1053455/(1053455+201642) #83.9% correct 0 rate
11410/(11410+4520) #71.6% correct 1 rate

plotcp(tree7)

#Build model on balanced data set
sum(trainData$DelayTime == 1) / nrow(trainData) #17% of observations are delays
trainDataDelays <- trainData[trainData$DelayTime == 1, ]
trainDataOnTime <- trainData[trainData$DelayTime == 0, ]
trainDataOnTime <- sample_n(trainDataOnTime, nrow(trainDataDelays))
trainDataBal <- rbind(trainDataDelays, trainDataOnTime)

#Make tree with only dep hour and distance
tree8 <- rpart(DelayTime ~ DepHour + Distance, method = "class", data = trainDataBal)
               #control=rpart.control(minsplit=1, minbucket=1, cp=0.0023))

summary(tree8)
fancyRpartPlot(tree8)

#Tree is very simple, let's see its prediction accuracy
#Check accuracy
preds8 <- as.data.frame(predict(tree8, newdata = testData, type = "class"))
acc8 <- preds8 == testData[, "DelayTime"]
sum(acc8 == 1) / nrow(acc8) #51.7%

confMat8 <- table(preds8$`predict(tree8, newdata = testData, type = "class")`, testData$DelayTime)
confMat8

483034 / (483034+37777) #93% true negative
175275 / (175275+574941) #23% true positive

plotcp(tree8)

#Terrible model, so try to prune
prune8 <- rpart(DelayTime ~ DepHour + Distance, method = "class", data = trainDataBal,
                control=rpart.control(minsplit=1, minbucket=1, cp=0.061))

#Check accuracy
preds8_pruned <- as.data.frame(predict(prune8, newdata = testData, type = "class"))
acc8_pruned <- preds8_pruned == testData[, "DelayTime"]
sum(acc8_pruned == 1) / nrow(acc8_pruned) #51.8%

confMat8_p <- table(preds8_pruned$`predict(prune8, newdata = testData, type = "class")`, 
                    testData$DelayTime)
confMat8 #terrible again

#tree7 is the best tree as it only uses DepHour and Distance
fancyRpartPlot(tree7, cex = 0.5)


#Remove distance from the model
tree9 <- rpart(DelayTime ~ DepHour + UniqueCarrier + Origin + Dest, method = "class", data = trainData,
               control=rpart.control(minsplit=1, minbucket=1, cp=0.001))

#Check accuracy
preds9 <- as.data.frame(predict(tree9, newdata = testData, type = "class"))
acc9 <- preds9 == testData[, "DelayTime"]
sum(acc9 == 1) / nrow(acc9) #84.3%

confMat9 <- table(preds9$`predict(tree9, newdata = testData, type = "class")`, testData$DelayTime)
confMat9

1051238/(1051238+192293) #84.53653
20759/(20759+6737) #75.5%

fancyRpartPlot(tree9, cex = 0.5)
