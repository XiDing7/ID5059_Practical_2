#Assignement Three
DF <- read.csv("FullData", header = TRUE)
library("tidyverse")
DF <- DF%>%select(-X)

#Converting to factor
DF$Month <- as.factor(DF$Month)
DF$DayOfWeek <- as.factor(DF$DayOfWeek)
DF$DayofMonth<- as.factor(DF$DayofMonth)
DF$DepHour <- as.factor(DF$DepHour)
DF$DelayTime <- as.factor(DF$DelayTime)

#Dividing the data into train and test
set.seed(1714155)
samp <- sample(nrow(DF), 0.6 * nrow(DF))
train <- DF[samp, ]
test <- DF[-samp, ]

library(gbm)
library(caret)

#Gradient boosting 
#Model 1: with ntrees 200 
Delayed.Boost <-  gbm(DelayTime ~ DepHour + Origin + Dest +
                        Month + DayofMonth+ DayOfWeek, distribution = "adaboost", 
                      data = train,  n.trees = 200, interaction.depth = 5, 
                      shrinkage = 0.1)


#Model 2: ntrees 200, interaction depth 6 
Delayed.Boost.1 <- gbm(DelayTime ~ DepHour + Dest+ Origin +
                         Month + DayofMonth+ DayOfWeek, distribution = "adaboost", 
                       data = train, n.trees = 500, interaction.depth = 6)


#GBM with caret n trees 200, learning rate, 2 fold cv
truth <- test$DelayTime
fitControl <- trainControl(method = "repeatedcv", number = 2)


gbmFit <- train(DelayTime ~ Month+DayofMonth+DayOfWeek+UniqueCarrier+Distance 
                + DepHour, data = train, method = "gbm", trControl = fitControl,
                verbose = FALSE, tuneGrid = gbmGrid)
gbmFit
preds1 <- predict.train(gbmFit, newdata = test)
x <- table(truth,preds1)
x
caret::confusionMatrix(x)

#Model 3: With 3 fold, ntrees 200 
gbmFit1 <- train(DelayTime ~ Month+DayOfWeek+UniqueCarrier+Origin+Dest 
                 + DepHour, data = train, method = "gbm",tuneGrid = gbmGrid,
                 verbose = FALSE, trControl = fitControl)
preds2 <- predict.train(gbmFit1, newdata = test)
x <- table(truth,preds2)
x
caret::confusionMatrix(x)

#Without altering paramters 2 fold cv
gbmFit2 <- train(DelayTime ~ Month+DayOfWeek+UniqueCarrier+Origin +Dest 
                 + DepHour, data = train, method = "gbm", trControl = fitControl,
                 verbose = FALSE)
gbmFit2
preds3 <- predict.train(gbmFit2, newdata = test)
x <- table(truth,preds3)
x
caret::confusionMatrix(x)

#Changing the fitting paramaters
gbmFit3 <- train(DelayTime ~ Month+DayofMonth+DayOfWeek+UniqueCarrier+Distance 
                 + DepHour, data = train, method = "gbm",trControl = fitControl,
                 verbose = FALSE, tuneGrid = gbmGrid)
preds4 <- predict.train(gbmFit3, newdata = test)
x <- table(truth,preds4)
x
caret::confusionMatrix(x)

#Using the most interpretable model
gbmFit4 <- train(DelayTime ~ UniqueCarrier+Distance+DepHour, data = train,
                 method = "gbm",trControl = fitControl,
                 verbose = FALSE, tuneGrid = gbmGrid)
gbmFit4
preds5 <- predict.train(gbmFit4, newdata = test)
x <- table(truth,preds5)
x
caret::confusionMatrix(x)

#Gbm fit 5
gbmGrid <-expand.grid(interaction.depth = 8, 
                   n.trees = 400, 
                   shrinkage = 0.1, n.minobsinnode = 10)
gbmFit5 <- train(DelayTime ~ UniqueCarrier+Distance+DepHour, data = train,
                 method = "gbm",trControl = fitControl,
                 verbose = FALSE, tuneGrid = gbmGrid)
gbmFit5
preds6 <- predict.train(gbmFit5, newdata = test)
x <- table(truth,preds6)
x
caret::confusionMatrix(x)

#Traing Sample 
set.seed(9560)
down_train <- downSample(x = train[, -ncol(train)],
                         y = train$DelayTime)
gbmGrid <-  expand.grid(interaction.depth = 6, 
                        n.trees = 400, 
                        shrinkage = 0.1, n.minobsinnode = 10)
gbmFit1 <- train(Class ~ Month+DayofMonth+DayOfWeek+Origin + Dest+
                   DepHour, data = down_train, method = "gbm", 
                 trControl = fitControl, verbose = FALSE)
preds1<- predict.train(gbmFit1, newdata = test)
truth <- test$DelayTime
x <- table(truth,preds1)
caret::confusionMatrix(x)