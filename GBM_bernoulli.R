library(gbm)

library(caret)

library(ROCR)

library(InformationValue)

#Importing the data

train_df = read.csv("Train.csv", header=T)

test_df = read.csv("Test.csv", header=T)

dim(train_df)

dim(test_df)


#Print first 5 rows of the train dataset.

head(train_df,n = 5)

head(test_df,n = 5)

#train GBM model 

Delayed.boost1 = gbm(DelayTime ~ .-DelayTime -X -DepTime, data = train_df, distribution = "bernoulli", n.trees = 300, shrinkage = 0.01, cv.folds = 3, n.cores = 6)

print(Delayed.boost1)

summary(Delayed.boost1)


#Tuning GBM Model 

ntree_opt_cv1 <- gbm.perf(Delayed.boost1, method = "cv")

ntree_opt_oob1 <- gbm.perf(Delayed.boost1, method = "OOB")

print(ntree_opt_cv1)

print(ntree_opt_oob1)


#Calculate optimal prediction probability cutoff 

optCutOff1 <- optimalCutoff(test_df$DelayTime, Delayed.pred1)[1] 


#Predictions

Delayed.pred1 <- predict(Delayed.boost1, newdata = test_df, n.trees = ntree_opt_cv1, type = 'response')
                        
predictionBinaries1 <- as.factor(ifelse(Delayed.pred1 > 0.15, 1, 0))

test_df$DelayTime <- as.factor(test_df$DelayTime)


#Confusion Matrix

conf1 <- caret::confusionMatrix(predictionBinaries1, test_df$DelayTime, positive="1")

as.matrix(conf1, what = "overall")

as.matrix(conf1)

as.matrix(conf1, what = "classes")

conf1

#ROC Curve

GBM_pred_testing1 <- ROCR::prediction(Delayed.pred1, test_df$DelayTime)

GBM_ROC_testing1 <-ROCR::performance(GBM_pred_testing1, "tpr", "fpr")

plot(GBM_ROC_testing1)

plot(GBM_ROC_testing1, add = TRUE, col = "green")

legend("right", legend = c("GBM"), col = c("green"), lty = 1:2, cex = 0.6)


#AUC

auc.tmp1 <- ROCR::performance(GBM_pred_testing1, 'auc')

gbm_auc_testing1 <- as.numeric(auc.tmp1@y.values)

gbm_auc_testing1

as.matrix(conf1, what = "classes")
