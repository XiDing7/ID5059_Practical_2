library(gbm)

library(caret)

library(ROCR)

#Importing the data

train_df = read.csv("Train.csv", header=T)

test_df = read.csv("Test.csv", header=T)

dim(train_df)

dim(test_df)


#Print first 5 rows of the train dataset.

head(train_df,n = 5)

head(test_df,n = 5)


#train GBM model 

Delayed.boost2 = gbm(DelayTime ~ .-DelayTime -X -DepTime, data = train_df, distribution = "adaboost", n.trees = 300, shrinkage = 0.01, interaction.depth = 4, cv.folds = 3)

print(Delayed.boost2)

summary(Delayed.boost2)


#Tuning GBM Model 

ntree_opt_cv2 <- gbm.perf(Delayed.boost2, method = "cv")

ntree_opt_oob2 <- gbm.perf(Delayed.boost2, method = "OOB")

print(ntree_opt_cv2)

print(ntree_opt_oob2)

#ntree_opt_cv2 = 300 not enough


#Predictions

Delayed.pred2 <- predict(Delayed.boost2, newdata = test_df, n.trees = ntree_opt_cv2, type = 'response')


#Calculate optimal prediction probability cutoff 

optCutOff2 <- optimalCutoff(test_df$DelayTime, Delayed.pred2)[1] 

#optCutOff2 = 0.3465829

predictionBinaries2 <- as.factor(ifelse(Delayed.pred2 > optCutOff2, 1, 0))

test_df$DelayTime2 <- as.factor(test_df$DelayTime)


#Confusion Matrix

conf2 <- caret::confusionMatrix(predictionBinaries2, test_df$DelayTime)

as.matrix(conf2)

as.matrix(conf2, what = "classes")

#ROC Curve

GBM_pred_testing2 <- ROCR::prediction(Delayed.pred2, test_df$DelayTime)

GBM_ROC_testing2 <-ROCR::performance(GBM_pred_testing2, "tpr", "fpr")

plot(GBM_ROC_testing2)

plot(GBM_ROC_testing2, add = TRUE, col = "green")

legend("right", legend = c("GBM"), col = c("green"), lty = 1:2, cex = 0.6)


#AUC

auc.tmp2 <- ROCR::performance(GBM_pred_testing2, 'auc')

gbm_auc_testing2 <- as.numeric(auc.tmp2@y.values)

gbm_auc_testing2

#gbm_auc_testing2 = 0.7391512
