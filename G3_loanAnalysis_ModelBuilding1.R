# IST 718 - Final Project 
# Group 3 - Ha Nguyen - Jaydeep - Ye Xu 
# File: G3_loanAnalysis_ModelBuilding1.R
# Purpose: Predict Good/Bad loan from lendingclub data 

# Pre-processing data 
# Run the file at working directory 
source("G3_loanAnalysis_PreProcess.R")
# Output is transformed data loan3c_Reduced and loan3d_Reduced

################################# MODEL BUILDING ###########################################
################## METHOD 1: use loan3c_Reduced as training, maintain good/bad loan ratio ######
#########################################################################################################

##### LOGISTIC REGRESSION ############################################ 
glm1 <- glm(loan_status ~., data = loan3c_Reduced, family = binomial)
summary(glm1)

# Remove insignificant features
glm2 <- glm(loan_status ~.-grade, data = loan3c_Reduced, family = binomial)
summary(glm2)
anova(glm1, glm2, test = "Chisq") # not significantly different

glm3 <- glm(loan_status ~.-grade-pub_rec, data = loan3c_Reduced, family = binomial)
summary(glm3)
anova(glm2, glm3, test = "Chisq")  #not significantly different 
### Use glm3 as final logistic model for prediction 

glm.predProb <- predict(glm3, loan3d_Reduced, type  = "response")
summary(glm.predProb)
# If prob > 0.5, consider as Bad loan
glm.pred = ifelse(glm.predProb > 0.5, 1, 0)
# Check how well the model predict 
table(glm.pred, loan3d_Reduced$loan_status)

##### RANDOM FOREST ############################################ 
library(randomForest)  #For model function 
library(caret) #For confusionMatrix function 

rf1 <- randomForest(loan_status ~.-grade-pub_rec, data = loan3c_Reduced, mtry=5, ntree=100, na.action=na.roughfix) 
# Conduct prediction
rf1.pred <- predict(rf1, loan3d_Reduced)
# Generate confusion Matrix and get the accuracy
confusionMatrix(rf1.pred, loan3d_Reduced$loan_status)

rf2 <- randomForest(loan_status ~.-grade-pub_rec, data = loan3c_Reduced, mtry=5, ntree=400, na.action=na.roughfix, nodesize = 1, importance = F, proximity = F)
rf2.pred <- predict(rf2, loan3d_Reduced)
confusionMatrix(rf2.pred, loan3d_Reduced$loan_status)

##### NAIVE BAYES ############################################ 
library(e1071)
nb1 <- naiveBayes(loan_status ~.-grade-pub_rec,  data = loan3c_Reduced)
nb1.pred <- predict(nb1, loan3d_Reduced)
confusionMatrix(nb1.pred, loan3d_Reduced$loan_status)


##### CALCULATE AUC  ############################################ 
library(pROC)
glmROC <- roc(loan_status ~  as.numeric(glm.pred), data = loan3d_Reduced, plot = T, col = "red", lty = 1)
nbROC <- roc(loan_status ~  as.numeric(nb1.pred), data = loan3d_Reduced, plot = T, add = T, col= "green", lty = 1)
rfROC <- roc(loan_status ~  as.numeric(rf2.pred), data = loan3d_Reduced, plot = T, add = T, col = "blue", lty = 1)
legend(0.6,0.6, c('logistic regression','naive-bayes', 'random forest'),col=c('red','green', 'blue'),lwd=3, xjust = -0.5)

#AUC score 
cbind(glmAUC = glmROC$auc, nbAUC = nbROC$auc, rfAUC = rfROC$auc)


