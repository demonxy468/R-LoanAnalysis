# IST 718 - Final Project 
# Group 3 - Ha Nguyen - Jaydeep - Ye Xu 
# File: G3_loanAnalysis_ModelBuilding2.R
# Purpose: Predict Good/Bad loan from lendingclub data 

# Pre-processing data 
# Run the file at working directory 
source("G3_loanAnalysis_PreProcess.R")
# Output is transformed data loan3c_Reduced and loan3d_Reduced

################################# MODEL BUILDING ###########################################
############# METHOD 2: Use loan3c_Reduced as training, sample good/bad loan ratio as 50/50 ####
#########################################################################################################

loan3Good <- loan3c_Reduced[loan3c_Reduced$loan_status == 0,]  # No of row = 31369 
loan3Bad <- loan3c_Reduced[loan3c_Reduced$loan_status == 1,]    # No of row = 13025 

# Take a sample of 50% from the Good Loan
set.seed(1)
GoodSample <- sample(nrow(loan3Good), nrow(loan3Good)/2)
loan3GoodSample <- loan3Good[GoodSample,]                       # No of row = 15684 

# Merging GoodLoan Sample with Bad Loan so that the combined dataset has distribution good/bad ~ 50/50 
loan3Sample <- rbind(loan3GoodSample, loan3Bad)

# Use the combined dataset for training models and predict data year 2015 

##### LOGISTIC REGRESSION ############################################ 
glm3.2 <- glm(loan_status ~.-grade-pub_rec, data = loan3Sample, family = binomial)
summary(glm3.2)

#Predict data year 2015 
glm.predProb2 <- predict(glm3.2, loan3d_Reduced, type  = "response")
summary(glm.predProb2)
glm.pred2 = ifelse(glm.predProb2 > 0.5, 1, 0)
# Check how well the model predict 
table(glm.pred2, loan3d_Reduced$loan_status)   #Accuracy increased 

##### RANDOM FOREST ############################################
library(randomForest)
library(caret)
rf2.2 <- randomForest(loan_status ~.-grade-pub_rec, data = loan3Sample, mtry=5, ntree=400, na.action=na.roughfix)
rf2.pred2 <- predict(rf2.2, loan3d_Reduced)
confusionMatrix(rf2.pred2, loan3d_Reduced$loan_status)

##### NAIVE BAYES ############################################ 
library(e1071)
nb2 <- naiveBayes(loan_status ~.-grade-pub_rec,  data = loan3Sample)
nb2.pred <- predict(nb2, loan3d_Reduced)
confusionMatrix(nb2.pred, loan3d_Reduced$loan_status)

##### CALCULATE AUC  ############################################ 
glmROC2 <- roc(loan_status ~  as.numeric(glm.pred2), data = loan3d_Reduced, plot = T, col = "red", lty = 1)
nbROC2 <- roc(loan_status ~  as.numeric(nb2.pred), data = loan3d_Reduced, plot = T, add = T, col= "green", lty = 1)
rfROC2 <- roc(loan_status ~  as.numeric(rf2.pred2), data = loan3d_Reduced, plot = T, add = T, col = "blue", lty = 1)
legend(0.6,0.6, c('logistic regression','naive-bayes', 'random forest'),col=c('red','green', 'blue'),lwd=3, xjust = -0.5)

#AUC score 
cbind(glmAUC = glmROC2$auc, nbAUC = nbROC2$auc, rfAUC = rfROC2$auc)

###################################### Visulize predicting results of random forest #####################################
pre <- predict(rf2.2, loan3d_Reduced, type  = "prob")
prediction <- data.frame(list(pre))   #,row.names = NULL
colnames(prediction) <- c("Good.Loan.Rate","Bad.Loan.Rate")
prediction$loan_amnt <- loan3d_Reduced$loan_amnt
prediction$term <- substr(loan3d_Reduced$term, 0, 3)
prediction$home_ownership <- loan3d_Reduced$home_ownership
prediction$installment <- loan3d_Reduced$installment
prediction$annual_inc <- loan3d_Reduced$annual_inc
prediction$open_acc <- loan3d_Reduced$open_acc
prediction$total_acc <- loan3d_Reduced$total_acc

#=============== Visualize random forest prediction==============================
library(ggplot2)
#Visulization with loan amount and default probabilities, group by loan term
p1 <- ggplot(data=prediction, aes(x=loan_amnt, y=Bad.Loan.Rate))  
p1 + geom_point(position = "jitter",aes(colour = prediction$home_ownership))  + geom_smooth(method = loess,aes(colour = factor(prediction$home_ownership)))

#Visualize with account opened and default probabilities, group by home ownership
p1 <- ggplot(data=prediction, aes(x=total_acc, y=Bad.Loan.Rate))  
p1 + geom_bar( aes(fill=term),position="dodge",stat="identity") +facet_wrap("home_ownership") 


