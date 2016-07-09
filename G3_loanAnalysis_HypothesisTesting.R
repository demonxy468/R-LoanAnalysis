# IST 718 - Final Project 
# Group 3 - Ha Nguyen - Jaydeep - Ye Xu 
# File: G3_loanAnalysis_HypothesisTesting.R
# Purpose: Predict Good/Bad loan from lendingclub data 

# Run the file at working directory 
source("G3_loanAnalysis_PreProcess.R")

# Annual income 
annualIncGoodloan<- loan3c_Reduced$annual_inc[loan3c_Reduced$loan_status == "0"]  # mean = 77884.23
annualIncBadloan <- loan3c_Reduced$annual_inc[loan3c_Reduced$loan_status == "1"]  # mean = 69529.4
t.test(annualIncGoodloan, annualIncBadloan, conf.level = 0.95)
t.test(annualIncGoodloan, mu = 77350, conf.level = 0.95, alternative = "greater")
t.test(annualIncBadloan, mu = 70150, conf.level = 0.95, alternative  = "less")

#t.test result:
#p-value = 0.009 < c.f = 0.05, reject null hypothesis, accpect:
#Difference in means is not equal to 0, they are different


# dti
dtiGoodloan<- loan3c_Reduced$dti[loan3c_Reduced$loan_status == "0"] # mean = 16.57652
dtiBadloan <- loan3c_Reduced$dti[loan3c_Reduced$loan_status == "1"]  # mean  = 19.35127
t.test(dtiGoodloan, dtiBadloan, conf.level = 0.95)
t.test(dtiGoodloan, mu = 16.65, conf.level = 0.95, alternative = "less")
t.test(dtiBadloan, mu = 19.22, conf.level = 0.95, alternative  = "greater")

#revol_util
ruGoodloan <- loan3c_Reduced$revol_util[loan3c_Reduced$loan_status == "0"]  #mean = 51.81
ruBadloan <- loan3c_Reduced$revol_util[loan3c_Reduced$loan_status == "1"]   #mean =  58.89
t.test(ruGoodloan, ruBadloan, conf.level = 0.95)
t.test(ruGoodloan, mu = 52.1, conf.level = 0.95, alternative = "less")
t.test(ruBadloan, mu = 58.5, conf.level = 0.95, alternative  = "greater")

