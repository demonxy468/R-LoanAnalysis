# IST 718 - Final Project 
# Group 3 - Ha Nguyen - Jaydeep - Ye Xu 
# File: G3_loanAnalysis_PreProcess.R
# Purpose: Predict Good/Bad loan from lendingclub data 

###################################### LOAD DATA ##########################################
# Data 2013 - 2014 for training models 
download.file("https://resources.lendingclub.com/LoanStats3c.csv.zip", "Loan3c.zip", method = "curl")
unzip(zipfile = "Loan3c.zip", files = "LoanStats3c.csv")
loan3c<- read.csv("LoanStats3c.csv", skip = 1, stringsAsFactors = F, nrows = 235629)

# Data 2015 - 30/6/2015 for testing models 
download.file("https://resources.lendingclub.com/LoanStats3d.csv.zip", "Loan3d.zip", method = "curl")
unzip(zipfile = "Loan3d.zip", files = "LoanStats3d.csv")
loan3d<- read.csv("LoanStats3d.csv", skip = 1, stringsAsFactors = F, nrows = 180102)

################################ DATA TRANSFORMATION ############################################

# Data Transformation 
loan_transformation <- function(loan3) {
        # Clean revol_util, int_rate 
        loan3$revol_util <- gsub("%", "", loan3$revol_util)
        loan3$revol_util <- as.numeric(loan3$revol_util)
        loan3$int_rate <- as.numeric(gsub("%", "", loan3$int_rate))
        
        # Choosing these loan status for analysis 
        goodBadIndicators <- c("Fully Paid", "Charged Off", "Default", "Late (16-30 days)", "Late (31-120 days)", "In Grace Period" )
        loan3 <- loan3[loan3$loan_status %in% goodBadIndicators,]
        # Fully Paid status as Good (coded 0) / Other statuses as bad (coded 1)
        loan3$loan_status <- ifelse(loan3$loan_status == "Fully Paid", "0", "1")
        loan3$loan_status <- factor(loan3$loan_status, levels = c(0,1))

        # Remove outlier values 
        loan3 <- loan3[loan3$emp_length !="n/a" & !loan3$home_ownership %in% c("ANY", "NONE", "OTHER"),]
        # Features selection 
        numerical_cols <- c("loan_amnt", "annual_inc", "dti", "delinq_2yrs", "inq_last_6mths", "open_acc", "pub_rec", "revol_bal", "revol_util", "total_acc")
        categorical_cols <- c("term", "grade", "sub_grade", "emp_length", "home_ownership", "verification_status", "purpose")
        label <-  c("loan_status")
        loan3 <- loan3[,names(loan3) %in% c(categorical_cols,numerical_cols, label)]
        
        # Data type transformation 
        loan3$grade <- as.factor(loan3$grade)
        loan3$sub_grade <- as.factor(loan3$sub_grade)
        loan3$term <- as.factor(loan3$term)
        loan3$emp_length <- as.factor(loan3$emp_length)
        loan3$home_ownership <- as.factor(loan3$home_ownership)
        loan3$verification_status <- as.factor(loan3$verification_status)
        loan3$purpose <- as.factor(loan3$purpose)
        return(loan3)
} 

### Pre-process train data
loan3c_Reduced <- loan_transformation(loan3c)
### Pre-process test data
loan3d_Reduced <- loan_transformation(loan3d)

################################ EXPLANATORY DATA ANALYSIS ############################################
summary(loan3c_Reduced)

# Corplot 
loan3c_numCol <- loan3c[c("loan_amnt", "installment", "annual_inc", "dti", "delinq_2yrs","inq_last_6mths", "mths_since_last_delinq", "open_acc", "pub_rec", "revol_bal","total_acc")]
library(corrplot)
corrplot(cor(loan3c_numCol, use = "pairwise.complete.obs"), method = "number")

#Scatter Plot 
qplot(x = annual_inc, y = dti, geom = "point", data = loan3c_Reduced, color = loan_status)

#  Borrower's employment title 
empTitleGoodloan<- loan3c$emp_title[loan3c$loan_status == "Fully Paid"]
empTitleBadloan <- loan3c$emp_title[loan3c$loan_status %in% c("Charged Off", "Default", "Late (31-120 days)", "In Grace Period" , "Late (16-30 days)")]

empTitleGoodloandf <- as.data.frame(table(tolower(empTitleGoodloan)))
empTitleBadloandf <- as.data.frame(table(tolower(empTitleBadloan)))
library(RColorBrewer)
pal <- brewer.pal(12,"Paired")
# Wordcloud for good loan / bad loan 
library(wordcloud)
wordcloud(empTitleGoodloandf$Var1, empTitleGoodloandf$Freq, max.words = 100, scale = c(5,0.5), colors = pal)
wordcloud(empTitleBadloandf$Var1, empTitleBadloandf$Freq, max.words = 100, scale = c(5,0.5), colors = pal)
