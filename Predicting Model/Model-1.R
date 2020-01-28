## Preparing data for predicting churn model
## Please put this file with the dataset (in the same folder) and set it as a working directory 
#initial stuff - loading required packages 

library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly)
library(scales)
library(corrplot)
library(caret)
library(car)
library(MASS)
library(pROC)
library(InformationValue)

## reading data
TelcoChurn_ADS <- read.csv("raw_data.csv")

##fixing missing values
TelcoChurn_ADS$TotalCharges[is.na(TelcoChurn_ADS$TotalCharges)] <- TelcoChurn_ADS$MonthlyCharges[is.na(TelcoChurn_ADS$TotalCharges)]

## factoring all cat variables 
TelcoChurn_ADS <- TelcoChurn_ADS %>%
  mutate(Partner = ifelse(Partner == "Yes", 1, 0)) 

TelcoChurn_ADS <- TelcoChurn_ADS %>%
  mutate(Dependents = ifelse(Dependents == "Yes", 1, 0)) 

TelcoChurn_ADS <- TelcoChurn_ADS %>%
  mutate(PhoneService = ifelse(PhoneService == "Yes", 1, 0)) 

TelcoChurn_ADS <- TelcoChurn_ADS %>%
  mutate(MultipleLines = ifelse(MultipleLines == "Yes", 1, 0)) 

TelcoChurn_ADS <- TelcoChurn_ADS %>%
  mutate(OnlineSecurity = ifelse(OnlineSecurity == "Yes", 1, 0)) 

TelcoChurn_ADS <- TelcoChurn_ADS %>%
  mutate(DeviceProtection = ifelse(DeviceProtection == "Yes", 1, 0)) 

TelcoChurn_ADS <- TelcoChurn_ADS %>%
  mutate(TechSupport = ifelse(TechSupport == "Yes", 1, 0)) 

TelcoChurn_ADS <- TelcoChurn_ADS %>%
  mutate(StreamingTV = ifelse(StreamingTV == "Yes", 1, 0)) 

TelcoChurn_ADS <- TelcoChurn_ADS %>%
  mutate(StreamingMovies = ifelse(StreamingMovies == "Yes", 1, 0)) 

TelcoChurn_ADS <- TelcoChurn_ADS %>%
  mutate(PaperlessBilling = ifelse(PaperlessBilling == "Yes", 1, 0)) 

TelcoChurn_ADS <- TelcoChurn_ADS %>%
  mutate(OnlineBackup = ifelse(OnlineBackup == "Yes", 1, 0))

## Dropping variables which were deemed not usefull from our in depth exploratory analysis
TelcoChurn_ADS$gender <- NULL
TelcoChurn_ADS$MultipleLines <- NULL
TelcoChurn_ADS$StreamingMovies <- NULL
TelcoChurn_ADS$StreamingTV <- NULL


## dataprep for model is done - now time to split the dataset into train and test
set.seed(1234)
trainIndex <- createDataPartition(TelcoChurn_ADS$Churn,
                                  p = .70, 
                                  list = FALSE, 
                                  times = 1)

TelcoChurn_train <- TelcoChurn_ADS[ trainIndex,]
TelcoChurn_test  <- TelcoChurn_ADS[-trainIndex,]

##changing Churn Yes to 1 in test data - will help us later in estimating accuracy of model
TelcoChurn_test <- TelcoChurn_test %>%
  mutate(Churn = ifelse(Churn == "Yes", 1, 0)) 


##base model - not including monthly charges but tenure and total charges
logitMod <- glm(Churn ~ TotalCharges
                +tenure
                +SeniorCitizen
                +Partner
                +Dependents
                +PhoneService
                +as.factor(InternetService)
                +OnlineSecurity
                +OnlineBackup
                +TechSupport
                +DeviceProtection
                +as.factor(Contract)
                +PaperlessBilling
                +as.factor(PaymentMethod)
                , data=TelcoChurn_train, family = binomial('logit'))
summary(logitMod)

### Simple stepwise logistic regression

step.model <- stepAIC(logitMod, direction = "both", 
                      trace = FALSE)
summary(step.model)


predicted_step<-predict(step.model, TelcoChurn_test, type="response")
optCutOff_step <- optimalCutoff(TelcoChurn_test$Churn, predicted_step)[1] 

## Lets put the optimal cutoff and put it as threshold 

TelcoChurn_test$pred_step=predicted_step
TelcoChurn_test <- TelcoChurn_test  %>% mutate(model_pred = 1*(pred_step > .55) + 0)
TelcoChurn_test <- TelcoChurn_test %>% mutate(accurate = 1*(model_pred == Churn))

sum(TelcoChurn_test$accurate)/nrow(TelcoChurn_test)

## Lets try building with manual intuitive variable selection

##dropping Partners from logitMod as it has high p value
logitMod2 <- glm(Churn ~ TotalCharges
                +tenure
                +SeniorCitizen
                +Dependents
                +PhoneService
                +as.factor(InternetService)
                +OnlineSecurity
                +OnlineBackup
                +TechSupport
                +DeviceProtection
                +as.factor(Contract)
                +PaperlessBilling
                +as.factor(PaymentMethod)
                , data=TelcoChurn_train, family = binomial('logit'))
summary(logitMod2)

##dropping device protection and dependants 
logitMod3 <- glm(Churn ~ TotalCharges
                 +tenure
                 +SeniorCitizen
                 +PhoneService
                 +as.factor(InternetService)
                 +OnlineSecurity
                 +OnlineBackup
                 +TechSupport
                 +as.factor(Contract)
                 +PaperlessBilling
                 +as.factor(PaymentMethod)
                 , data=TelcoChurn_train, family = binomial('logit'))
summary(logitMod3)

##seems like a good model - lets check the VIF - all variables have less than 5 VIF so good to go
vif(logitMod3)


### lets bring in Monthly charges instead of Total and tenure - model3 had lesser number of factors 
## lets see if we can reduce AIC 

logitMod4 <- glm(Churn ~ MonthlyCharges
                 +SeniorCitizen
                 +PhoneService
                 +as.factor(InternetService)
                 +OnlineSecurity
                 +OnlineBackup
                 +TechSupport
                 +as.factor(Contract)
                 +PaperlessBilling
                 +as.factor(PaymentMethod)
                 , data=TelcoChurn_train, family = binomial('logit'))
summary(logitMod4)

##Removing senior citizen flag

logitMod5 <- glm(Churn ~ MonthlyCharges
                 +PhoneService
                 +as.factor(InternetService)
                 +OnlineSecurity
                 +OnlineBackup
                 +TechSupport
                 +as.factor(Contract)
                 +PaperlessBilling
                 +as.factor(PaymentMethod)
                 , data=TelcoChurn_train, family = binomial('logit'))
summary(logitMod5)

## going back to Total Charges to see if we can get a lower AIC

logitMod6 <- glm(Churn ~ TotalCharges
                 +tenure
                 +PhoneService
                 +as.factor(InternetService)
                 +OnlineSecurity
                 +OnlineBackup
                 +TechSupport
                 +as.factor(Contract)
                 +PaperlessBilling
                 +as.factor(PaymentMethod)
                 , data=TelcoChurn_train, family = binomial('logit'))
summary(logitMod6)


## AIC comparable with "Monthly Charges" model with better/more intuitive set of variables

vif(logitMod6)

##All variables have less than 5 VIF

### checking interactions between contract type and internet - two of the most significant categorical variables

logitMod7_interact <- glm(Churn ~ TotalCharges
                 +tenure
                 +PhoneService
                 +as.factor(InternetService)
                 +OnlineSecurity
                 +OnlineBackup
                 +TechSupport
                 +as.factor(Contract)
                 +PaperlessBilling
                 +as.factor(PaymentMethod)
                 +as.factor(InternetService)*as.factor(Contract)
                 , data=TelcoChurn_train, family = binomial('logit'))
summary(logitMod7_interact)

### the interaction variables are not significant in terms of pvalue
## checking total_charges against Internet service 

logitMod8_interact <- glm(Churn ~ TotalCharges
                          +tenure
                          +PhoneService
                          +as.factor(InternetService)
                          +OnlineSecurity
                          +TechSupport
                          +as.factor(Contract)
                          +PaperlessBilling
                          +as.factor(PaymentMethod)
                          +TotalCharges*as.factor(InternetService)
                          , data=TelcoChurn_train, family = binomial('logit'))

summary(logitMod8_interact)


##again, interacted variables aren't significant 

## lets see if tenure and type of internet service opted for interplay or not

logitMod9_interact <- glm(Churn ~ TotalCharges
                          +tenure
                          +PhoneService
                          +as.factor(InternetService)
                          +OnlineSecurity
                          +TechSupport
                          +as.factor(Contract)
                          +PaperlessBilling
                          +as.factor(PaymentMethod)
                          +tenure*as.factor(InternetService)
                          , data=TelcoChurn_train, family = binomial('logit'))

summary(logitMod9_interact)

### again the variables are not significant - going back to model6

## Model diagnostics - model6

prob <- predict(logitMod6, newdata=TelcoChurn_test, type="response")
#oprimal cut off

optCutOff <- optimalCutoff(TelcoChurn_test$Churn, prob)[1] 

## Lets put the optimal cutoff and put it as threshold 

TelcoChurn_test$pred=prob
TelcoChurn_test <- TelcoChurn_test  %>% mutate(model_pred = 1*(pred > optCutOff) + 0)
TelcoChurn_test <- TelcoChurn_test %>% mutate(accurate = 1*(model_pred == Churn))
sum(TelcoChurn_test$accurate)/nrow(TelcoChurn_test)


##lets stick with this model as it has better set of pvalues than stepwise and slightly more accurate
## lets check sensitivity VS specificity

pred <- factor(ifelse(prob >= optCutOff, "Yes", "No"))
actual <- factor(ifelse(TelcoChurn_test$Churn==1,"Yes","No"))
conf_matrix_logmod6 <- table(actual,pred)
## sensitivity is low compared to specificity

 
## reducing threshold to improve sensitivity 

pred <- factor(ifelse(prob >= 0.35, "Yes", "No"))
actual <- factor(ifelse(TelcoChurn_test$Churn==1,"Yes","No"))
conf_matrix_logmod6 <- table(actual,pred)
## similar mix between sensitivity and specificity

## lets re run accuracy for this combination 
TelcoChurn_test$pred_step=prob
TelcoChurn_test <- TelcoChurn_test  %>% mutate(model_pred = 1*(pred_step > .35) + 0)
TelcoChurn_test <- TelcoChurn_test %>% mutate(accurate = 1*(model_pred == Churn))

sum(TelcoChurn_test$accurate)/nrow(TelcoChurn_test)
## Accuracy of ~80%


# Compute AUC for predicting Class with the model
library(pROC)
roccurve <- roc(TelcoChurn_test$Churn ~ prob)
plot(roccurve)
auc(roccurve)
## We get AUC ROC as 0.86
