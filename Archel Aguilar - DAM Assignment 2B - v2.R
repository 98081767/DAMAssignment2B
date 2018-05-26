#-----------------------------------------
#Archel Aguilar (980817867)
# DAM - Assignment 2B 
#
# Classification modelling
# 
# Background: 
#   - Assist car manufacturer by helping them target existing customers for a re-purchase campaign
#   - Objective is to send communication to customers who are highly likely to purchase a new vehicle
#   - All customers have already purchased at least one vehicle
#
# Data:
#   - Customer demographics, previous car type bought, age of vehicle, servicing details
#   - Servicing details are only for mechanics at official dealerships.
#   - There is a lot of noise in the numeric variables so this has been transformed into deciles (1 to 10)
#   - The deciles can be treated as numeric or factors
# 
# Variables
#   - ID,                         Unique ID, Unique ID of the customer
#   - Target,                     Integer, Model target. 1 if the customer has purchased more than 1 vehicle, 0 if they have only purchased 1.
#                                 (Model to predict 1-repurchase or 0-not repurchase)
#  -  age_band,                   Categorical, Age banded into categories
#  -  gender,                     Categorical, Male, Female or Missing
#  -  car_model,                  Categorical, The model of vehicle, 18 models in total
#  -  car_segment,                Categorical, The type of vehicle
#  -  age_of_vehicle_years,       Integer, Age of their last vehicle, in deciles
#  -  sched_serv_warr,            Integer, Number of scheduled services (e.g. regular check-ups) used under warranty, in deciles
#  -  non_sched_serv_warr,        Integer, Number of non-scheduled services (e.g. something broke out of the service cycle)used under warranty, in deciles
#  -  sched_serv_paid,            Integer, Amount paid for scheduled services, in deciles
#  -  non_sched_serv_paid,        Integer, Amount paid for non scheduled services, in deciles
#  -  total_paid_services,        Integer, Amount paid in total for services, in deciles
#  -  total_services,             Integer, Total number of services, in deciles
#  -  mth_since_last_serv,        Integer, The number of months since the last service, in deciles
#  -  annualised_mileage,         Integer, Annualised vehicle mileage, in deciles
#  -  num_dealers_visited,        Integer, Number of different dealers visited for servicing, in deciles
#  -  num_serv_dealer_purchased,  Integer, Number of services had at the same dealer where the vehicle was purchased, in deciles

#
#-----------------------------------------

#clear variables
rm(list=ls())

install.packages("ISLR")
library(ISLR)

setwd("C:/Users/arche/Documents/UTS/R-References/R-references-Git/DAM-Assignment-2B")
#setwd("C:/Personal/UTS/R-References/R-references-Git/DAMAssignment2B")

getwd()

cpur = read.csv("repurchase_training.csv")
str(cpur)

cpur$Target                     = as.factor(cpur$Target)
cpur$age_of_vehicle_years       = as.factor(cpur$age_of_vehicle_years)
cpur$sched_serv_warr            = as.factor(cpur$sched_serv_warr )         
cpur$non_sched_serv_warr        = as.factor(cpur$non_sched_serv_warr)
cpur$sched_serv_paid            = as.factor(cpur$sched_serv_paid)
cpur$non_sched_serv_paid        = as.factor(cpur$non_sched_serv_paid)
cpur$total_paid_services        = as.factor(cpur$total_paid_services)
cpur$total_services             = as.factor(cpur$total_services)
cpur$mth_since_last_serv        = as.factor(cpur$mth_since_last_serv)
cpur$annualised_mileage         = as.factor(cpur$annualised_mileage)
cpur$num_dealers_visited        = as.factor(cpur$num_dealers_visited)
cpur$num_serv_dealer_purchased  = as.factor(cpur$num_serv_dealer_purchased)

summary(cpur)

str(cpur)

contrasts(cpur$age_band)
contrasts(cpur$gender)
contrasts(cpur$car_segment)


#---------------------------------------------
# Create train and test data
#---------------------------------------------

install.packages('caret', dependencies = TRUE)
library(caret)

set.seed(42)

#splits sample group maintaining the ratio of the target
train = createDataPartition(y = cpur$Target, p = 0.7, list = F)


# partition purchase data into two sets 
training = cpur[train, ]
resetTraining = training
testing = cpur[-train, ]
resetTesting = testing

str(training)
str(testing)

#---------------------------------------------
# Analyse data
#---------------------------------------------

nrow(cpur)     #131,337
nrow(training)  #91,937
nrow(testing)   #39,400

nrow(cpur[cpur$Target=="1",]) #3521 - number of targets
nrow(cpur[cpur$Target=="0",]) #127816 - number of non targets
#proportion of targets in data = 0.0268

nrow(training[training$Target=="1",]) #2465 - targets in trainng
nrow(training[training$Target=="0",]) #89472 - non targets in training
#proportion of targets in data = 0.0268

install.packages("ggplot2")
library(ggplot2)
library(scales)

#frequency charts
ggplot(data.frame(cpur$Target), aes(x=cpur$Target)) + geom_bar() + xlab("Customer Repurchased a Vechicle (1=Yes, 0=No)") + scale_y_continuous(labels = comma)
ggplot(data.frame(cpur$age_band), aes(x=cpur$age_band)) + geom_bar() + xlab("Age Band")
ggplot(data.frame(cpur$gender), aes(x=cpur$gender)) + geom_bar() + xlab("Gender")
ggplot(data.frame(cpur$car_model), aes(x=cpur$car_model)) + geom_bar() + xlab("Car Model")
ggplot(data.frame(cpur$car_segment), aes(x=cpur$car_segment)) + geom_bar() + xlab("Car Type")
ggplot(data.frame(cpur$age_of_vehicle_years), aes(x=cpur$age_of_vehicle_years)) + geom_bar() + xlab("Vehicle Age (Deciles)")
ggplot(data.frame(cpur$sched_serv_warr), aes(x=cpur$sched_serv_warr)) + geom_bar() + xlab("# Scheduled services (Deciles)")
ggplot(data.frame(cpur$non_sched_serv_warr), aes(x=cpur$non_sched_serv_warr)) + geom_bar() + xlab("# Non-Scheduled services (Deciles)")
ggplot(data.frame(cpur$sched_serv_paid), aes(x=cpur$sched_serv_paid)) + geom_bar() + xlab("Amount paid for scheduled services (Deciles)")
ggplot(data.frame(cpur$non_sched_serv_paid), aes(x=cpur$non_sched_serv_paid)) + geom_bar() + xlab("Amount paid for non scheduled services (Deciles)")
ggplot(data.frame(cpur$total_paid_services), aes(x=cpur$total_paid_services)) + geom_bar() + xlab("Amount paid for all services (Deciles)")
ggplot(data.frame(cpur$total_services), aes(x=cpur$total_services)) + geom_bar() + xlab("Total # services (Deciles)")
ggplot(data.frame(cpur$mth_since_last_serv), aes(x=cpur$mth_since_last_serv)) + geom_bar() + xlab("Months since last service (Deciles)")
ggplot(data.frame(cpur$annualised_mileage), aes(x=cpur$annualised_mileage)) + geom_bar() + xlab("Months since last service (Deciles)")
ggplot(data.frame(cpur$num_dealers_visited), aes(x=cpur$num_dealers_visited)) + geom_bar() + xlab("Number of dealers visited for servicing (Deciles)")
ggplot(data.frame(cpur$num_serv_dealer_purchased), aes(x=cpur$num_serv_dealer_purchased)) + geom_bar() + xlab("Number of services at purchased dealer (Deciles)")



#---------------------------
# Create models
#---------------------------

#Precision:Positive Pred Value
#Recall:Sensitivtiy

#include all except for identifier (ID)
glmodel = "Target ~. -ID" #all variables (AIC: 9078, F1: 0.660561, sensitivity/recall: 0.54640, precision/pos pred value: 0.83502)

#glmodel = "Target ~ -ID + gender + car_model + age_of_vehicle_years + sched_serv_warr + non_sched_serv_warr + sched_serv_paid + non_sched_serv_paid + total_services + mth_since_last_serv + annualised_mileage + num_dealers_visited + num_serv_dealer_purchased"
#(AIC: 9118, F1: 0.6613181, sensitivity: 0.54640)
#glmodel = "Target ~ gender - ID" (AIC: 22569, F1: NA, sensitivity: 0.0000)

cpur.glm = glm(formula = glmodel,
             data = training,
             family = "binomial")
summary(cpur.glm)
#AIC: 9078


###########################
# Create probabilities and predictions
###########################

# add the probabilities to the testing data
testing$probability = predict(cpur.glm, newdata = testing, type = "response")

# assume that the optimum probability threshold is 0.5
# Create the class prediction - our target is 1
testing$prediction = "0"
testing[testing$probability >= 0.5, "prediction"] = "1"


###########################
# Evaluation using confusion matrix
###########################

#set Target=1 as the focus for confusion matrix
cm = confusionMatrix(data = as.factor(testing$prediction), testing$Target, positive="1")
#get F1 score
cm$byClass["F1"] #0.6553341

#summary
cm
#               Reference
# Prediction      0     1
#           0 38253   497
#           1    91   559
# 
# Accuracy : 0.9851          
# 95% CI : (0.9838, 0.9863)
# No Information Rate : 0.9732          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.6481          
# Mcnemar's Test P-Value : < 2.2e-16       
# 
# Sensitivity : 0.52936         
# Specificity : 0.99763         
# Pos Pred Value : 0.86000         
# Neg Pred Value : 0.98717         
# Prevalence : 0.02680         
# Detection Rate : 0.01419         
# Detection Prevalence : 0.01650         
# Balanced Accuracy : 0.76349         
# 
# 'Positive' Class : 1          

#-------------------------------------------
# Lasso & Ridge check
#--------------------------------------------

install.packages("glmnet")
library(glmnet)

###########################
# Lasso Regression (F1 - 0.6524002, sensitivity/recall: 0.53409, precision/pos pred value: 0.83804)
###########################

#reset variables
training = resetTraining
#remove ID and Target from model
x = model.matrix(~ ., training[, c(-1,-2)])
y = training$Target

testing = resetTesting
#remove ID and Target from model
z = model.matrix(~ ., testing[, c(-1,-2)])
a = testing$Target

set.seed(42)

# alpha = 1 specifies lasso regression
cv.fit_lasso = cv.glmnet(x, y, family = 'binomial', alpha = 1)
#lambda is the strength of the penalty on the coefficients
#as lambda gets larger, ths bias is unchanged but the variance drops


# Results
plot(cv.fit_lasso)
cv.fit_lasso$lambda.min #error measure. can choose either
cv.fit_lasso$lambda.1se #error measure. can choose either

cv.lasso_coef = coef(cv.fit_lasso, s = cv.fit_lasso$lambda.min)
cv.lasso_coef
#co-efficients with the largest +ve result provide the biggest change to the baseline factor


prediction_lasso = predict(cv.fit_lasso$glmnet.fit, newx =z, type = "class", s = cv.fit_lasso$lambda.min)

lasso_confusion = confusionMatrix(data = as.factor(prediction_lasso), a, positive="1")
lasso_confusion
#                 Reference
# Prediction      0     1
#           0 38235   492
#           1   109   564
# 
# Accuracy : 0.9847          
# 95% CI : (0.9835, 0.9859)
# No Information Rate : 0.9732          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.645           
# Mcnemar's Test P-Value : < 2.2e-16       
# 
# Sensitivity : 0.53409         
# Specificity : 0.99716         
# Pos Pred Value : 0.83804         
# Neg Pred Value : 0.98730         
# Prevalence : 0.02680         
# Detection Rate : 0.01431         
# Detection Prevalence : 0.01708         
# Balanced Accuracy : 0.76562         
# 
# 'Positive' Class : 1          

lasso_confusion$byClass["F1"] #0.6524002

###########################
# Ridge Regression (F1: 0.4292893, sensitivity/recall: 0.283144, precision/pos pred value: 0.887240)
###########################

#reset variables
training = resetTraining
#remove ID and Target from model
x = model.matrix(~ ., training[, c(-1,-2)])
y = training$Target

testing = resetTesting
#remove ID and Target from model
z = model.matrix(~ ., testing[, c(-1,-2)])
a = testing$Target

set.seed(42)

# alpha = 0 specifies ridge regression
cv.fit_ridge = cv.glmnet(x, y, family = 'binomial', alpha = 0)

# Results
plot(cv.fit_ridge)
cv.fit_ridge$lambda.min
cv.fit_ridge$lambda.1se

#shows the coefficents
coef(cv.fit_ridge, s = cv.fit_ridge$lambda.min)

prediction_ridge = predict(cv.fit_ridge$glmnet.fit, newx=z, type="class", s = cv.fit_ridge$lambda.min)


#set Target=1 as the focus for confusion matrix
ridge_confusion = confusionMatrix(data = as.factor(prediction_ridge), testing$Target, positive="1")
ridge_confusion
#               Reference
# Prediction        0     1
#             0 38306   757
#             1    38   299
# 
# Accuracy : 0.9798          
# 95% CI : (0.9784, 0.9812)
# No Information Rate : 0.9732          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.4218          
# Mcnemar's Test P-Value : < 2.2e-16       
# 
# Sensitivity : 0.283144        
# Specificity : 0.999009        
# Pos Pred Value : 0.887240        
# Neg Pred Value : 0.980621        
# Prevalence : 0.026802        
# Detection Rate : 0.007589        
# Detection Prevalence : 0.008553        
# Balanced Accuracy : 0.641076        
# 
# 'Positive' Class : 1  

ridge_confusion$byClass["F1"] #0.4292893 

#-------------------------------------------
# Tree classification
#--------------------------------------------

install.packages("rpart", dependencies = TRUE)
install.packages("rpart.plot", dependencies = TRUE)
install.packages("mlbench")

library(rpart)
library(rpart.plot)
library(mlbench)

set.seed(42)

#reset variables
training = resetTraining
#remove ID and Target from model
x = model.matrix(~ ., training[, c(-1,-2)])
y = training$Target

testing = resetTesting
#remove ID and Target from model
z = model.matrix(~ ., testing[, c(-1,-2)])
a = testing$Target

###########################
# Decision Tree (F1: 0.5927273, sensitivity/recall: 0.46307, precision/pos pred value: 0.82323)
###########################

#build model
rpart_model = rpart(Target ~.-ID,data = training, method="class") #use method ="anova" for regression problems

#plot tree
prp(rpart_model)


#prediction
rpart_predict = predict(rpart_model,testing,type="class")

rpart_confusion = confusionMatrix(data = as.factor(rpart_predict), testing$Target, positive="1")
rpart_confusion

rpart_confusion$byClass["F1"] #0.5927273

#                 Reference
# Prediction      0     1
#           0 38239   567
#           1   105   489
# 
# Accuracy : 0.9829          
# 95% CI : (0.9816, 0.9842)
# No Information Rate : 0.9732          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.5847          
# Mcnemar's Test P-Value : < 2.2e-16       
# 
# Sensitivity : 0.46307         
# Specificity : 0.99726         
# Pos Pred Value : 0.82323         
# Neg Pred Value : 0.98539         
# Prevalence : 0.02680         
# Detection Rate : 0.01241         
# Detection Prevalence : 0.01508         
# Balanced Accuracy : 0.73016         
# 
# 'Positive' Class : 1         


###########################
# Random Forests (F1: 0.8247978, sensitivity/recall: 0.72443, precision/pos pred value: 0.95745)
###########################

install.packages("randomForest")
library(randomForest)

#reset variables
training = resetTraining
#remove ID and Target from model
x = model.matrix(~ ., training[, c(-1,-2)])
y = training$Target

testing = resetTesting
#remove ID and Target from model
z = model.matrix(~ ., testing[, c(-1,-2)])
a = testing$Target

#run cross validation - takes a long time
#tc = trainControl(method="cv", number=5)
#train(Target ~. -ID, data=training, method="rf", trControl=tc)



#Build random forest model
#-mytry = number of random variables selcted at each tree split (it's good to have variety for each tree to learn)
#  default 
#    -if regression=floor(number of varaibles/3)
#    -if categorical=floor(sqrt(no of independent variables))
# lower mtry means 1) less correlation between trees (good thing), 2) decreases strength of each tree. cannot predict accurately because of the limited variables (bad thing)

rf_model = randomForest(Target ~. -ID, data = training, importance=TRUE, xtest=testing[,c(-1,-2)], keep.forest=TRUE, ntree=1000)
rf_model
#OOB estimate of  error rate: 0.79%
#Confusion matrix:
#      0    1  class.error
#0 89394   78 0.0008717811
#1   644 1821 0.2612576065


#model summary
summary(rf_model)

#variables contained in model 
names(rf_model)

#probability
test_prob_rf = predict(rf_model, testing, type="prob")

#predictions for test set
test_predictions_rf = predict(rf_model, testing, type="class")
rf_confusion = confusionMatrix(data = as.factor(test_predictions_rf), testing$Target, positive="1")

#predictions for test set
#test_predictions_rf = data.frame(testing, rf_model$test$predicted)
#rf_confusion = confusionMatrix(data = as.factor(test_predictions_rf$rf_model.test.predicted), testing$Target, positive="1")

rf_confusion$byClass["F1"] #0.8247978
rf_confusion
# Confusion Matrix and Statistics
# 
#               Reference
# Prediction        0     1
#             0 38310   291
#             1    34   765
# 
# Accuracy : 0.9918          
# 95% CI : (0.9908, 0.9926)
# No Information Rate : 0.9732          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.8207          
# Mcnemar's Test P-Value : < 2.2e-16       
# 
# Sensitivity : 0.72443         
# Specificity : 0.99911         
# Pos Pred Value : 0.95745         
# Neg Pred Value : 0.99246         
# Prevalence : 0.02680         
# Detection Rate : 0.01942         
# Detection Prevalence : 0.02028         
# Balanced Accuracy : 0.86177         
# 
# 'Positive' Class : 1  

#quantitative measure of variable importance
importance(rf_model)
#sorted plot of importance
varImpPlot(rf_model)

#mean decrease accuracy = how much of the model accuracy decreases if we drop that variable
#high value of mean decrease accuracy or gini scores higher importance of the variable in the model

#top predictors
# 1. mth_since_last_serv
# 2. num_serv_dealer_purchased
# 3. annualised_mileage
# 4. age_of_vechile_years
# 5. total_services

#----------------------------------------------
#   Predict on validation file
#----------------------------------------------

cpurvalid = read.csv("repurchase_validation.csv")
str(cpurvalid)

cpurvalid$Target                     = as.factor(0)
cpurvalid$ID                         = as.integer(0)
cpurvalid$age_of_vehicle_years       = as.factor(cpurvalid$age_of_vehicle_years)
cpurvalid$sched_serv_warr            = as.factor(cpurvalid$sched_serv_warr )         
cpurvalid$non_sched_serv_warr        = as.factor(cpurvalid$non_sched_serv_warr)
cpurvalid$sched_serv_paid            = as.factor(cpurvalid$sched_serv_paid)
cpurvalid$non_sched_serv_paid        = as.factor(cpurvalid$non_sched_serv_paid)
cpurvalid$total_paid_services        = as.factor(cpurvalid$total_paid_services)
cpurvalid$total_services             = as.factor(cpurvalid$total_services)
cpurvalid$mth_since_last_serv        = as.factor(cpurvalid$mth_since_last_serv)
cpurvalid$annualised_mileage         = as.factor(cpurvalid$annualised_mileage)
cpurvalid$num_dealers_visited        = as.factor(cpurvalid$num_dealers_visited)
cpurvalid$num_serv_dealer_purchased  = as.factor(cpurvalid$num_serv_dealer_purchased)

summary(cpurvalid)

#------------------
# check data
#------------------
#frequency charts
ggplot(data.frame(cpurvalid$age_band), aes(x=cpurvalid$age_band)) + geom_bar() + xlab("Age Band")
ggplot(data.frame(cpurvalid$gender), aes(x=cpurvalid$gender)) + geom_bar() + xlab("Gender")
ggplot(data.frame(cpurvalid$car_model), aes(x=cpurvalid$car_model)) + geom_bar() + xlab("Car Model")
ggplot(data.frame(cpurvalid$car_segment), aes(x=cpurvalid$car_segment)) + geom_bar() + xlab("Car Type")
ggplot(data.frame(cpurvalid$age_of_vehicle_years), aes(x=cpurvalid$age_of_vehicle_years)) + geom_bar() + xlab("Vehicle Age (Deciles)")
ggplot(data.frame(cpurvalid$sched_serv_warr), aes(x=cpurvalid$sched_serv_warr)) + geom_bar() + xlab("# Scheduled services (Deciles)")
ggplot(data.frame(cpurvalid$non_sched_serv_warr), aes(x=cpurvalid$non_sched_serv_warr)) + geom_bar() + xlab("# Non-Scheduled services (Deciles)")
ggplot(data.frame(cpurvalid$sched_serv_paid), aes(x=cpurvalid$sched_serv_paid)) + geom_bar() + xlab("Amount paid for scheduled services (Deciles)")
ggplot(data.frame(cpurvalid$non_sched_serv_paid), aes(x=cpurvalid$non_sched_serv_paid)) + geom_bar() + xlab("Amount paid for non scheduled services (Deciles)")
ggplot(data.frame(cpurvalid$total_paid_services), aes(x=cpurvalid$total_paid_services)) + geom_bar() + xlab("Amount paid for all services (Deciles)")
ggplot(data.frame(cpurvalid$total_services), aes(x=cpurvalid$total_services)) + geom_bar() + xlab("Total # services (Deciles)")
ggplot(data.frame(cpurvalid$mth_since_last_serv), aes(x=cpurvalid$mth_since_last_serv)) + geom_bar() + xlab("Months since last service (Deciles)")
ggplot(data.frame(cpurvalid$annualised_mileage), aes(x=cpurvalid$annualised_mileage)) + geom_bar() + xlab("Months since last service (Deciles)")
ggplot(data.frame(cpurvalid$num_dealers_visited), aes(x=cpurvalid$num_dealers_visited)) + geom_bar() + xlab("Number of dealers visited for servicing (Deciles)")
ggplot(data.frame(cpurvalid$num_serv_dealer_purchased), aes(x=cpurvalid$num_serv_dealer_purchased)) + geom_bar() + xlab("Number of services at purchased dealer (Deciles)")



str(cpurvalid)
cpurvalid = cpurvalid[c(17,16,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)]
str(cpurvalid)

firstrow = training[1,]

cpurvalid = rbind(training[1,], cpurvalid)
cpurvalid = cpurvalid[-1,]


#probability
cpurvalid.prob_rf = predict(rf_model, cpurvalid, type="prob")
cpurvalid.prob_rf = as.data.frame(cpurvalid.prob_rf)
cpurvalid$prob_0 = as.numeric(cpurvalid.prob_rf[,1])
cpurvalid$prob_1 = as.numeric(cpurvalid.prob_rf[,2])

#predictions for test set
cpurvalid.predictions_rf = predict(rf_model, cpurvalid, type="class")

cpurvalid$Target = cpurvalid.predictions_rf
cpurvalid.predTarget = nrow(cpurvalid[cpurvalid$Target=="1",]) #1235
#proportion of target predicted
cpurvalid.predTarget / nrow(cpurvalid) #0.0247 -similar to training proportion

write.csv(cpurvalid, "predicted_repurchase_validation.csv")


#---------------------- END ---------------------------



