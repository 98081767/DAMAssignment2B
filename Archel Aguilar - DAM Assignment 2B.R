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
#setwd("C:/Personal/UTS/R-References/R-references-Git/DAM-Assignment-2B")

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
testing = cpur[-train, ]
str(training)
str(testing)

nrow(cpur)     #131,337
nrow(training)  #91,937
nrow(testing)   #39,400

nrow(cpur[cpur$Target=="1",]) #3521 - number of targets
nrow(cpur[cpur$Target=="0",]) #127816 - number of non targets
#proportion of targets in data = 0.0268

nrow(training[training$Target=="1",]) #2465 - targets in trainng
nrow(training[training$Target=="0",]) #89472 - non targets in training
#proportion of targets in data = 0.0268


#---------------------------
# Create models
#---------------------------
glmodel = "Target ~. -ID" #all variables
#glmodel = "Target ~ gender - ID"

#include all except for identifier (ID)
cpur.glm = glm(formula = glmodel,
             data = training,
             family = "binomial")
summary(cpur.glm)
#AIC: 9016


###########################
# Create probabilities and predictions
###########################

# add the probabilities to the testing data
testing$probability = predict(cpur.glm, newdata = testing, type = "response")

# assume that the optimum probability threshold is 0.5
# Create the class prediction - our target is 1
testing$prediction = "0"
testing[testing$probability >= 0.5, "prediction"] = "1"


# Have a look at the data
#head(testing)

###########################
# Evaluation

# Create a confusion matrix (along with other measures) using the 
# function 'confusionMatrix' from the caret package
myPred = as.factor(testing$prediction)
#str(myPred)
#str(testing$prediction)
#str(testing$Target)
#testing$prediction
#testing$Target

#set Target=1 as the focus for confusion matrix
cm = confusionMatrix(data = myPred, testing$Target, positive="1")
#get F1 score
cm$byClass["F1"] #0.6553341

#summary
cm
#                 Reference
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



# ------------- 
# Run PCA
# -------------

install.packages("pls")
library(pls)









