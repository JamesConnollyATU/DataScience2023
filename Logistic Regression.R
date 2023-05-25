# Practical 12
# Data is a simulated dataset containing information 
# on ten thousand customers. The aim is to predict
# which customers will default on their credit card debt

# load dataset
# install.packages("ISLR")
library(ISLR)
student_data <- ISLR::Default
write.csv(student_data, "Student data.csv")

# view summary of dataset & total observations
summary(student_data)
nrow(student_data)

# Creating training and testing data
# make this example reproducible
set.seed(1)
no_rows_data <- nrow(student_data)
sample <- sample(1:no_rows_data, size = round(0.7 * no_rows_data), replace = FALSE)
# 70% training, 30% testing
training_data <- student_data[sample, ]
testing_data <- student_data[-sample, ]


# Fit the logistic model
# Note we use glm instead of lm for logistic regression
attach(student_data)
log_model <- glm(default ~ student 
                 + balance 
                 + income, 
                 family="binomial", 
                 data=training_data)

# disable scientific notation for model summary
# to show full extent of powered values
options(scipen=999)

# view model summary
# one unit increase in balance is associated with 
# an average increase of 0.005988 in the log odds of defaulting.
# Income not as important as student status as imortant predictors
# of defaulting
summary(log_model)

# calculate McFadden's R-Squared
# from the pscl library
# Value 0.46 = fairly high and model 
# fits the data well
#install.packages("pscl")
library(pscl)
pscl::pR2(log_model)["McFadden"]


#calculate variable importance
#install.packages("caret")
# Higher values indicate more importance. 
# These results match up nicely with the p-values 
# from the model. Balance is by far the most important 
# predictor variable, followed by student status and then income.
library(caret)
varImp(log_model)

#calculate VIF values
# of each variable in the model to see 
# if multicollinearity is a problem:
#  VIF values above 5 indicate severe multicollinearity. 
# Since none of the  predictor variables in our models have a 
# VIF over 5, we can assume that multicollinearity 
# is not an issue in our model.
library(car)
vif(log_model)




# We will examine the optimal cutoff for the model later on.

# Model diagnostics
# need to install devtools so that
# informationValue can be installed from github
#install.packages('devtools')
devtools::install_github("selva86/InformationValue")

library(InformationValue)
# Evaluate optimum cutoff for dependent variable
# convert dependent variable first
# defaults information from "Yes" and "No" to 1's and 0's
testing_data$will_default <- ifelse(testing_data$default=="Yes", 1, 0)

#calculate probability of default for each individual in test dataset
# to use for optimal calculation
predicted_sample <- predict(log_model, testing_data, type="response")

#find optimal cutoff probability to use to maximise accuracy
# the optimal probability cutoff to use is 0.3295383. 
# Thus, a person with a probability of defaulting of 0.3295383
# or higher will be predicted to default, while anyone
# with a probability less than this number will 
# be predicted to not default.
optimal <- optimalCutoff(testing_data$will_default, predicted_sample)[1]
optimal

# Add a new column into the dataframe
# so that it reflects the optimum cutoff

actuals_predictions <- data.frame(cbind(actuals = testing_data$will_default,
                                        predicted = predicted_sample,
                                        optimum =ifelse(predicted_sample >= optimal, 1, 0)))
head(actuals_predictions, 20)


# define two individuals for initial discussion
# The probability of an individual with a balance 
# of $1,400, an income of $2,000, and a student status of “Yes” 
# has a probability of defaulting of .0273. Conversely, 
# an individual with the same balance and income but 
# with a student status of “No” has a probability of 
# defaulting of 0.0439. 
predicted_data <- data.frame(balance = 1400, income = 2000, student = c("Yes", "No"))
predicted <- predict(log_model, predicted_data, type="response")
predicted

# An individual with a balance of $5000 and and income of 2000
# and is not a student is 99% probability of defaulting
predicted_data <- data.frame(balance = c(1400, 5000), income = 2000, student = c("Yes", "No"))
predicted <- predict(log_model, predicted_data, type="response")
predicted

# This model will have 94% accuracy
correlation_accuracy <- cor(actuals_predictions)
correlation_accuracy

#create confusion matrix
# to show test data predictions
# versus model predictions
confusionMatrix(testing_data$will_default, actuals_predictions$optimum)

#calculate sensitivity
# true positive rate
sensitivity(testing_data$will_default, actuals_predictions$optimum)

#calculate specificity
specificity(testing_data$default, actuals_predictions$optimum)

#calculate total misclassification error rate
# This is the total misclassification
# error rate for the model.
# Error rate = 2.5%. Lower error rate
# means the better the model can
# predict outcomes
misClassError(testing_data$will_default, actuals_predictions$optimum, threshold=optimal)

#plot the ROC curve
plotROC(testing_data$will_default, actuals_predictions$optimum)

