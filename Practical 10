# Practical 9

# Q1
states <- as.data.frame(state.x77)
str(states)
# Add the states name as a variable
states$name <- state.name

# Renaming Life Exp and HS Grad variables as 
# these will cause possible issues when referring to
# them since they contain a space.
colnames(states)[colnames(states) == "Life Exp"] <- "Life_Exp"
colnames(states)[colnames(states) == "HS Grad"] <- "HS_Grad"

# Q3a
# Examine initial linearity between variables in the dataset
library(psych)
pairs.panels(states,
             smooth = FALSE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = FALSE,    # If TRUE, draws ellipses
             method = "spearman",# Correlation method (also "pearson" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence intervals

# Examine linearity in more detail
scatter.smooth(x = states$Population,
               y = states$Murder,
               xlab = "Population (,000)",
               ylab = "Murder %", main = "Correlation of murder ~ population")

# chart appears to have medium correlation
# low correlation (-0.2 < x < 0.2) suggests that 
# much of variation of the response
# variable (Y) is unexplained by the predictor (X)
# in which case, we should probably look for better
# explanatory variables
cor(states$Murder, states$Population)

scatter.smooth(x = states$Illiteracy,
               y = states$Murder,
               main = "Correlation of Murder ~ Illiteracy",
               xlab = "Illiteracy %",
               ylab = "Murder %")

# Examining correlation between murder and illiteracy
cor(states$Murder, states$Illiteracy)

# This is a better correlation value between both variables.
# Lets examine murder and frost variables for correlation.
scatter.smooth(x = states$Frost,
               y = states$Murder,
               main = "Correlation of Murder ~ Frost",
               xlab = "Frost",
               ylab = "Murder %")
cor(states$Murder, states$Frost)

# Examining the other variables
paste("Correlation for Murder and Frost: ", cor(states$Murder, states$Frost))
paste("Correlation for Murder and Illiteracy: ", cor(states$Murder, states$Illiteracy))
paste("Correlation for Murder and Population: ", cor(states$Murder, states$Population))
paste("Correlation for Murder and HS Grad: ", cor(states$Murder, states$HS_Grad))
paste("Correlation for Murder and Income: ", cor(states$Murder, states$Income))
paste("Correlation for Murder and Life Exp: ", cor(states$Murder, states$Life_Exp))
paste("Correlation for Murder and Area: ", cor(states$Murder, states$Area))

# It appears that the variable Area has a vary low correlation with Murder. 
# Therefore I am going to remove it from the dataset. 
# Alternatively we can choose to exclude these independent variables when
# we are constructing the MLR model.

# Q3b
# Check for outliers
opar <- par(no.readonly = TRUE)
par(mfrow = c(4, 2)) # divide graph area in 3 rows by 2 columns
attach(states)
boxplot(Murder,
        main = "Murder",
        sub = paste("Outlier rows: ",
                    boxplot.stats(Murder)$out)) # box plot for 'murder'
boxplot(Population,
        main = "Population",
        sub = paste("Outlier rows: ",
                    boxplot.stats(Population)$out)) # box plot for 'Population'
boxplot(states$HS_Grad,
        main = "Graduation",
        sub = paste("Outlier rows: ",
                    boxplot.stats(states$HS_Grad)$out)) # box plot for 'HS Grad'
boxplot(Illiteracy,
        main = "Illiteracy",
        sub = paste("Outlier rows: ",
                    boxplot.stats(Illiteracy)$out)) # box plot for 'HS Grad'
boxplot(Income,
        main = "Income",
        sub = paste("Outlier rows: ",
                    boxplot.stats(Income)$out)) # box plot for 'HS Grad'
boxplot(Frost,
        main = "Frost",
        sub = paste("Outlier rows: ",
                    boxplot.stats(Frost)$out)) # box plot for 'HS Grad'
boxplot(states$Life_Exp,
        main = "Life Exp",
        sub = paste("Outlier rows: ",
                    boxplot.stats(states$Life_Exp)$out)) # box plot for 'HS␣Grad'
detach(states)
par(opar)

# Both the population and Income variables contain outliers.
# Use boxplot.stats() function to generate relevant outliers
outlier_values <- boxplot.stats(states$Population)$out # outlier values.
paste("Population outliers: ", paste(outlier_values, collapse=", "))

# Use boxplot.stats() function to generate relevant outliers
outlier_values <- boxplot.stats(states$Income)$out # outlier values.
paste("Income outliers: ", paste(outlier_values, collapse=", "))

# Remove population outliers
states <- subset(states,
                 states$Population != 21198
                 & states$Population != 11197
                 & states$Population != 18076
                 & states$Population != 11860
                 & states$Population != 12237)
# Remove Income outliers
states <- subset(states, states$Income != 6315)
# Remove Area outliers
states <- subset(states,
                 states$Area != 566432
                 & states$Population != 156361
                 & states$Population != 262134)

# Re-run the box-plots to verify that outliers have now gone.

# Q3c
# Check for normality
# Skewness function to examine normality
# install.packages("e1071")
library(e1071)
opar <- par(no.readonly = TRUE)
par(mfrow = c(4,2)) # divide graph area into 1 row x 2 cols

# skewness of < -1 or > 1 = highly skewed
# -1 to -0.5 and 0.5 to 1 = moderately skewed
# Skewness of -0.5 to 0.5 = approx symetrical
plot(density(states$Population),
     main = "Density plot : Population",
     ylab = "Frequency", xlab = "Population",
     sub = paste("Skewness : ", round(e1071::skewness(states$Population), 2)))
# fill the area under the plot
polygon(density(states$Population), col = "red")

plot(density(states$Murder),
     main = "Density plot : Murder",
     ylab = "Frequency", xlab = "Murder",
     sub = paste("Skewness : ", round(e1071::skewness(states$Murder), 2)))
polygon(density(states$Murder), col = "red")

plot(density(states$HS_Grad),
     main = "Density plot : HS grade",
     ylab = "Frequency", xlab = "HS grade",
     sub = paste("Skewness : ", round(e1071::skewness(states$HS_Grad), 2)))
# fill the area under the plot
polygon(density(states$HS_Grad), col = "red")
plot(density(states$Illiteracy),
     main = "Density plot : Illiteracy",
     ylab = "Frequency", xlab = "Illiteracy",
     sub = paste("Skewness : ", round(e1071::skewness(states$Illiteracy), 2)))
polygon(density(states$Illiteracy), col = "red")
plot(density(states$Income),
     main = "Density plot : Income",
     ylab = "Frequency", xlab = "Income",
     sub = paste("Skewness : ", round(e1071::skewness(states$Income), 2)))
# fill the area under the plot
polygon(density(states$Income), col = "red")
plot(density(states$Frost),
     main = "Density plot : Frost",
     ylab = "Frequency", xlab = "Feost",
     sub = paste("Skewness : ", round(e1071::skewness(states$Frost), 2)))
# fill the area under the plot
polygon(density(states$Frost), col = "red")
par(opar)

# Minimal skewness = -0.11 - slightly skewed to the left. 
# NB a skewness value <-1 or >1 = highly skewed. 
# Skewness -1 to -05 and 0.5 to 1 = moderately skewed. 
# And skewness -0.5 to 0-5 = approx symetric.
# Illiteracy = 0.87 = moderatly skewed
# Population = 1.15 = highly skewed
# all others seem okay
paste("Skewness for illiteracy : ", round(e1071::skewness(states$Illiteracy), 2))
paste("Skewness for population : ", round(e1071::skewness(states$Population), 2))
paste("Skewness for murder : ", round(e1071::skewness(states$Murder), 2))
paste("Skewness for HS grad : ", round(e1071::skewness(states$HS_Grad), 2))
paste("Skewness for income : ", round(e1071::skewness(states$Income), 2))
paste("Skewness for frost : ", round(e1071::skewness(states$Frost), 2))

# It seems that population must be converted
# Data is visually skewed to the right
hist(states$Population)

# p-value indices that the data is not normally distributed
shapiro.test(states$Population)

# Check normality of the other variables
shapiro.test(states$Illiteracy)
shapiro.test(states$Murder)
shapiro.test(states$HS_Grad)
shapiro.test(states$Income)
shapiro.test(states$Frost)

# If p-value < 0.05 then variable
# is not normally distributed

# Illiteracy is not normally distributed (p-value = 8.297e-05)
# Murder is normally distributed (p-value = 0.06601)
# HS_grad is not normally distributed (p-value = 0.02194)
# Income is normally distributed (p-value = 0.3246)
# Frost is normally distributed (p-value = 0.0928)

# Need to transform illiteracy, HS_grad and populaiton

library(MASS)

# find optimal lambda for box-cox transform
# Plotting lambda from -6 to 6 in steps of 0.1
Box = boxcox(states$Population ~ 1, lambda = seq(-6,6,0.1))
# Create a data frame with the results
Cox = data.frame(Box$x, Box$y)   
# Order the new data frame by decreasing y
cox_smallest_y = Cox[with(Cox, order(-Cox$Box.y)),] 
# Display the lambda with the greatest
# log likelihood
cox_smallest_y[1,]                                  
# Extract that lambda
lambda = cox_smallest_y[1, "Box.x"]                 
lambda
transformed_population = (states$Population ^ lambda - 1)/lambda
hist(transformed_population)

# p-value indictes that the data is now normally distributed
shapiro.test(transformed_population)

# Convert HS_grad
Box = boxcox(states$HS_Grad ~ 1, lambda = seq(-6,6,0.1))
# Create a data frame with the results
Cox = data.frame(Box$x, Box$y)   
# Order the new data frame by decreasing y
cox_smallest_y = Cox[with(Cox, order(-Cox$Box.y)),] 
# Display the lambda with the greatest
# log likelihood
cox_smallest_y[1,]                                  
# Extract that lambda
lambda = cox_smallest_y[1, "Box.x"]                 
lambda
transformed_HS_grad = (states$HS_Grad ^ lambda - 1)/lambda
hist(transformed_HS_grad)

# p-value indictes that the data is now normally distributed
shapiro.test(transformed_HS_grad)

# Convert illiteracy
# Data is not normally distributed

# Transform illiteracy as a single vector
# Try values -6 to 6 by 0.1
Box = boxcox(states$Illiteracy ~ 1, lambda = seq(-6,6,0.1))
# Create a data frame with the results
Cox = data.frame(Box$x, Box$y)   
# Order the new data frame by decreasing y
cox_smallest_y = Cox[with(Cox, order(-Cox$Box.y)),] 
# Display the lambda with the greatest
# log likelihood
cox_smallest_y[1,]                                  
# Extract that lambda
lambda = cox_smallest_y[1, "Box.x"]                 
lambda
transformed_illiteracy = (states$Illiteracy ^ lambda - 1)/lambda
hist(transformed_illiteracy)
shapiro.test(transformed_illiteracy)

# Illiteracy is still not normally distributed

# Tukey’s Ladder of Powers transformation
# also indicates that the best value for 
# lambda is -0.6 for illiteracy variable
library(rcompanion)

#transform_tukey_illiteracy = transformTukey(states$Illiteracy, plotit=FALSE)
# -0.6 indaictes that "-1 * x ^ lambda" is required
#transformed_illiteracy <- -1 * states$Illiteracy ^ -0.625
#shapiro.test(transformed_illiteracy)

# Converting data in data frame
states$transformed_Population <- transformed_population
states$transformed_HS_Grad <- transformed_HS_grad
states$transformed_Illiteracy <- transformed_illiteracy

# Practical 10 ---------------------------------------------------------------


attach(states)

# Split the data into training and testing
set.seed(1)
no_rows_data <- nrow(states)
sample <- sample(1:no_rows_data, size = round(0.7 * no_rows_data), replace = FALSE)

training_data <- states[sample, ]
testing_data <- states[-sample, ]

unmodified_model <- lm(Murder ~ Population + Income + Illiteracy + Life_Exp + HS_Grad + Frost, data=training_data)
summary(unmodified_model)

# Examine which combination of independent variables best fits the model
# install.packages("leaps")
library(leaps)
# See https://cran.r-project.org/web/packages/leaps/leaps.pdf
# Regression Subset Selection
MLR_subset_selection <-regsubsets(Murder ~ Population + Income + Illiteracy + Life_Exp + HS_Grad + Frost, data=training_data, nbest=6)
plot(MLR_subset_selection, scale="adjr2")

# I want to use the model to examine whether population, illiteracy
# and high school grade has an impact on murder rate
# There is an adjusted r2 model at 0.7 which does not use income 
# as an independent variable

# Smallest AIC is best
# Seems that Murder ~ Population + illiteracy + Life_Exp + HS_grad 
# is best. I'm dropping frost as it has probably
# a spurious correlation
stepAIC(unmodified_model, direction="backward")
