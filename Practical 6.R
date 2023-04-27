# Practical 6 --------------------------------------------------------
# Refer to notes on Blackboard
# --------------------------------------------------------------------


# Using statistical methods to examine
# the relationships between variables of interest

?beavers
str(beaver2)
# The beaver dataset contains data on body temp of 4 beavers
# every 10 mins over a day for demo purposes 
# We want to examine the difference in average body temp
# during periods of activity to evaluate whether
# body temperature is affected by activity
# First we need to ensure that data is in correct format
# Activ should be a factor
# Temp is numerical

# H0: Body temperate is not affected by activity
# h1: Body temperature is affected by activity

# I'm copying the data to a data frame
# This is not a necessary step
beavers_data <- beaver2
str(beaver2)

# Using the default pairs() option first
# to examine correlations between variables
pairs(beavers_data, labels = colnames(beavers_data), main = "Beavers dataset correlation plot")

# I'm examining body temp and activity so I need 
# to prepare both variables first
# change activ to a factor variable
# as it seems to be a categorical dichotomous variable
# The temp variable is a continuous variable, and it is 
# in numeric format already so does not need to be converted

# labels starts with what is assigned to lower value first
# eg 0 = no, 1 = yes
beavers_data$activity <- factor(beaver2$activ, labels = c("no", "yes"))

# We could alternatively use the transform command
# as an alternative method to perform the conversion
beavers_data <- transform(beaver2,
                          activ = factor(activ, labels = c("no", "yes")))

# Lets look at the correlation between both of these variables
# to evaluate the strength of the relationship
# and whether it is negative or positive

# We can use libraries to help improve 
# the chart. Also includes correlations between variables
install.packages("psych")
library(psych)

# Seems there could be a positive correlation between 
# temp and activ
pairs.panels(beavers_data,
             smooth = TRUE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = TRUE,    # If TRUE, draws ellipses
             method = "spearman",# Correlation method (also "pearson" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence intervals

# Before carrying any analysis, summarise the 
# medians and interquartile range by group.
# When reporting differences between groups for 
# skewed data, it is common to report the medians 
# by group rather than the means 
tapply(beavers_data$temp, median)
# summary data can be used to show this as well
# Shows the temperature details for no and yes answers
tapply(temp, activity, summary)


# We can show this categorical data in a plot
# but not in a scatter plot
attach(beavers_data)
plot(beavers_data$activity, temp, pch = 19, col = "lightblue")

# We can split the activity data into 2 subsets
# and then use the histogram() function
library("lattice")
# The histogram uses a 1 sided formula, so we
# dont specify anything on left side of ~
# and on right side we specify which variable is in the histogram
# ie temp.
# After the vertical line we show the factor by which the data
# is split ie "activ"
attach(beavers_data)
histogram(~temp | activ, 
          data = beavers_data, 
          main = "Distribution of beaver activity data", 
          xlab = "Temperature (degrees)", 
          ylab = "Activity %")
detach(beavers_data)

# ----------------------------------------------------------------
# Selecting the appropriate test
# ----------------------------------------------------------------
# We need to check whether the data is normally distributed or not
# See notes on Blackboard for more information

# Quantile-quantile plot allows us to check if the
# data is distributed normally
# Compare the quantiles of both samples 
# We use square brackets to select the cases we want
attach(beavers_data)
qqnorm(temp)
# this line represents normal distribution
qqline(temp, col = "red")

# We can examine whether there is a linear
# correlation between both answers in
# the activity variable
# Seems that the "yes" occurrences may 
# be normally distributed and the "no"
# occurrences may not be normally distributed

opar <- par(no.readonly = TRUE)
# arrange plots in 1 rows and 2 column
par(mfrow = c(1, 2))

with(beavers_data, {
  qqnorm(temp[activity == "yes"], 
         main = "Beavers active data")
  qqline(temp[activity == "yes"])
})

with(beavers_data, {
  qqnorm(temp[activity == "no"], 
         main = "Beavers inactive data")
  qqline(temp[activity == "no"])
})

par(opar)

# Formal test of normality
# provided through widely used Shapiro-Wilks test
normality_test <- shapiro.test(beavers_data$temp)
normality_test$p.value
# p-value tells us the chances that the sample comes 
# from a normal distribution 
# In this example, p-value is clearly lower than 0.05
# so not normally distributed

# this method does not work for a dichotomous variable
# Data needs to be numeric for shapiro Wilk test
normality_test <- shapiro.test(beavers_data$activity)

# We can check the normality in each variable
# using the tapply() function instead
with(beavers_data, tapply(temp, activity, shapiro.test))

# OR
tapply(temp, activity, shapiro.test)

# Data is not normally distributed
# and p-value for "temp" variable also indicates that the
# data is not normally distributed
# so we need to use a non-parametric test

# After consulting the chart, I am examining
# a dependent continuous variable (temp)
# with an independent categorical variable (activity)
# so I use the Mann-Whitney test
# this is also known as the "wilcox test" in R
# Format = wilcox.test(dependent~independent)
wilcox.test(temp~activity)

# p-value is < 0.05 so we reject H0 and conclude that 
# beaver body temperature is affected by activity (p = 2.2e-16)
