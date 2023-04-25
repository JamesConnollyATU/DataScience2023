
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
