# Read data
normal_data <- read.csv("normaldata.csv")
non_normal_data <- read.csv("nonnormaldata.csv")


#define plotting region
opar <- par(no.readonly = TRUE)
par(mfrow=c(1,2)) 

#create histogram for both datasets
hist(normal_data$x, col='steelblue', main='Frequency chart of data distribution (parametric data)')
hist(non_normal_data$x, col='steelblue', main='Frequency chart of data distribution (non-parametric data)')

# Option 2
# Create a q-q plot for both datasets

# The first Q-Q plot exhibits a dataset that is normally distributed 
# (the points fall along a straight diagonal line) 
# Second Q-Q plot exhibits a dataset that is not normally distributed.

qqnorm(normal_data$x, main='Normal')
qqline(normal_data$x)

qqnorm(non_normal_data$x, main='Non-normal')
qqline(non_normal_data$x)

# Shapiro-wilk testing for parametric dataset
# If p-value >0.5 then data is normally distributed
# If p-value <0.5 then data not normally distributed
# The test gives you a W value; small values indicate your sample is 
# not normally distributed 
# We use the p-value for analysis

# Note: The sample size must be between 3 and 5,000 
# to use the shapiro.test() function.
shapiro.test(normal_data$x)

# Shapiro-wilk testing for nonparametric dataset
shapiro.test(non_normal_data$x)

# Option 4 - perform kolmogorov-smirnov test
# If p-value >0.5 then data is normally distributed
# If p-value <0.5 then data not normally distributed

ks.test(normal_data$x, 'pnorm')

ks.test(non_normal_data$x, 'pnorm')

# How can we make non-parametric data normally distributed?

# Log transform
# depends on skewness

# skewness test
install.packages("moments")
library(moments)
skewness(non_normal_data$x, na.rm = TRUE)
