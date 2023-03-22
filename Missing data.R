
# -------------------------------------------------------------------------------
# Dealing with missing data 
# -------------------------------------------------------------------------------

# Load the managers data frame first
managers_data <- read.csv("managers.csv")

# removes any rows that contains NA - listwise deletion
new_data <- na.omit(managers_data)
new_data

# Use complete.cases to show rows where data is available
complete_data <- complete.cases(managers_data)
complete_data
# Show sum of missing rows
sum(complete_data)

# list the rows that do not have missing values
# Note that the ',' and no number inside square brackets means "all columns"
complete_data <- managers_data[complete.cases(managers_data),]
complete_data

# List rows with missing values
managers_data[!complete.cases(managers_data),]

# Find sum of all missing values in the age attribute
sum(is.na(managers_data$Age))

# Find the mean of missing values from the Age attribute
mean(is.na(managers_data$Age))

# Find the mean of rows with no incomplete data
# Note that we dont need to add the ',' as we're only
# looking for an overall mean of rows with missing values
mean(!complete.cases(managers_data))

install.packages("mice")
library(mice)
md.pattern(managers_data)

# USe VIM package to show missing values
install.packages("VIM")
library(VIM)
missing_values <- aggr(managers_data, prop = FALSE, numbers = TRUE)
# Show summary of the contents of missing_values
summary(missing_values)
# See this link for more info https://www.rdocumentation.org/packages/VIM/versions/4.8.0/topics/aggr 

# View missing data as a matrix
# where red indicates a missing value
# using a heatmap
# And other colours represent identical answers
matrixplot(managers_data)
