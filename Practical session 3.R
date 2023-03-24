# Merging datasets ------------------------------------------------------
#
# Answers to practical problem 3 - merging datasets
#
# -----------------------------------------------------------------------

# Read in the datasets first
# Data needs to be in the working directory
# Move data from blackboard to WD 
# and ensure that both csv files are in files window
# in RStudio
new_managers_data <- read.csv("MoreData.csv")
managers_data <- read.csv("managers.csv")

# When analysing data frame structures we must
# examine if there are differences between both
# data frames. Both must use same structure if we
# want to merge them.
str(new_managers_data)
str(managers_data)

# Show headers of the data
# using names function
# There are obvious additional data in
# managers_data that are not in new_managers_data
names(managers_data)
names(new_managers_data)

# Build a list of variables that we want to 
# include in both data frames
# Managers contains more variables than
# new_managers and therefore the list is different
managers_include_list <- c("Date", "Country", "Gender", "Age", "Q1", "Q2", "Q3", "Q4", "Q5", "AgeCat", "Answer.total", "mean.value")
managers_include_list

new_managers_include_list <- c("Date", "Country", "Gender", "Age", "Q1", "Q2", "Q3", "Q4", "Q5")
new_managers_include_list
# Use this method with caution as the position of variables within both datasets
# may not be the same
include_list <- c(6,3,7,4,8:12)
include_list

# Extract the variables shown in include_list 
# for both data frames
managers_data <- managers_data[managers_include_list]
new_managers_data <- new_managers_data[new_managers_include_list]

# Confirm that both data frames are identical
# managers_data contains an additional variable
# called AgeCat that must be created
head(managers_data)
head(new_managers_data)

# We can't use rbind() function yet
# because of structure mismatch
rbind(managers_data, new_managers_data)

head(new_managers_data)

# ------Calculate additional values and add to data frame --------------- 

# we can now calculate AgeCat values
# and add to new_managers_data dataframe
attach(new_managers_data)
new_managers_data$AgeCat[Age >= 45] <- "Elder"
new_managers_data$AgeCat[Age >= 26 & Age <= 44] <- "Middle Aged"
new_managers_data$AgeCat[Age <= 25] <- "Young"
new_managers_data$AgeCat[is.na(Age)] <- "Elder"
detach(new_managers_data)

# Total answers column can then be recreated
# Create a new column called 'summary_col' that
# contains a summary of each row
# use attach to reduce the need to provide full 
# reference to each variable
# In this example we do not need to refer to newe_managers_data when
# we use the attach function
attach(new_managers_data)
total_answers <- Q1 + Q2 + Q3 + Q4 + Q5
mean_value <- rowMeans(new_managers_data[5:9])
detach(new_managers_data)

# Now add computed columns to
# the new_managers data frame
new_managers_data <- data.frame(new_managers_data, total_answers, mean_value)
head(new_managers_data)

# ------ Convert date variables to Date type --------------------------- 

# Now we need to convert the date field to date
# in both data frames
str(managers_data)
str(new_managers_data)

# Examining the date field in manager_data
# Date is in mm/dd/yy format in the include_list data
# Refer to https://www.statology.org/r-date-format/ for 
# symbol representation of date structure
str(managers_data$Date)

# Date is a character as it is not in the
# default date structure
# If we try to convert it then 
# R will not understand this structure
str(managers_data$Date)
example_of_date_issue <- as.Date(managers_data$Date)
example_of_date_issue

# Instead we convert the date to the
# default R structure 
# and inform R of the "current" date structure
# which in manages is in yyyy-dd-mm format
formatted_managers_date <- as.Date(managers_data$Date, format = "%Y-%d-%m")
str(formatted_managers_date)
class(formatted_managers_date)

# Repeat conversion for new_managers_data Date field
# Examine the structure first
str(new_managers_data$Date)

# Date is in mm/dd/yyyy format
# and required upper case Y to represent
# 4 digit year element
formatted_new_managers_date <- as.Date(new_managers_data$Date, format = "%m/%d/%Y")
str(formatted_new_managers_date)
class(formatted_new_managers_date)

# Then update both data frames with new date
managers_data$Date <- formatted_managers_date
new_managers_data$Date <- formatted_new_managers_date
head(managers_data)
str(managers_data)

# names are not identical between both data frames
# Therefore one needs to change to match the other
# In this example I'm changing both as I
# want to update them to remove space and dot
names(managers_data)[11] <- "AnswerTotal"
names(new_managers_data)[11] <- "AnswerTotal"
names(managers_data)[12] <- "MeanValue"
names(new_managers_data)[12] <- "MeanValue"

# ------------------ Merge data frames vertically --------------------------
# Now we can combine both datasets with
# rbind function
managers_data <- rbind(managers_data, new_managers_data)
managers_data
str(managers_data)
head(managers_data)
