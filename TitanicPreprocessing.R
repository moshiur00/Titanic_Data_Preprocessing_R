# Author: Md. Moshiur Rahman
# Title: Titanic Data Preprocessing
# Data From: https://raw.githubusercontent.com/thomaspernet/data_csv_r/master/data/titanic_csv.csv


## Importing dataset on titanic data and assign it to the data frame "df_titanic"
PATH <- "https://raw.githubusercontent.com/thomaspernet/data_csv_r/master/data/titanic_csv.csv"
df_titanic <- read.csv(PATH, sep = ',', stringsAsFactors =FALSE)


## Call the below function to examine the data set
#The summary provides descriptive statistics including the min, max, mean, median, and quartiles of each column.
summary(df_titanic)
# shows the dimensions of the data frame by row and column
dim(df_titanic)
# shows the structure of the data frame
str(df_titanic)
# shows the name of each column in the data frame
colnames(df_titanic)
# The head() and tail() functions default to first and last 6 rows respectively, but we can adjust the number of rows using the "n = " argument
head(df_titanic, n = 10)
tail(df_titanic, n = 5)
# The View() function opens a table in another window
View(df_titanic)


# Return the column names containing the missing observation
list_na <- colnames(df_titanic)[apply(df_titanic, 2, anyNA)]
list_na

## For handling missing data with MICE Pacckage: install.packages("mice")
#The mice package provides a nice function md.pattern() to get a better understanding of the pattern of missing data
# The output tells us that 1045 samples are complete, 263 samples miss only the age, 1 samples miss only the fare value and so on.
library(mice)
md.pattern(df_titanic)

# For Visual Representation: install.packages("VIM")
library(VIM)
aggr_plot <- aggr(df_titanic, col=c('green','red'), numbers=TRUE, sortVars=TRUE, labels=names(df_titanic), cex.axis=.6, gap=2, ylab=c("Histogram of missing data","Pattern"))
# Another helpful visual approach is a special box plot
# Here we are constrained at plotting 2 variables at a time only
marginplot(df_titanic[c(2, 3)])


# Sometimes a missing value might be read into R as a empty string or blank
# The embarked column has some missing values that has not detected by R
is.na(df_titanic$embarked[df_titanic$embarked =='']) <- TRUE
sum(is.na(df_titanic$embarked))
#Find the missing values of df_titanic$embarked replace NA with 'S'
df_titanic$embarked <- replace(df_titanic$embarked, which(is.na(df_titanic$embarked)), 'S')
sum(is.na(df_titanic$embarked))


is.na(df_titanic$boat[df_titanic$boat =='']) <- TRUE
sum(is.na(df_titanic$boat))

# Replacing missing age values with mean values of age as we can see from the plot that about 20% age data are missing
age_replace <- mean(df_titanic$age, na.rm = TRUE)
age_replace
df_titanic$age <- replace(df_titanic$age, which(is.na(df_titanic$age)), age_replace)
sum(is.na(df_titanic$age))


# Imputing missing data using MICE package of R
# mice() function takes care of the imputing process 
# for methods of MICE: methods(mice)
# m=5 refers to the number of imputed datasets. meth='pmm' refers to the imputation method. 
tempdata <- mice(df_titanic,m=5,maxit=50,meth='pmm',seed=500)
# Check the imputed data
tempdata$imp$fare
tempdata$imp$age
# Check the imputation method used for each variable
tempdata$meth
completedData <- complete(tempdata,1)
completedData

library(dplyr)
# Create Mean
average_missing <- apply(df_titanic[, colnames(df_titanic) %in% list_na], 2, mean, na.rm=TRUE)
median_missing <- apply(df_titanic[, colnames(df_titanic) %in% list_na], 2, median, na.rm=TRUE)

# Create a new variable with the mean and median
df_titanic_replace_mean <- df_titanic %>%
  mutate(replace_mean_age=ifelse(is.na(age), average_missing[1], age),
         replace_median_fare=ifelse(is.na(fare), average_missing[2], fare))



df_titanic_replace_median <- df_titanic %>%
  mutate(replace_median_age=ifelse(is.na(age), median_missing[1], age),
         replace_median_fare=ifelse(is.na(fare), median_missing[2], fare))

sum(is.na(df_titanic_replace_mean$age))
sum(is.na(df_titanic_replace_mean$replace_mean_age))


write.csv(df_titanic_replace_mean, file = 'titanic_clean_mean.csv')
write.csv(df_titanic_replace_median, file = 'titanic_clean_median.csv')

