# UDatE Preprocessing task code 

# Load necessary packages
library(tidyverse)
library(summarytools)
library(ggplot2)
library(car)
library(caret)
library(MASS)
library(mctest)
library(gmodels)
library(wesanderson)
library(stargazer)
library(dplyr)

# Loading the data files into R
store <- read.csv('store.csv')
attach(store)
summary(store)

train <- read.csv('train.csv')
attach(train)
summary(train)

test <- read.csv('test.csv')
attach(test)
summary(test)

# Store dataframe pre-processing
# Finding missing values in store dataframe
sapply(store, function(x) sum(is.na(x)))

# Creating indicator of months since competition opened
store$CompYearCountRev <- store$CompetitionOpenSinceYear-1990

store$CompYearCount <- 25-store$CompYearCountRev

store$CompetitionMonthsOpen <- 
  (store$CompYearCount*12)+store$CompetitionOpenSinceMonth

# Creatinig indicator of weeks since promo
store$PromoYearCountRev <- store$Promo2SinceYear-2009

store$PromoYearCount <- 6-store$PromoYearCountRev

store$PromoWeeksOn <- (store$PromoYearCount*52)+store$Promo2SinceWeek


# Missing values imputation for continuous variables

store$CompetitionDistance[is.na(store$CompetitionDistance)] <- 0

store$CompetitionMonthsOpen[is.na(store$CompetitionMonthsOpen)] <- 0

store$PromoWeeksOn[is.na(store$PromoWeeksOn)] <- 0



# Drop unnecessary columns
remove_cols <- c('CompetitionOpenSinceMonth', 'CompetitionOpenSinceYear',
                 'Promo2SinceWeek', 'Promo2SinceYear', 'CompYearCount', 
                 'PromoYearCount', 'CompYearCountRev', 'PromoYearCountRev')


store = subset(store, select = !(names(store) %in% remove_cols)) 

# Remove outliers
boxplot(store$CompetitionMonthsOpen)

store<-subset(store, CompetitionMonthsOpen<500)


# Train data frame pre-processing

# Checking for missing values
sapply(train, function(x) sum(is.na(x)))

train <- subset(train, Open!=0) # Drop rows if store was closed
train <- subset(train, Sales!=0) # Drop rows if Sales were 0
train <- subset(train, select = -c(Open)) # Drop Open column

# Test dataframe pre-processing

# Checking for missing values
sapply(test, function(x) sum(is.na(x)))

test <- subset(test, Open!=0) # Drop rows if store was closed
test <- subset(test, select = -c(Open)) # Drop open column
test <- subset(test, Store!=146 & Store!=815) # Get rid of outliers


# Linking train / test and store data frames via the shared Store column
train_final <- merge(train, store)
train_final <- train_final[complete.cases(train_final),] 

test_final <- merge(test, store)

# Train_final Pre-Processing
# Change date into a date variables
train_final$Date <- as.Date(train_final$Date, format="%d/%m/%Y")

# Make sure all variables are appropriate types
train_final$DayOfWeek[train_final$DayOfWeek == 1] <- "Monday"
train_final$DayOfWeek[train_final$DayOfWeek == 2] <- "Tuesday"
train_final$DayOfWeek[train_final$DayOfWeek == 3] <- "Wednesday"
train_final$DayOfWeek[train_final$DayOfWeek == 4] <- "Thursday"
train_final$DayOfWeek[train_final$DayOfWeek == 5] <- "Friday"
train_final$DayOfWeek[train_final$DayOfWeek == 6] <- "Saturday"
train_final$DayOfWeek[train_final$DayOfWeek == 7] <- "Sunday"

train_final$DayOfWeek <- as.factor(train_final$DayOfWeek)
train_final$StateHoliday <- as.factor(train_final$StateHoliday)
train_final$StoreType <- as.factor(train_final$StoreType)
train_final$Assortment <- as.factor(train_final$Assortment)
train_final$PromoInterval <- as.factor(train_final$PromoInterval)

train_final$Promo <- as.logical(train_final$Promo)
train_final$SchoolHoliday <- as.logical(train_final$SchoolHoliday)
train_final$Promo2 <- as.logical(train_final$Promo2)

# Test_final Pre-Processing

test_final$Date <- as.Date(test_final$Date, format="%d/%m/%Y")

# Make sure all variables are appropriate types
test_final$DayOfWeek[test_final$DayOfWeek == 1] <- "Monday"
test_final$DayOfWeek[test_final$DayOfWeek == 2] <- "Tuesday"
test_final$DayOfWeek[test_final$DayOfWeek == 3] <- "Wednesday"
test_final$DayOfWeek[test_final$DayOfWeek == 4] <- "Thursday"
test_final$DayOfWeek[test_final$DayOfWeek == 5] <- "Friday"
test_final$DayOfWeek[test_final$DayOfWeek == 6] <- "Saturday"
test_final$DayOfWeek[test_final$DayOfWeek == 7] <- "Sunday"

test_final$DayOfWeek <- as.factor(test_final$DayOfWeek)
test_final$StateHoliday <- as.factor(test_final$StateHoliday)
test_final$StoreType <- as.factor(test_final$StoreType)
test_final$Assortment <- as.factor(test_final$Assortment)
test_final$PromoInterval <- as.factor(test_final$PromoInterval)

test_final$Promo <- as.logical(test_final$Promo)
test_final$SchoolHoliday <- as.logical(test_final$SchoolHoliday)
test_final$Promo2 <- as.logical(test_final$Promo2)

test_final$Sales <- as.numeric(test_final$Sales)
test_final$Customers <- as.numeric(test_final$Customers)

# Exploratory Data Analysis
# Univariate Statistics for train_final
descr(train_final)
freq(train_final)
summary(train_final) 
sapply(train_final, function(x) sum(is.na(x)))

# Univariate Statistics for test_final
descr(test_final)
freq(test_final)
summary(test_final) 
sapply(test_final, function(x) sum(is.na(x)))


# Histograms
ggplot(data=train_final, aes(Sales)) + 
  geom_histogram(bins=100, fill='steelblue')

# Sales against date in train_final
ggplot(data = train_final, aes(x = Date, y = Sales)) + 
  geom_density2d_filled()

# Sales versus attributes (Scatter plots)
ggplot(data = train_final, aes(x =CompetitionDistance, y = Sales)) + 
  geom_point(size = .5, alpha = .1)

ggplot(data = train_final, aes(x =CompetitionMonthsOpen, y = Sales)) + 
  geom_point(size = .5, alpha = .1)

ggplot(data = train_final, aes(x =PromoWeeksOn, y = Sales)) + 
  geom_point(size = .5, alpha = .1)

# Density plots by categorical variables
qplot(Sales,data=train_final,geom="density",color=DayOfWeek)

qplot(Sales,data=train_final,geom="density",color=Promo)

qplot(Sales,data=train_final,geom="density",color=StateHoliday)

qplot(Sales,data=train_final,geom="density",color=SchoolHoliday)

qplot(Sales,data=train_final,geom="density",color=StoreType)

qplot(Sales,data=train_final,geom="density",color=Assortment)

qplot(Sales,data=train_final,geom="density",color=Promo2)

qplot(Sales,data=train_final,geom="density",color=PromoInterval)


# Correlation matrix
cor(train_final[, c('Sales', 'Customers', 'CompetitionDistance', 
                    'CompetitionMonthsOpen', 'PromoWeeksOn')])

# Set random seed
set.seed(123)

#define model with all predictors
all <- lm(Sales ~ ., data=train_final)
vif(all)
imcdiag(all) # Using vif as measure of co linearity, we can see that promointerval
# and promo2 are too colinear in our model.

# Make predictions > new model with all variables apart from customers and promointerval
model1 <- lm(Sales ~ DayOfWeek+Promo+StateHoliday+SchoolHoliday+StoreType+
              Assortment+CompetitionDistance+Promo2+CompetitionMonthsOpen+
              PromoWeeksOn, data=train_final)
vif(model1) # model now has acceptable vif!

# Feature selection > model building 
# Define intercept-only model
intercept_only <- lm(Sales ~ 1, data=train_final)
summary(intercept_only)

# Forward and backwards stepwise regression based on AIC
bestsubset <- stepAIC(intercept_only, direction='both', scope=formula(model1))
summary(bestsubset) # This has chosen same configuration of variables as previously

vif(bestsubset) # Passes multicolinearity check

rmse_intercept <- sqrt(mean(intercept_only$residuals^2))
print(rmse_intercept) # RMSE for intercept only model

rmse <- sqrt(mean(bestsubset$residuals^2))
print(rmse) # RMSE for final model.

# Exporting regression table
stargazer(intercept_only, bestsubset, type="html")

# Predict sales values on the test data 
predicted_sales_test <- predict(bestsubset, newdata = test_final)
test_final$Sales <- predicted_sales_test # Importing values into dataframe

train_final$predicted_train <- predict(bestsubset) # Predicted values for train dataset

# Statistics for comparison of actual values, prediction on train and prediction
# on test data
summary(test_final$Sales) # Similar mean, median and IQ range to actual train values
summary(train_final$Sales) 
summary(train_final$predicted_train) # Again similar as above 

# Plot predicted sales against date for test
ggplot(data = test_final, aes(x = Date, y = Sales)) + 
  geom_density_2d_filled()
