#Find the risk bank loan customers

library(C50) # For decision Tree
library(gmodels) # For Cross Table - Evaluataion metrics


# 1. exploring and preparing the data
# 1(a)  Exploring the data
# Explore Data

credit <- read.csv("credit.csv")


# We see the expected 1,000 observations and 17 features, which are a combination of factor and integer data types.
str(credit) # Currency is in German DM - Deutsche Mark


# The checking and savings account balance may prove to be important predictors of
# loan default status. Note that since the loan data was obtained from Germany, the
# currency is recorded in Deutsche Marks (DM).

table(credit$checking_balance)

table(credit$savings_balance)


# The loan amounts ranged from 250 DM to 18,420 DM across terms of 4 to 72 months
# with a median duration of 18 months and an amount of 2,320 DM.

summary(credit$months_loan_duration)

summary(credit$amount)


# The default vector indicates whether the loan applicant was unable to meet the
# agreed payment terms and went into default. A total of 30 percent of the loans in
# this dataset went into default:

# Defaulters - 70% normal, 30% defaulters

str(credit)

# 2. exploring and preparing the data
# 2(b)  Data preparation - creating random training and test datasets
#       90% - Train data, 10% - Test Data


set.seed(123)

# sample() function to select 900 values at random out of the sequence of integers from 1 to 1000.
train_sample <- sample(1000, 900)

# Training Data - 900 lines
credit_train <- credit[train_sample, ]

# Test Data - Remaining lines (- represents the rest of the data)
credit_test <- credit[-train_sample, ]

# Check whether this appears to be a fairly even split or not, train should have 
# about 30 percent of defaulted loans and test data also should have similar % of default loans

prop.table(table(credit_train$default)) #  Gives x/sum(x)

prop.table(table(credit_test$default))  


# C5.0 requires in factor format,  because it is a classification model

credit_train$default<-as.factor(credit_train$default) # Convert labels as factors

# Train the model using C5.0 algorithm
credit_model <- C5.0(credit_train[-17], credit_train$default)


# 3. evaluating model performance using test data
# We use predict() function

credit_pred <- predict(credit_model, credit_test)


CrossTable(credit_test$default, credit_pred, dnn = c('actual default', 'predicted default'))


# 4. Improving the model performance

# It may or may not improve performance

credit_boost10 <- C5.0(credit_train[-17], credit_train$default,
                       trials = 10)
credit_boost10

summary(credit_boost10)

credit_boost_pred10 <- predict(credit_boost10, credit_test)

CrossTable(credit_test$default, credit_boost_pred10,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

