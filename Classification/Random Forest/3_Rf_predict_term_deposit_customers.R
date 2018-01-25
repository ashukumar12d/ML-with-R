
library(randomForest)

## Read data
termCrosssell<-read.csv(file="bank-5000.csv",header = T, sep = ";")

## Explore data frame

names(termCrosssell)

# How much % of target variable has "yes"...
table(termCrosssell$y)/nrow(termCrosssell)
table(termCrosssell$y)

# split the data sample into development(train) and validation(test) samples.

sample.ind <- sample(2, 
                     nrow(termCrosssell),
                     replace = T)

train <- termCrosssell[sample.ind==1,] # 1st sample for train
test <- termCrosssell[sample.ind==2,] # 2nd sample for test/validation 


# Check whether Both development and validation samples have similar target variable distribution.

table(train$y)/nrow(train)

table(test$y)/nrow(test)

class(train$y)

# Make formaula of all the features

varNames <- names(train)

# Exclude ID or Response variable
varNames <- varNames[!varNames %in% c("y")]
print(varNames)

# add + sign between exploratory variables
varNames1 <- paste(varNames, collapse = "+")
print(varNames1)

# Add response variable and convert to a formula object
RF_formula <- as.formula(paste("y", varNames1, sep = " ~ "))
print(RF_formula)

# Building Random Forest using R


rf_model <- randomForest(RF_formula,
                              train,
                              ntree=500,
                              importance=T)

#plot(rf_model, log='y')
layout(matrix(c(1,2),nrow=1),
       width=c(4,1)) 
par(mar=c(5,4,4,0)) #No margin on the right side
plot(rf_model, log="y")
par(mar=c(5,0,4,2)) #No margin on the left side
plot(c(0,1),type="n", axes=F, xlab="", ylab="")
legend("top", colnames(rf_model$err.rate),col=1:4,cex=0.8,fill=1:4)

#---------------------------------------------------------------
# Find out the important features
#---------------------------------------------------------------

# Variable importance plot is also a useful tool and can be plotted 
# using varImpPlot function. Top 5 variables are selected and plotted
# based on Model Accuracy and Gini value. We can also get a table with 
# decreasing order of importance based on a measure (1 for model accuracy 
# and 2 node impurity)

# Variable Importance Plot
varImpPlot(rf_model,
           sort = T,
           main="Variable Importance",
           n.var=5)


# Measuer the accuracy of the RF model

# Predicting response variable
train$predicted.response <- predict(rf_model,train)


# Create confusion matrix
library(e1071)
library(caret)

confusionMatrix(data=train$predicted.response,
                reference=train$y,
                positive='yes')

# Predicting response variable
test$predicted.response <- predict(rf_model ,test)

# Create Confusion Matrix
confusionMatrix(data=test$predicted.response,
                reference=test$y,
                positive='yes')


