
# Datase:  ionosphere dataset
#
# 
# Problem: Determine if a given signal is Good (g) or Bad (b).
# Good RADAR returns a nice structure, BAD RADAR do not return good
# structures.


# Load libraries
library(mlbench) # ML benchmark problems with UCI repository, for dataset
library(caret) # Classification and Regresssion training
library(caretEnsemble) # For ensembles of caret models

# Load the dataset
data(Ionosphere)
dataset <- Ionosphere
dataset <- dataset[,-2]
dataset$V1 <- as.numeric(as.character(dataset$V1))


head(dataset)

control <- trainControl(method="cv", number=10, repeats=3) # Cross validation
seed <- 7
metric <- "Accuracy"

# C5.0
set.seed(seed)
fit.c50 <- train(Class~., data=dataset, method="C5.0", metric=metric, trControl=control)

# Stochastic Gradient Boosting
set.seed(seed)
fit.gbm <- train(Class~., data=dataset, method="gbm", metric=metric, trControl=control)

# summarize results
boosting_results <- resamples(list(c5.0=fit.c50, gbm=fit.gbm))
summary(boosting_results)
dotplot(boosting_results)


# We can see that the C5.0 algorithm produces a more accurate model with an accuracy of 94.58%.

