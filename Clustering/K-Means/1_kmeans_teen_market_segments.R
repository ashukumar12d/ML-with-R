
# Given the text of teenagers' Social Networking Service (SNS) pages, 
# it is possible to identify groups that share common interests such 
# as sports, religion, or music.


# Load the necessary libraries 

library(ggplot2)  # For Visulaization
library(lattice)  # Support package for visualization
library(stats)    # for statistical calculations and random number generation. 
library(cluster)  # for plotting clusters with clusplot()
library(fpc)      # for plotting clusters with plotcluster(), validating clusters

# Load Data
teens <- read.csv("snsdata.csv")

str(teens) # Get all the attributes, type, and other information


# 1. Analyse the data

# check for the presence of missing values in any of four variables
# corresponding to users' private information.

table(teens$gradyear, useNA = "ifany") 

table(teens$gender, useNA = "ifany")

summary(teens$age)

summary(teens$friends)

# Check for outliers

boxplot(teens$age, ylab = "Age")
rug(jitter(teens$age), side = 2) # Creates tick marks on side-2 means y-axis
abline(h = mean(teens$age, na.rm = T), lty = 2) # Add verticle, horizontal line to a plot

# 2. Cleaning and preparing the data

# A reasonable range of ages for high school students includes those who are at least 13 years old 
# and not yet 20 years old. Any age value falling outside this range will be treated
# the same as missing data because it is not feasible to trust the age provided.

# Consider only age between 13 and 20, treat all others as Missing values - NA

teens$age <- ifelse(teens$age >= 13 & teens$age < 20, teens$age, NA)
summary(teens$age)

# create dummy variables for female and unknown gender.

teens$female <- ifelse(teens$gender == "F" & !is.na(teens$gender), 1, 0)
teens$no_gender <- ifelse(is.na(teens$gender), 1, 0)

# Imputing missing values - Use average age of each graduation year

# In order to eliminate 5523 missing values on age, 
# I will use an imputation of mean age values, calculated for each specific 
# graduation year.

mean(teens$age, na.rm = TRUE)

aggregate(data = teens, age ~ gradyear, mean, na.rm = TRUE)

ave_age <- ave(teens$age, 
               teens$gradyear, 
               FUN = function(x) mean(x, na.rm = TRUE))

# Fill all the missing age fields with average age
teens$age <- ifelse(is.na(teens$age), ave_age, teens$age)

# 3. Train the model

# Using z-scoe standardization 
# (rescales features such that they have a mean of zero and a standard deviation of one.)
# select only 36 features.

interests <- teens[5:40]  # Interests columns are from 5 to 40 in csv file
interests_z <- as.data.frame(lapply(interests, scale)) # Apply scale function to interests and return as data frame

set.seed(12)  # Seed can take 1 to 2^32 distinct values, Fixing the randomness to reproduce results

# train the model using k-means clustering, use k as 5

teen_clusters <- kmeans(interests_z, 5)

# Plot the clusters

clusplot(interests_z,
         teen_clusters$cluster, 
         color=TRUE,
         col.clus=c(1:5)[unique(teen_clusters$cluster)],
         shade=TRUE,
         labels=5, 
         lines=0, 
         main = "Bivariate Cluster Plot")

plotcluster(interests_z, teen_clusters$cluster)

# Evaluate model performance
# check how many members are there in each cluster. Usually, if the
# groups are too large or too small, then they are not likely to be very useful.

teen_clusters$size

# the third cluster contains 21386 users, which is quite a lot compared to other clusters.

teen_clusters$centers

# Analyze model performance

teens$cluster <- teen_clusters$cluster

# Find out the cluster numbers of first 10 teens

teens[1:10, c("cluster", "gender", "age", "friends")]

# Average age of each cluster
aggregate(data = teens, age ~ cluster, mean)

# % female in each cluster 
aggregate(data = teens, female ~ cluster, mean)

# Avg friends for each cluster
aggregate(data = teens, friends ~ cluster, mean)

