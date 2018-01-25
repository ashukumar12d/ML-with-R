
# Problem: Market Basket Analysis using ARM - Apriori
# Generate association rules for the given transactions

library(arules)

# Open and read transactions
groceries <- read.transactions("groceries.csv", sep = ",")

summary(groceries)

# Print the first 5 transactions
inspect(groceries[1:5])


# view the support level for the first three items in the grocery data:
itemFrequency(groceries[, 1:3])


# Visualizing item support - item frequency plots


# Plot the item frequency with support more than 0.1
itemFrequencyPlot(groceries, support = 0.1)

itemFrequencyPlot(groceries, topN = 20)


# Train the model

# In this case, if I attempt to use the default settings of support = 0.1 and
# confidence = 0.8, we will end up with a set of zero rules:

groceryrules <- apriori(groceries, parameter = list(support =
                                                      0.006, confidence = 0.25, minlen = 2))

groceryrules

# Evaluate the model

summary(groceryrules)

inspect(groceryrules[1:3])

# Sorting the set of association rules
# Depending upon the objectives of the market basket analysis, the most useful rules
# might be the ones with the highest support, confidence, or lift.

inspect(sort(groceryrules, by = "lift")[1:5])

# Taking subsets of association rules
# Suppose that given the preceding rule, the marketing team is excited about the
# possibilities of creating an advertisement to promote berries, which are now in
# season.

berryrules <- subset(groceryrules, items %in% "berries")
inspect(berryrules)

# Saving association rules to a file or data frame
write(groceryrules, file = "groceryrules.csv",
      sep = ",", quote = TRUE, row.names = FALSE)

# convert the rules into an R data frame
groceryrules_df <- as(groceryrules, "data.frame")

str(groceryrules_df)

