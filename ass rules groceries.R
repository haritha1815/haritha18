library(arules)
groceries<-read.transactions(file.choose(),format="basket")
inspect(groceries[1:10])
class(groceries)
# itemFrequencyPlot can be applicable only for transaction data 
# count of each item from all the transactions 
itemFrequencyPlot(groceries,topN=20)
groceries_rules<-apriori(groceries,parameter = list(support = 0.002,confidence = 0.05,minlen=3))

library(arulesViz)
plot(groceries_rules,method = "scatterplot")
plot(groceries_rules,method = "grouped")
plot(groceries_rules,method = "graph")
plot(groceries_rules,method = "mosaic")


### On inbuilt Data set #####
library(arules)
data("Groceries")
summary(Groceries)
inspect(Groceries[1:10])
rules <- apriori(Groceries,parameter = list(support = 0.002,confidence = 0.05,minlen=5))
inspect(rules[1:5])
windows()
plot(rules,method = "scatterplot")
plot(rules,method = "grouped")
plot(rules,method = "graph")
plot(rules,method = "mosaic")

rules <- sort(rules,by="lift")

inspect(rules[1:4])
