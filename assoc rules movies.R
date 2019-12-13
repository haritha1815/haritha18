my_movies <- read.csv(file.choose())
# phone_data1 <- readLines(file.choose()) # if we use readLines functions more lines will be reduced
View(my_movies)
str(my_movies)

# converting everything into character format 
my_movies[] <- lapply(my_movies,as.character)
View(my_movies)
# Creating a custom fucntion to collapse all the items in a transaction into  a single sentence 
paste_fun <- function(i){
   return (paste(as.character(i),collapse=" "))
}

# Applying the custom function
my_movies["new_col"] <- apply(my_movies,1,paste_fun)
View(my_movies)

install.packages("tm")
# tm package 
library(tm)
x <- Corpus(VectorSource(my_movies$Gladiator))
# contains all items of a transaction in a single sentence
x <- tm_map(x,stripWhitespace)
# Creating a TDM matrix
movies0 <- t(TermDocumentMatrix(x))
# Converting TDM matrix to data frame
movies_df <- data.frame(as.matrix(movies0))
View(movies_df)

library(arules)
library(arulesViz)
# Item Frequecy plot
windows()
# count of each item from all the transactions 
barplot(sapply(movies_df,sum),col=1:10)

# Applying apriori algorithm to get relevant rules
rules <- apriori(as.matrix(dtm0_df),parameter = list(support=0.002,confidence=0.5,minlen=2))
inspect(rules)
plot(rules)

# Sorting rules by confidence 
rules_conf <- sort(rules,by="confidence")
inspect(rules_conf)
# Sorint rules by lift ratio
rules_lift <- sort(rules,by="lift")
inspect(rules_lift)

# Visualizing rules in scatter plot


plot(rules,method = "graph")

