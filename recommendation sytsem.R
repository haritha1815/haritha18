
install.packages("recommenderlab")
install.packages("Matrix")
library("recommenderlab")
library(caTools)
??caTools
#books rating
books_rating <- read.csv(file.choose())
View(books_rating)

#metadata about the variable
str(books_rating)


#rating distribution
hist(books_rating$rating)

#the datatype should be realRatingMatrix inorder to build recommendation engine
books_rating_matrix <- as(books_rating, 'realRatingMatrix')

#Popularity based 

books_rating_recomm_model1 <- Recommender(books_rating_matrix, method="POPULAR")


#Predictions for two users 
recommended_items1 <- predict(books_rating_recomm_model1, books_rating_matrix[413:475], n=5)
as(recommended_items1, "list")



#User Based Collaborative Filtering

books_recomm_model2 <- Recommender(books_rating_matrix, method="UBCF")

#Predictions for two users 
recommended_items2 <- predict(books_recomm_model2, books_rating_matrix[413:414], n=5)
as(recommended_items2, "list")

