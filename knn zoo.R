zoo = read.csv()
colnames(zoo)
zoo <- zoo[-1]

table(zoo$animal)

str(zoo)
zoo$animal <- factor(zoo$animal, levels = c("B", "M"),
                         labels = c("Benign", "Malignant"))

# table or proportions with more informative labels
round(prop.table(table(zoo$animal)) * 100, digits = 2)

# summarize any three numeric features
summary(zoo[c("radius_mean", "area_mean", "smoothness_mean")])

# create normalization function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# test normalization function - result should be identical
normalize(c(0.01, 0.02, 0.03, 0.04, 0.05))
normalize(c(10, 20, 30, 40, 50))

zoo_n <- as.data.frame(lapply(zoo[2:31], normalize))

# confirm that normalization worked
summary(zoo_n$area_mean)

# create training and test data
zoo_train <- zoo_n[1:469, ]
zoo_test <- zoo_n[470:569, ]

# create labels for training and test data

zoo_train_labels <- zoo[1:469, 1]
zoo_test_labels <- zoo[470:569, 1]
#---- Training a model on the data ----

# load the "class" library
install.packages("class")
library(class)

zoo_test_pred <- knn(train = zoo_train, test = zoo_test,
                      cl = zoo_train_labels, k=21)



##--------Evaluating model performance ----

# load the "gmodels" library
library(gmodels)

# Create the cross tabulation of predicted vs. actual
CrossTable(x = zoo_test_labels, y = zoo_test_pred,
           prop.chisq=FALSE)
## Improving model performance ----

# use the scale() function to z-score standardize a data frame
zoo_z <- as.data.frame(scale(zoo[-1]))

# confirm that the transformation was applied correctly
summary(zoo_z$area_mean)

# create training and test datasets
zoo_train <- zoo_z[1:469, ]
zoo_test <- zoo_z[470:569, ]


# re-classify test cases
zoo_test_pred <- knn(train = zoo_train, test = zoo_test,
                      cl = zoo_train_labels, k=21)

# Create the cross tabulation of predicted vs. actual
CrossTable(x = zoo_test_labels, y = zoo_test_pred,
           prop.chisq=FALSE)

# try several different values of k
zoo_train <- zoo_n[1:469, ]
zoo_test <- zoo_n[470:569, ]

for (i in seq(4,400,3))
{
zoo_test_pred <- knn(train = zoo_train, test = zoo_test, cl = zoo_train_labels, k=1)
zoo_test_pred <- knn(train = zoo_train, test = zoo_test, cl = zoo_train_labels, k=5)
zoo_test_pred <- knn(train = zoo_train, test =zoo_test, cl = zoo_train_labels, k=11)
zoo_test_pred <- knn(train = zoo_train, test = zoo_test, cl = zoo_train_labels, k=15)
zoo_test_pred <- knn(train = zoo_train, test = zoo_test, cl = zoo_train_labels, k=21)
zoo_test_pred <- knn(train = zoo_train, test = zoo_test, cl = zoo_train_labels, k=27)
}