glass = read.csv()
colnames(glass)
glass <- glass[-1]

table(glass$type)

str(zoo)
glass$type <- factor(glass$tpe, levels = c("B", "M"),
                     labels = c("Benign", "Malignant"))

# table or proportions with more informative labels
round(prop.table(table(glass$type)) * 100, digits = 2)

# summarize any three numeric features
summary(glass[c("radius_mean", "area_mean", "smoothness_mean")])

# create normalization function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# test normalization function - result should be identical
normalize(c(0.01, 0.02, 0.03, 0.04, 0.05))
normalize(c(10, 20, 30, 40, 50))

glass_n <- as.data.frame(lapply(glass[2:31], normalize))

# confirm that normalization worked
summary(glass_n$area_mean)

# create training and test data
glass_train <- glass_n[1:469, ]
glass_test <- glass_n[470:569, ]

# create labels for training and test data

glass_train_labels <- glass[1:469, 1]
glass_test_labels <- glass[470:569, 1]
#---- Training a model on the data ----

# load the "class" library
install.packages("class")
library(class)

glass_test_pred <- knn(glass = glass_train, test = glass_test,
                     cl = zoo_train_labels, k=21)



##--------Evaluating model performance ----

# load the "gmodels" library
library(gmodels)

# Create the cross tabulation of predicted vs. actual
CrossTable(x = glass_test_labels, y = zoo_test_pred,
           prop.chisq=FALSE)
## Improving model performance ----

# use the scale() function to z-score standardize a data frame
glass_z <- as.data.frame(scale(glass[-1]))

# confirm that the transformation was applied correctly
summary(glass_z$area_mean)

# create training and test datasets
glass_train <- glass_z[1:469, ]
glass_test <- glass_z[470:569, ]


# re-classify test cases
glass_test_pred <- knn(train = glass_train, test = glass_test,
                     cl = glass_train_labels, k=21)

# Create the cross tabulation of predicted vs. actual
CrossTable(x = glass_test_labels, y = glass_test_pred,
           prop.chisq=FALSE)

# try several different values of k
glass_train <- glass_n[1:469, ]
glass_test <- glass_n[470:569, ]

glass_test_pred <- knn(train = glass_train, test = glass_test, cl = glass_train_labels, k=1)
CrossTable(x = glass_test_labels, y = glass_test_pred, prop.chisq=FALSE)

glass_test_pred <- knn(train = glass_train, test = glass_test, cl = glass_train_labels, k=5)
CrossTable(x = glass_test_labels, y = glass_test_pred, prop.chisq=FALSE)

glass_test_pred <- knn(train = glass_train, test =glass_test, cl = glass_train_labels, k=11)
CrossTable(x = glass_test_labels, y = glass_test_pred, prop.chisq=FALSE)

glass_test_pred <- knn(train = glass_train, test = glass_test, cl = glass_train_labels, k=15)
CrossTable(x = glass_test_labels, y = glass_test_pred, prop.chisq=FALSE)

glass_test_pred <- knn(train = glass_train, test = glass_test, cl = glass_train_labels, k=21)
CrossTable(x = glass_test_labels, y = glass_test_pred, prop.chisq=FALSE)

glass_test_pred <- knn(train = glass_train, test = glass_test, cl = glass_train_labels, k=27)
CrossTable(x = glass_test_labels, y = glass_test_pred, prop.chisq=FALSE)
