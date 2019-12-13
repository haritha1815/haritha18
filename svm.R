salary = read.csv(file.choose())
salary_train <- read.csv(file.choose())
salary_test <- read.csv(file.choose())

##############training
install.packages("kernlab")
library(kernlab)
salary_classifier <- ksvm(salary_train ~.,data = salary_train, kernel = "vanilladot")
##########testing
salary_predictions <- predict(salary_classifier, salary_test)

head(salary_predictions)

table(salary_predictions, salary_test$salary)


data <- salary_predictions == salary_test$salary
table(data())
prop.table(table(data))


salary_classifier_rbf <- ksvm(salary ~ ., data = salary_train, kernel = "rbfdot")
salary_predictions_rbf <- predict(salary_classifier_rbf, salary_test)

data_rbf <- salary_predictions_rbf == salary_test$letter
table(data_rbf)
prop.table(table(data_rbf))
