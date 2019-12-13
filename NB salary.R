install.packages("naivebayes")
library(naivebayes)
install.packages("e1071")
library(e1071)
train_sal <- read.csv(file.choose())
str(train_sal)
View(train_sal)
train_sal$educationno <- as.factor(train_sal$educationno)
class(train_sal)
Model <- naiveBayes(train_sal$Salary ~ ., data = train_sal)

Model_pred <- predict(Model,train_sal)
library(gmodels)
CrossTable(Model_pred, train_sal$Salary,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))
mean(Model_pred==train_sal$Salary) 


install.packages("naivebayes")
library(naivebayes)
install.packages("e1071")
library(e1071)
test_sal <- read.csv(file.choose())
View(test_sal)
str(test_sal)
test_sal$educationno <- as.factor(test_sal$educationno)
class(test_sal)
Model <- naiveBayes(test_sal$Salary ~ ., data = test_sal)
Model
Model_pred <- predict(Model,test_sal)
library(gmodels)
CrossTable(Model_pred, test_sal$Salary,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))
mean(Model_pred==test_sal$Salary)