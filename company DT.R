install.packages("C50")
install.packages("tree")
library(C50)

# Building model on training data 
company_train <- C5.0(company_train[,-5],company_train$Salary)
windows()
plot(company_train) # Tree graph
# Training accuracy
pred_train <- predict(company_train,company_train)

mean(company_train$Salary==pred_train) # 97.33% Accuracy

library(caret)
confusionMatrix(pred_train,company_train$Salary)

company_test <- predict(company_train,newdata=company_test) # predicting on test data
mean(pred_test==company_test$Salary) # 94.66% accuracy 
confusionMatrix(pred_test,company_test$Salary)
library(gmodels)
# Cross tablez
CrossTable(company_test$Salary,predc_test)

##### Using tree function 
library(tree)
# Building a model on training data 
company_tree <- tree(Salary~.,data=company_train)
plot(company_tree)
text(company_tree,pretty = 0)

# Predicting the test data using the model
pred_tree <- as.data.frame(predict(company_tree,newdata=company_test))
pred_tree["final"] <- NULL
pred_test_df <- predict(comapny_tree,newdata=company_test)

pred_tree$final <- colnames(pred_test_df)[apply(pred_test_df,1,which.max)]


mean(pred_tree$final==company_test$salary) # Accuracy = 94.66%
CrossTable(company_test$Salary,pred_tree$final)

# Simple model
model_cart1 <- rpart(MPG~.,data=salary,method="anova")
plot(model_cart1)
text(model_cart1)
summary(model_cart1)
pred <- predict(model_cart1,salary)


Adjusted_RSqred <- function(pred, obs, formula = "corr", na.rm = FALSE) {
  n <- sum(complete.cases(pred))
  switch(formula,
         corr = cor(obs, pred, use = ifelse(na.rm, "complete.obs", "everything"))^2,
         traditional = 1 - (sum((obs-pred)^2, na.rm = na.rm)/((n-1)*var(obs, na.rm = na.rm))))
}

Adjusted_RSqred(pred_mpg,company$sales) # 0.8484

plot(pred,company$sales)
cor(pred,company$sales) 
