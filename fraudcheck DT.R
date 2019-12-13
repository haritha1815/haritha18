install.packages("C50")
install.packages("tree")
library(C50)

# Building model on training data 
fraudcheck_train <- C5.0(fraudcheck_train[,-5],fraudcheck_train$Salary)
windows()
plot(fraudcheck_train) # Tree graph
# Training accuracy
pred_train <- predict(fraudcheck_train,fraudcheck_train)

mean(fraudcheck_train$income==pred_train) # 97.33% Accuracy

library(caret)
confusionMatrix(pred_train,fraudcheck_train$income)

fraudcheck_test <- predict(fraudcheck_train,newdata=fraudcheck_test) # predicting on test data
mean(pred_test==fraudcheck_test$income) # 94.66% accuracy 
confusionMatrix(pred_test,fraudcheck_test$income)
library(gmodels)
# Cross tablez
CrossTable(fraudcheck_test$income,predc_test)

##### Using tree function 
library(tree)
# Building a model on training data 
fraudcheck_tree <- tree(income~.,data=fraudcheck_train)
plot(fraudcheck_tree)
text(fraudcheck_tree,pretty = 0)

# Predicting the test data using the model
pred_tree <- as.data.frame(predict(fraudcheck_tree,newdata=fraudcheck_test))
pred_tree["final"] <- NULL
pred_test_df <- predict(fraudcheck_tree,newdata=fraudcheck_test)

pred_tree$final <- colnames(pred_test_df)[apply(pred_test_df,1,which.max)]


mean(pred_tree$final==fraudcheck_test$income) # Accuracy = 94.66%
CrossTable(fraudcheck_test$income,pred_tree$final)

# Simple model
model_cart1 <- rpart(MPG~.,data=income,method="anova")
plot(model_cart1)
text(model_cart1)
summary(model_cart1)
pred <- predict(model_cart1,income)

Adjusted_RSqred <- function(pred, obs, formula = "corr", na.rm = FALSE) {
  n <- sum(complete.cases(pred))
  switch(formula,
         corr = cor(obs, pred, use = ifelse(na.rm, "complete.obs", "everything"))^2,
         traditional = 1 - (sum((obs-pred)^2, na.rm = na.rm)/((n-1)*var(obs, na.rm = na.rm))))
}

Adjusted_RSqred(pred,company$sales) # 0.8484

plot(pred,fraudcheck$income)
cor(pred,fraudcheck$income) 
