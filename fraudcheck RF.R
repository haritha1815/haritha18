# Using Random Forest
install.packages("randomForest")
library(randomForest)
#########train data##################
mydata<-read.csv(file.choose())
Risky <- NULL
Risky <- ifelse(mydata$Taxable.Income<=30000,1,0)
mydata[,"Risky"] <- Risky
mydata$Undergrad <- as.factor(mydata$Undergrad)
mydata$Marital.Status <- as.factor(mydata$Marital.Status)
mydata$Urban <- as.factor(mydata$Urban)
mydata$Risky <- as.factor(mydata$Risky)

fraud_risky <- mydata[mydata$Risky == "1",] 
fraud_not_risky <- mydata[mydata$Risky == "0",]

# Prediction of train data
data_train <- rbind(fraud_risky[1:89,], fraud_not_risky[1:357,])

# Building a random forest model on training data 
fit.forest <- randomForest(Risky~.,data=data_train, na.action=na.roughfix,importance=TRUE)

########### Training accuracy 
mean(data_train$Risky == predict(fit.forest,data_train))
pred_train <- predict(fit.forest,data_train)
install.packages("caret")
library(caret)


# Confusion Matrix
confusionMatrix(data_train$Risky, pred_train)


# Predicting test data 
pred_test <- predict(fit.forest,newdata=data_test)
mean(pred_test == data_test$Risky)


# Confusion Matrix 

confusionMatrix(pred_test$Risky, pred_test)

# Visualization 
plot(fit.forest,lwd=2)
legend("topright", colnames(fit.forest$err.rate),col=1:4,cex=0.8,fill=1:4)




