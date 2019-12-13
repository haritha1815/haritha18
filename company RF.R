# Using Random Forest
install.packages("randomForest")
library(randomForest)
#########train data##################
mydata<-read.csv(file.choos())
colnames(mydata)

Sales_Result <- NULL
Sales_Result <- ifelse(mydata$Sales > 7.9,1,0)
mydata[,"Sales_Result"] <- Sales_Result

mydata$ShelveLoc <- as.factor(mydata$ShelveLoc)
mydata$Urban <- as.factor(mydata$Urban)
mydata$US <- as.factor(mydata$US)
mydata$Sales_Result <- as.factor(mydata$Sales_Result)

sales_high <- mydata[mydata$Sales_Result == "1",] 
sales_low <- mydata[mydata$Sales_Result == "0",]

data_train <- rbind(sales_high[1:130,], sales_low[1:130,])
data_test <- rbind(sales_high[131:199,], sales_low[131:201,])

# Building a random forest model on training data 
fit.forest <- randomForest(Sales_Result~.,data=data_train, na.action=na.roughfix,importance=TRUE)

# Training accuracy 
mean(data_train$Sales_Result == predict(fit.forest,data_train))


pred_train <- predict(fit.forest,data_train)
library(caret)
confusionMatrix(data_train$Sales_Result, pred_train)


# Predicting test data 
pred_test <- predict(fit.forest,newdata=data_test)
mean(pred_test == data_test$Sales_Result) 

confusionMatrix(data_test$Sales_Result, pred_test)

plot(fit.forest,lwd=2)
legend("topright", colnames(fit.forest$err.rate),col=1:4,cex=0.8,fill=1:4)