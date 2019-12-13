setwd()
forest<-read.csv(file.choose())
View(letters)
colnames(letters)<- c("month,day,FFMC	,DMC	,DC	,ISI	,temp	,RH,wind	,rain	,Size")
View(letters)
write.csv(letters,file="forest.csv",col.names = F,row.names = F)

forest_train <- forest[1:16000, ]
forest_test  <- forest[16001:20000, ]

##Training a model on the data
library(kernlab)
forest_classifier <- ksvm(forest ~ ., data = forest_train,
                          kernel = "vanilladot")
# predictions on testing dataset
forest_predictions <- predict(forest_classifier, forest_test)

head(forest_predictions)

table(forest_predictions, forest_test$letter)


fire<- forest_predictions == forest_test$letter
table(fire)
prop.table(table(fire))


## Improving model performance ----
forest_classifier_rbf <- ksvm(forest ~ ., data = forest_train, kernel = "rbfdot")
forest_predictions_rbf <- predict(forest_classifier_rbf, forest_test)

forest_rbf <- forest_predictions_rbf == forest_test$forest
table(fire_rbf)
prop.table(table(forest_rbf))

# kernal = besseldot
model_besseldot<-ksvm(forest ~.,data = forest_train,kernel = "besseldot")
pred_bessel<-predict(model_besseldot,newdata=forest_test)
mean(pred_bessel==forest_test$lettr)

# kernel = polydot

model_poly<-ksvm(forest ~.,data = forest_train,kernel = "polydot")
pred_poly<-predict(model_poly,newdata = forest_test)
mean(pred_poly==forest_test$lettr) # 83.925



