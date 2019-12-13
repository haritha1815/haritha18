startups = read.csv(file.choose())
# custom normalization function
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}

# apply normalization to entire data frame
startups_norm <- as.data.frame(startups)


# create training and test data
startups_train <- startups_norm[1:773, ]
startups_test <- startups_norm[774:1030, ]

## Training a model on the data ----
install.packages("neuralnet")
library(neuralnet)

# simple ANN with only a single hidden neuron
attach(startups)
startups_model <- neuralnet(Profit~R.D.Spend+Administration
                            +Marketing.Spend+State,data = Startups_train)
str(startups_model)


# visualize the network topology
plot(startups_model)

# obtain predicted  values
str(results_model)
predicted_startups <- results_model$net.result

# examine the correlation between predicted and actual values
cor(predicted_startups, startups_test$Profit)

## Improving model performance 
startups_model2 <- neuralnet(Profit~R.D.Spend+Administration
                             +Marketing.Spend+State, hidden = 10)


# plot the network
plot(startups_model2)

startups_results2 <- compute(startups_model2, startups_test[1:8])
predicted_strength2 <- model_results2$net.result
cor(predicted_strength2, concrete_test$strength)
