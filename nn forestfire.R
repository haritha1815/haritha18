forestfire = read.csv(file.choose())
# custom normalization function
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}

# apply normalization to entire data frame
forestfire_norm <- as.data.frame(lapply(forestfire, normalize))

# create training and test data
forestfire_train <- forestfire_norm[1:773, ]
forestfire_test <- forestfire_norm[774:1030, ]

## Training a model on the data 
library(neuralnet)

# simple ANN with only a single hidden neuron
forestfire_model <- neuralnet(formula = temp~month+ffmc+day+DMC+ISI+RH+wind+rain+area
                              +dayfri+daymon+daysat+daysun+daytues+daywed+monthapr+monthaug
                              +monthjan+monthfeb+monthmarc+monthmay+monthjun+monthjul+monthnov
                              ,data = forestfire)


# visualize the network topology
plot(forestfire_model)

## Evaluating model performance 
  
  results_model <- compute(forestfire_model, forestfire_test)
# obtain predicted strength values
str(results_model)
predicted_strength <- results_model$net.result

# examine the correlation between predicted and actual values
cor(predicted_strength, concrete_test$strength)

## Improving model performance 
forestfire_model2 <- neuralnet(formula = temp~month+ffmc+day+DMC+ISI+RH+wind+rain+area
                               +dayfri+daymon+daysat+daysun+daytues+daywed+monthapr+monthaug
                               +monthjan+monthfeb+monthmarc+monthmay+monthjun+monthjul+monthnov
                               ,data = forestfire, hidden = 10)


# plot the network
plot(forestfire_model2)

# evaluate the results as we did before
model_results2 <- compute(forestfire_model2, forestfire_test[1:8])
predicted_temp2 <- model_results2$net.result
cor(predicted_temp2, forestfire_test$temp)
