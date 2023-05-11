#Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(caret)
library(haven)

# Data Import and Cleaning
predict_dataset<- read.csv("../data/combined_dataset.csv")

# Analysis
# Create training and testing datasets using the sampled indices
train_cases <- sample(1:nrow(predict_dataset))
train_tbl <- predict_dataset[train_cases, ]
test_tbl <- predict_dataset[-train_cases, ]

# Identify factor variables with only one level
single_level_factors <- sapply(predict_dataset, function(x) is.factor(x) && length(levels(x)) < 2)

# Remove those single-level factor variables from the dataset
predict_dataset <- predict_dataset[, !single_level_factors]
# Create 10-fold cross-validation indices for the training dataset
training_folds <- createFolds(train_tbl$Attrition,
                              k=10)
# Dummy coding the Attrition variable (convert "Yes" to 1 and "No" to 0)
train_tbl$Attrition <- ifelse(train_tbl$Attrition == "Yes", 1, 0)

# Identify columns with one level
single_level_cols <- sapply(train_tbl, function(col) length(unique(col)) == 1)
# Convert single-level columns to factors with two levels
train_tbl[single_level_cols] <- lapply(train_tbl[single_level_cols], factor, levels = c("level1", "level2"))

# Train a linear regression model (Model 1)
model1 <- train(
  Attrition ~ .,
  train_tbl, 
  method="lm",
  na.action=na.pass,
  preProcess=c("center", "scale", "nzv", "medianImpute"),
  trControl=trainControl(method="cv", number=10, indexOut=training_folds, verboseIter=T) 
)
model1

# Train a generalized linear model with elastic net regularization (Model 2)
model2 <- train(
  Attrition ~ .,
  train_tbl, 
  method="glmnet",
  tuneLength=3,
  na.action=na.pass,
  preProcess=c("center", "scale", "nzv", "medianImpute"),
  trControl=trainControl(method="cv", number=10, indexOut=training_folds, verboseIter=T) 
)
model2

# Train a random forest model (Model 3)
model3 <- train(
  Attrition ~ .,
  train_tbl, 
  method="ranger",
  tuneLength=3,
  na.action=na.pass,
  preProcess=c("center", "scale", "nzv", "medianImpute"),
  trControl=trainControl(method="cv", number=10, indexOut=training_folds, verboseIter=T) 
)
model3

# Train an XGBoost model (Model 4)
model4 <- train(
  Attrition ~ .,
  train_tbl, 
  method="xgbTree",
  tuneLength=3,
  na.action=na.pass,
  preProcess=c("center", "scale", "nzv", "medianImpute"),
  trControl=trainControl(method="cv", number=10, indexOut=training_folds, verboseIter=T) 
)
model4

# Publication
#create summary table and plot
summary(resamples(list(model1, model2, model3, model4)))
resample_sum <- summary(resamples(list(model1, model2, model3, model4)))
dotplot(resamples(list(model1, model2, model3, model4)))
# Get the mean values of each metric for each model
mean_MAE <- resample_sum $statistics$MAE[, "Mean"]
mean_RMSE <- resample_sum $statistics$RMSE[, "Mean"]
mean_Rsquared <- resample_sum$statistics$Rsquared[, "Mean"]
# Create the model_comparison data frame
model_comparison <- data.frame(
  Model = c("Model1", "Model2", "Model3", "Model4"),
  Mean_MAE = mean_MAE,
  Mean_RMSE = mean_RMSE,
  Mean_Rsquared = mean_Rsquared
)
# Set the row names of the data frame to the model names
rownames(model_comparison) <- model_comparison$Model
# Remove the "Model" column for simplicity and clarity
model_comparison <- model_comparison[, -1]
# Display the modified model_comparison table
print(model_comparison)

#What characteristics of how you created the final model likely made the biggest 
#impact in maximizing its performance? How do you know? 
#XGBoost is a powerful ensemble method that combines multiple decision trees 
#and can handle complex relationships between features and the target variable. 
#This improved performance is reflected in the table.Based on the results in the table, 
#Model4 (XGBoost) has the best predicted performance.This is because Model4 has the lowest 
#Mean Absolute Error (Mean_MAE=0.09) and Mean RootMean Squared Error (RMSE=0.15) values, indicating better prediction accuracy. 
#Additionally,the Mean R-squared value of Model4 is pretty big, indicating that it 
#can explain the large proportion of variance in the dependent variable (Attrition) 
#compared to the other models. In addition, xgbTree (XGBoost) have better ability to
#handle missing data and imbalanced datasets.In addtion,XGBoost can well prevent overfitting problem.

#Part 2-2:What is the incremental predictive accuracy gained by including text 
#data in your model versus not including text data?
# Identify columns with less than two unique values
low_var_cols <- sapply(train_tbl, function(col) length(unique(col)) < 2)

# Remove those columns with less than two unique values
#note: I deleted columns with less than two unique values since contrasts can be 
#applied only to factors with 2 or more levels. Otherwise, it will throw me error. 
train_tbl <- train_tbl[, !low_var_cols]

# Model without text predictors
model_no_text <- train(
  Attrition ~ . - bad - good,
  train_tbl,
  method="glmnet",
  tuneLength=3,
  na.action=na.pass,
  preProcess=c("center", "scale", "nzv", "medianImpute"),
  trControl=trainControl(method="cv", number=10, indexOut=training_folds, verboseIter=T)
)
model_no_text

# Model with text predictors
model_with_text <- train(
  Attrition ~ .,
  train_tbl,
  method="glmnet",
  tuneLength=3,
  na.action=na.pass,
  preProcess=c("center", "scale", "nzv", "medianImpute"),
  trControl=trainControl(method="cv", number=10, indexOut=training_folds, verboseIter=T)
)
model_with_text

# Publication
#create summary table and plot
summary(resamples(list(model_no_text, model_with_text)))
resample_sum <- summary(resamples(list(model_no_text, model_with_text)))
dotplot(resamples(list(model_no_text, model_with_text)))
# Get the mean values of each metric for each model
mean_MAE <- resample_sum $statistics$MAE[, "Mean"]
mean_RMSE <- resample_sum $statistics$RMSE[, "Mean"]
mean_Rsquared <- resample_sum$statistics$Rsquared[, "Mean"]
# Create the model_comparison data frame
model_comparison <- data.frame(
  Model = c("Model_no_text", "model_with_text" ),
  Mean_MAE = mean_MAE,
  Mean_RMSE = mean_RMSE,
  Mean_Rsquared = mean_Rsquared
)
# Set the row names of the data frame to the model names
rownames(model_comparison) <- model_comparison$Model
# Remove the "Model" column for simplicity and clarity
model_comparison <- model_comparison[, -1]
# Display the modified model_comparison table
print(model_comparison)

#Since the Mean Mean Absolute Error (MAE), mean RMSE and mean R square are almost the same for both models,
#the differences in predictive accuracy between the model with text data and the model without text data are very small.
#This suggests that including the text data doesn't contribute much to improving the predictive accuracy of the model.
