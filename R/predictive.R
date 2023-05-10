#Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(caret)
library(haven)

# Read the dataset from the CSV file
predict_dataset<- read.csv("../data/combined_dataset.csv")

train_cases <- sample(1:nrow(predict_dataset))

gss_train_tbl <- predict_dataset[train_cases, ]
gss_test_tbl <- predict_dataset[-train_cases, ]

# Identify factor variables with only one level
single_level_factors <- sapply(predict_dataset, function(x) is.factor(x) && length(levels(x)) < 2)

# Remove those variables
predict_dataset <- predict_dataset[, !single_level_factors]


training_folds <- createFolds(gss_train_tbl$Attrition,
                              k=10)
#dummy coding 
gss_train_tbl$Attrition <- ifelse(gss_train_tbl$Attrition == "Yes", 1, 0)

# Identify columns with one level
single_level_cols <- sapply(gss_train_tbl, function(col) length(unique(col)) == 1)

# Convert single-level columns to factors with two levels
gss_train_tbl[single_level_cols] <- lapply(gss_train_tbl[single_level_cols], factor, levels = c("level1", "level2"))

model1 <- train(
  Attrition ~ .,
  gss_train_tbl, 
  method="lm",
  na.action=na.pass,
  preProcess=c("center", "scale", "nzv", "medianImpute"),
  trControl=trainControl(method="cv", number=10, indexOut=training_folds, verboseIter=T) 
)
model1

model2 <- train(
  Attrition ~ .,
  gss_train_tbl, 
  method="glmnet",
  tuneLength=3,
  na.action=na.pass,
  preProcess=c("center", "scale", "nzv", "medianImpute"),
  trControl=trainControl(method="cv", number=10, indexOut=training_folds, verboseIter=T) 
)
model2

model3 <- train(
  Attrition ~ .,
  gss_train_tbl, 
  method="ranger",
  tuneLength=3,
  na.action=na.pass,
  preProcess=c("center", "scale", "nzv", "medianImpute"),
  trControl=trainControl(method="cv", number=10, indexOut=training_folds, verboseIter=T) 
)
model3

model4 <- train(
  Attrition ~ .,
  gss_train_tbl, 
  method="xgbTree",
  tuneLength=3,
  na.action=na.pass,
  preProcess=c("center", "scale", "nzv", "medianImpute"),
  trControl=trainControl(method="cv", number=10, indexOut=training_folds, verboseIter=T) 
)
model4

summary(resamples(list(model1, model2, model3, model4)))
resample_sum <- summary(resamples(list(model1, model2, model3, model4)))
dotplot(resamples(list(model1, model2, model3, model4)))

# Publication
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
# Remove the "Model" column
model_comparison <- model_comparison[, -1]
# Display the modified model_comparison table
print(model_comparison)

#What characteristics of how you created the final model likely made the biggest 
#impact in maximizing its performance? How do you know? 
#XGBoost is a powerful ensemble method that combines multiple decision trees 
#and can handle complex relationships between features and the target variable. 
#This improved performance is reflected in the table.Based on the results in the table, 
#Model4 has the best predicted performance.This is because Model4 has the lowest 
#Mean Absolute Error (MAE) and Mean RootMean Squared Error (RMSE) values, indicating better prediction accuracy. 
#Additionally, Model4 has the highest Mean R-squared value, indicating that it 
#can explain the largest proportion of variance in the dependent variable (Attrition) 
#compared to the other models.

library(tidyverse)
library(caret)
library(haven)

# Read the dataset from the CSV file
predict_dataset <- read.csv("../data/combined_dataset.csv")

train_cases <- sample(1:nrow(predict_dataset))

gss_train_tbl <- predict_dataset[train_cases, ]
gss_test_tbl <- predict_dataset[-train_cases, ]

# Remove columns with only one level
#single_level_factors <- sapply(predict_dataset, function(x) is.factor(x) && length(levels(x)) < 2)
#predict_dataset <- predict_dataset[, !single_level_factors]

# Create training folds
training_folds <- createFolds(gss_train_tbl$Attrition, k=10)

# Dummy coding
gss_train_tbl$Attrition <- ifelse(gss_train_tbl$Attrition == "Yes", 1, 0)

# Model without text-derived predictors
model_no_text <- train(
  Attrition ~ . - Text_Feature, #edit  later..........
  gss_train_tbl,
  method="glmnet",
  tuneLength=3,
  na.action=na.pass,
  preProcess=c("center", "scale", "nzv", "medianImpute"),
  trControl=trainControl(method="cv", number=10, indexOut=training_folds, verboseIter=T)
)
model_no_text

# Model with text-derived predictors
model_with_text <- train(
  Attrition ~ .,
  gss_train_tbl,
  method="glmnet",
  tuneLength=3,
  na.action=na.pass,
  preProcess=c("center", "scale", "nzv", "medianImpute"),
  trControl=trainControl(method="cv", number=10, indexOut=training_folds, verboseIter=T)
)
model_with_text

