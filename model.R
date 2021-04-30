# Title     : model
# Objective : Neural Network for regression
# Created by: szpet
# Created on: 4/30/2021
library(keras)

df = read.csv('results.csv')
data_onshot <- df

split <- rsample::initial_split(data_onshot,prop = .7, strata = 'PFS_time_months')
print(split)
train <- rsample::training(split)
test <- rsample::testing(split)
train_x <- train %>% dplyr::select(-PFS_time_months)

test_x <- test %>% dplyr::select(-PFS_time_months)


# Create & transform response sets
train_y <- train$PFS_time_months
test_y  <- test$PFS_time_months

zv <- which(colSums(is.na(train_x)) > 0, useNames = FALSE)
train_x <- train_x[, -zv]
test_x  <- test_x[, -zv]
dim(train_x)
dim(test_x)
