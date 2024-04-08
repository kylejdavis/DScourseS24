library(tidymodels)
library(glmnet)
library(tidyverse)
library(recipes)
library(readr)


set.seed(123456)

housing <- read_table("http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data", col_names = FALSE)
names(housing) <- c("crim","zn","indus","chas","nox","rm","age","dis","rad","tax","ptratio","b","lstat","medv")

library(rsample)

housing_split <- initial_split(housing, prop = 0.8)
housing_train <- training(housing_split)
housing_test  <- testing(housing_split)

housing_recipe <- recipe(medv ~ ., data = housing_train) %>%
  # convert outcome variable to logs
  step_log(all_outcomes()) %>%
  # convert 0/1 chas to a factor
  step_bin2factor(chas) %>%
  # create interaction term between crime and nox
  step_interact(terms = ~ crim:nox) %>%
  # create square terms of some continuous variables
  step_poly(dis,nox) %>%
  prep()


housing_train_prepped <- housing_recipe %>% juice
housing_test_prepped  <- housing_recipe %>% bake(new_data = housing_test)


housing_train_x <- housing_train_prepped %>% select(-medv)
housing_test_x  <- housing_test_prepped  %>% select(-medv)
housing_train_y <- housing_train_prepped %>% select( medv)
housing_test_y  <- housing_test_prepped  %>% select( medv)


# Define the model specification
lasso_spec <- linear_reg(penalty = tune(), mixture = 1) %>% 
  set_engine("glmnet")

rec <- recipe(medv ~ ., data = housing_train) %>%
  step_log(all_outcomes()) %>%
  step_interact(terms = ~ crim:nox) %>%
  step_poly(dis,nox)

# Define the workflow
lasso_workflow <- workflow() %>%
  add_model(lasso_spec) %>%
  add_recipe(rec)

# Define the resampling scheme
cv_folds <- vfold_cv(housing_train, v = 6)

# Tune the model
lasso_results <- tune_grid(
  lasso_workflow,
  resamples = cv_folds,
  grid = 20  # number of different penalty parameters to try
)

# Find the best model
best_model <- lasso_results %>% select_best(metric = "rmse")

# Fit the model with the optimal λ
lasso_fit <- finalize_model(lasso_spec, best_model) %>% 
  fit(medv ~ ., data = housing_train)

# Predict on the test data
housing_test_pred <- predict(lasso_fit, housing_test)

# Predict on the training data
housing_train_pred <- predict(lasso_fit, housing_train)

# Create a data frame with the observed and predicted values
df_in_sample <- data.frame(observed = housing_train$medv, predicted = housing_train_pred$.pred)

# Calculate the in-sample RMSE
rmse_in_sample <- yardstick::rmse(df_in_sample, observed, predicted)
print(rmse_in_sample)

df <- data.frame(observed = housing_test$medv, predicted = housing_test_pred$.pred)
rmse_val <- yardstick::rmse(df, observed, predicted)
print(rmse_val)

# Extract the optimal lambda
optimal_lambda <- best_model$penalty
print(optimal_lambda)

# Get the dimensions of the training data
dim(housing_train)

# Define the model specification
ridge_spec <- linear_reg(penalty = tune(), mixture = 0) %>%  # mixture = 0 for Ridge
  set_engine("glmnet")

# Define the cross-validation plan
cv_plan <- vfold_cv(housing_train, v = 6)

# Tune the model
ridge_tune_results <- tune_grid(
  ridge_spec,
  medv ~ .,
  resamples = cv_plan,
  grid = 10  # number of lambda values to try
)

# Extract the best model
best_model <- ridge_tune_results %>% select_best("rmse")

# Fit the final model
ridge_fit <- finalize_model(ridge_spec, best_model) %>%
  fit(medv ~ ., data = housing_train)

# Print the final model
ridge_fit

# Extract the optimal lambda
optimal_lambda <- best_model$penalty
print(optimal_lambda)

# Predict on the test data
housing_test_pred <- predict(ridge_fit, new_data = housing_test)

# Create a data frame with the observed and predicted values
df_out_of_sample <- data.frame(observed = housing_test$medv, predicted = housing_test_pred$.pred)

# Calculate the out-of-sample RMSE
rmse_out_of_sample <- yardstick::rmse(df_out_of_sample, observed, predicted)
print(rmse_out_of_sample)
