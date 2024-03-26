# Check if the package is installed
# If not, install it
if (!require(mice)) {
    install.packages("mice")
}

if (!require(modelsummary)) {
    install.packages("modelsummary")
}

library(mice)
library(modelsummary)

install.packages(
    c("modelsummary", "tinytable", "insight", "performance", "parameters"),
    repos = c(
        "https://vincentarelbundock.r-universe.dev",
        "https://easystats.r-universe.dev"))

# Read the CSV file
wages <- read.csv("wages.csv")

# Using na.omit
wages <- na.omit(wages, cols = c("hgc", "tenure"))

head(wages)


install.packages("correlation", repos = "https://cloud.r-project.org/")
library(correlation)

fun <- function(x) {
  out <- correlation(wages) |>
    summary() |>
    format(2) |> 
    as.matrix()
  row.names(out) <- out[, 1]
  out <- out[, 2:ncol(out)]
  return(out)
}

datasummary_correlation(wages, method = fun)

# Load necessary libraries
library(mice)
library(lmtest)

# Prepare the data
# Assuming 'data' is your dataframe and 'logwage', 'hgc', 'college', 'tenure', 'tenure2', 'age', 'married' are your columns
wages$tenure2 <- wages$tenure^2

# Estimate the regression using only complete cases
complete_cases_data <- wages[complete.cases(wages$logwage), ]
model_complete_cases <- lm(logwage ~ hgc + college + tenure + tenure2 + age + married, data = complete_cases_data)
summary(model_complete_cases)

# Perform mean imputation to fill in missing log wages
mean_imputed_data <- wages
mean_imputed_data$logwage[is.na(mean_imputed_data$logwage)] <- mean(complete_cases_data$logwage, na.rm = TRUE)
model_mean_imputed <- lm(logwage ~ hgc + college + tenure + tenure2 + age + married, data = mean_imputed_data)
summary(model_mean_imputed)

# Impute missing log wages as their predicted values from the complete cases regression
predicted_data <- wages
predicted_data$logwage[is.na(predicted_data$logwage)] <- predict(model_complete_cases, newdata = predicted_data[is.na(predicted_data$logwage), ])
model_predicted <- lm(logwage ~ hgc + college + tenure + tenure2 + age + married, data = predicted_data)
summary(model_predicted)

# Perform multiple imputation
imputed_data <- mice(wages, m=5, maxit=50, method='pmm', seed=500)

# Fit the model on each imputed dataset
fit <- with(data=imputed_data, exp=lm(logwage ~ hgc + college + tenure + tenure2 + age + married))

# Combine the results
pooled_model <- pool(fit)

# Summary of the model
summary(pooled_model)

# Create a list of models
models_list <- list(
  "Complete Cases" = model_complete_cases,
  "Mean Imputed" = model_mean_imputed,
  "Predicted" = model_predicted,
  "Multiple Imputed" = pooled_model
)

# Create the regression table
modelsummary(models_list)

modelsummary(models_list, output = 'table.html')

#Save table.html to the working directory
save_modelsummary(models_list, "table.html")
modelsummary(models_list, output = "table.html")

modelsummary(models_list, output = "table.html")
