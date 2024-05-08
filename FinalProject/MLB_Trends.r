#Load CSV
MLB <- read.csv("YEARLY_TREND.csv")

# Step 1: Load necessary libraries
install.packages(c("tidyverse", "lubridate", "forecast"))
library(tidyverse)
library(lubridate)
library(forecast)

# Step 2: Load your data
MLB <- read.csv("Trends.csv")

# Check for NA values
sum(is.na(MLB$Season))

# Check for non-numeric values
sum(!is.numeric(as.character(MLB$Season)))

# Identify the non-numeric value
non_numeric_index <- which(!is.numeric(as.character(MLB$Season)))
MLB$Season[non_numeric_index]

# Correct the non-numeric value
MLB$Season[non_numeric_index] <- "2022"

# Ensure that the season is in the correct format
MLB$Season <- as.Date(paste0(MLB$Season, "-01-01"))

# Set the season as the row names
row.names(MLB) <- MLB$Season

# Convert the data frame to a time series object
MLB_ts <- ts(MLB$OPS, start = min(MLB$Season), end = max(MLB$Season), frequency = 1)

# Fit an ARIMA model
fit <- auto.arima(MLB_ts)

# Predict future values
forecast(fit)

# Get the fitted values from the ARIMA model
fitted_values <- fitted(fit)

# Print the fitted value for 2022
fitted_values["2022-01-01"]

# Check the range of the time series
range(time(MLB_ts))

# Check the format of the dates in the fitted values
head(index(fitted_values))

# Convert the start and end dates to years
start_year <- as.numeric(format(min(MLB$Season), "%Y"))
end_year <- as.numeric(format(max(MLB$Season), "%Y"))

# Convert the data frame to a time series object
MLB_ts <- ts(MLB$OPS, start = start_year, end = end_year, frequency = 1)

# Create the forecast using only the last 3 years of data
fit <- auto.arima(window(MLB_ts, start = end_year - 3, end = end_year))

# Predict future values
forecast(fit)

# Create the forecast using only the first 3 years of data
fit <- auto.arima(window(MLB_ts, start = start_year, end = start_year + 3))

# Predict future values
forecast(fit)

# Plot OPS over time
plot(MLB_ts, main = "OPS Over Time", xlab = "Year", ylab = "OPS")

# Sort the data by season in descending order
MLB <- MLB[order(MLB$Season, decreasing = TRUE), ]

# Plot OPS over time
plot(MLB$Season, MLB$OPS, main = "OPS Over Time", xlab = "Year", ylab = "OPS")

# Plot OPS over time with a trend line using ggplot2
ggplot(MLB, aes(x = Season, y = OPS)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "OPS Over Time", x = "Year", y = "OPS")

# Forecast OPS using a linear regression model
model <- lm(OPS ~ Season, data = MLB)

# Create a data frame with future seasons
future_seasons <- data.frame(Season = seq(as.Date("2022-01-01"), by = "year", length.out = 10))

# Predict OPS for future seasons
predicted_ops <- predict(model, newdata = future_seasons)

# Combine the future seasons and predicted OPS

future_ops <- data.frame(Season = future_seasons$Season, OPS = predicted_ops)

# Plot the forecasted OPS
ggplot(MLB, aes(x = Season, y = OPS)) +
  geom_point() +
  geom_line(data = future_ops, aes(x = Season, y = OPS), color = "red") +
  labs(title = "Forecasted OPS", x = "Year", y = "OPS")

# Plot the forecasted OPS with a trend line using ggplot2
ggplot(MLB, aes(x = Season, y = OPS)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_line(data = future_ops, aes(x = Season, y = OPS), color = "red") +
  labs(title = "Forecasted OPS", x = "Year", y = "OPS")

# List the next 5 seasons and their forecasted OPS
future_ops[1:5, ]

# Load the necessary library
library(forecast)

# Split the data into a training set and a test set
train <- window(MLB_ts, end = c(2022, 1))
test <- window(MLB_ts, start = c(1920, 1))

# Fit an ARIMA model to the training data
fit <- auto.arima(train)

# Forecast future values
forecasted_values <- forecast(fit, h = length(test))

# Calculate accuracy measures
accuracy(forecasted_values, test)

# Compare the forecasted values to the actual values
plot(forecasted_values, main = "Forecasted OPS vs. Actual OPS", xlab = "Year", ylab = "OPS")

# Create a 5 year moving average of OPS
moving_average <- stats::filter(MLB$OPS, rep(1/5, 5), sides = 2)

# Plot the moving average
plot(MLB$Season, moving_average, type = "l", main = "5 Year Moving Average of OPS", xlab = "Year", ylab = "OPS")

# Plot the moving average with the original data
plot(MLB$Season, MLB$OPS, type = "l", main = "5 Year Moving Average of OPS", xlab = "Year", ylab = "OPS")
lines(MLB$Season, moving_average, col = "red")

# Start the PNG device
png("moving_average.png")

# Recreate the plot
plot(MLB$Season, MLB$OPS, type = "l", main = "5 Year Moving Average of OPS", xlab = "Year", ylab = "OPS")
lines(MLB$Season, moving_average, col = "red")

# Close the PNG device
dev.off()

# Create a 5 year moving average of OPS using the rollmean function from the zoo package
library(zoo)
moving_average <- rollmean(MLB$OPS, k = 5, fill = NA)

# Plot the moving average with the original data
plot(MLB$Season, MLB$OPS, type = "l", main = "5 Year Moving Average of OPS", xlab = "Year", ylab = "OPS")
lines(MLB$Season, moving_average, col = "red")

# Predict the next 5 seasons using the same moving average
future_seasons <- data.frame(Season = seq(as.Date("2022-01-01"), by = "year", length.out = 5))
future_ops <- data.frame(Season = future_seasons$Season, OPS = rep(tail(moving_average, 5), each = 5))

# List the last 5 seasons in the data and their forecasted OPS
future_ops

# Plot the trendline
ggplot(MLB, aes(x = Season, y = OPS)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "OPS Over Time", x = "Year", y = "OPS")

# Plot the trendline with the forecasted OPS
ggplot(MLB, aes(x = Season, y = OPS)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_line(data = future_ops, aes(x = Season, y = OPS), color = "red") +
  labs(title = "Forecasted OPS", x = "Year", y = "OPS")

# Add a line that connects the actual data
ggplot(MLB, aes(x = Season, y = OPS)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_line(data = future_ops, aes(x = Season, y = OPS), color = "red") +
  geom_line(data = MLB, aes(x = Season, y = OPS), color = "blue") +
  labs(title = "Forecasted OPS", x = "Year", y = "OPS")

# Change the actual line to a dashed line
ggplot(MLB, aes(x = Season, y = OPS)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_line(data = future_ops, aes(x = Season, y = OPS), color = "red") +
  geom_line(data = MLB, aes(x = Season, y = OPS), color = "blue", linetype = "dashed") +
  labs(title = "Forecasted OPS", x = "Year", y = "OPS")


# Save the plot as a PNG file
ggsave("forecasted_ops.png")

summary(model)
