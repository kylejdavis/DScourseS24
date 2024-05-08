#Load the File
HEC1B <- read.csv("HEC1B.csv")

# List the first few rows of the data
head(HEC1B)

# Create a new variable that is (H+(3*HR)+RBI+R+(3*SB)+BB)
HEC1B$Fantasy <- ((HEC1B$H - HEC1B$HR - HEC1B$X3B - HEC1B$X2B) *3) + (6 * HEC1B$HR) + (HEC1B$SB) + (HEC1B$BB * 0.5) + (HEC1B$X2B * 4) + (HEC1B$X3B * 5) - (HEC1B$AB * 0.5) - (HEC1B$CS * 2)


# Compute the mean of the fantasy score
mean_fantasy_score <- mean(HEC1B$Fantasy)
print(mean_fantasy_score)


#Show variable names in HEC1B data frame
names(HEC1B)


# Create a predictor with multiple variables for Fantasy variable
fit <- lm(Fantasy ~ BB. + BABIP + ISO + G, data = HEC1B)



# Print the summary of the model
summary(fit)

# Use the model to make predictions
predictions <- predict(fit, newdata = HEC1B)

# Print the predictions
print(predictions)

# Create a scatterplot of the actual vs. predicted values
plot(HEC1B$Fantasy/HEC1B$G, predictions, xlab = "Actual Fantasy/G", ylab = "Predicted Fantasy/G", main = "Actual vs. Predicted Fantasy/G")

# Calculate residuals
residuals <- HEC1B$Fantasy/HEC1B$G - predictions

# Plot residuals vs. predicted values
plot(predictions, residuals, xlab = "Predicted Fantasy/G", ylab = "Residuals", main = "Residuals vs. Predicted Fantasy/G")

# Plot (OPS*G) vs. Fantasy
plot(HEC1B$OPS*HEC1B$G, HEC1B$Fantasy, xlab = "OPS*G", ylab = "Fantasy", main = "OPS*G vs. Fantasy")


# Function to automatically test different predictor variables
test_predictors <- function(response, predictors, data) {
    for (predictor in predictors) {
        formula <- as.formula(paste(response, "~", predictor))
        fit <- lm(formula, data = data)
        print(summary(fit))
    }
}

# List of predictor variables to test
predictors <- c("BB.", "BABIP", "ISO", "G", "K.", "Hard.",
                "OPS", "Spd", "LD.", "GB.", "FB.", "HR:FB.")

# Test the predictor variables
test_predictors("Fantasy", predictors, HEC1B)

# Function to automatically test different combinations of predictor variables
test_predictors <- function(response, predictors, data) {
    for (i in 1:(length(predictors) - 1)) {
        for (j in (i + 1):length(predictors)) {
            formula <- as.formula(paste(response, "~", predictors[i], "+", predictors[j]))
            fit <- lm(formula, data = data)
            print(summary(fit))
        }
    }
}

# List of predictor variables to test
predictors <- c("BB.", "BABIP", "ISO", "G", "K.", "Hard.",
                                "OPS", "Spd", "LD.", "GB.", "FB.", "HR/FB.")

# Test the predictor variables
test_predictors("Fantasy", predictors, HEC1B)

# Function to automatically test different combinations of predictor variables
test_predictors <- function(response, predictors, data) {
    # Initialize a data frame to store the results
    results <- data.frame(
        formula = character(),
        adj.r.squared = numeric(),
        stringsAsFactors = FALSE
    )

    for (i in 1:(length(predictors) - 1)) {
        for (j in (i + 1):length(predictors)) {
            formula <- as.formula(paste(response, "~", predictors[i], "+", predictors[j]))
            fit <- lm(formula, data = data)
            # Add the results to the data frame
            results <- rbind(results, data.frame(
                formula = paste(predictors[i], "+", predictors[j]),
                adj.r.squared = summary(fit)$adj.r.squared
            ))
        }
    }

    # Sort the results by the adjusted R-squared value in descending order
    results <- results[order(-results$adj.r.squared), ]

    # Return the top 3 models
    return(head(results, 10))
}

# List of predictor variables to test
predictors <- c("BB.", "BABIP", "ISO", "G", "K.", "Hard.",
                                "OPS", "Spd", "LD.", "GB.", "FB.", "HR/FB.")

# Test the predictor variables and print the top 3 models
top_models <- test_predictors("Fantasy", predictors, HEC1B)
print(top_models)




## THIS IS WHERE THE FINAL MODEL IS FIRST FOUND ##


# Function to automatically test different combinations of predictor variables
test_predictors <- function(response, predictors, data) {
    # Initialize a data frame to store the results
    results <- data.frame(
        formula = character(),
        adj.r.squared = numeric(),
        stringsAsFactors = FALSE
    )

    # Use the combn() function to generate all combinations of 4 predictors
    combinations <- combn(predictors, 4)
    
    for (i in 1:ncol(combinations)) {
        formula <- as.formula(paste(response, "~", paste(combinations[, i], collapse = " + ")))
        fit <- lm(formula, data = data)
        # Add the results to the data frame
        results <- rbind(results, data.frame(
            formula = paste(combinations[, i], collapse = " + "),
            adj.r.squared = summary(fit)$adj.r.squared
        ))
    }

    # Sort the results by the adjusted R-squared value in descending order
    results <- results[order(-results$adj.r.squared), ]

    # Return the top 10 models
    return(head(results, 10))
}

# List of predictor variables to test
predictors <- c("BB.", "BABIP", "ISO", "G", "K.", "Hard.",
                                 "Spd", "LD.", "GB.", "FB.", "HR/FB.")

# Test the predictor variables and print the top 10 models
top_models <- test_predictors("Fantasy", predictors, HEC1B)
print(top_models)

# Create model prediciting fantasy by ISO, OPS, HR/FB., and G
fit <- lm(Fantasy ~ Hard. + HR:FB. + G + Contact., data = HEC1B)

# Print the summary of the model
summary(fit)

# Install and load the car package
if (!require(car)) install.packages('car')
library(car)

# Calculate VIF
vif(fit)

# Install glmnet if not already installed
if (!require(glmnet)) install.packages('glmnet')

# Load the glmnet package
library(glmnet)

# Prepare the data for glmnet
x <- model.matrix(Fantasy ~ Hard. + HR:FB. + G + Contact. , data = HEC1B)[, -1]
y <- HEC1B$Fantasy

# Perform ridge regression
fit <- glmnet(x, y, alpha = 0, lambda = 0.1)

# Print the coefficients
print(coef(fit))

#Plot actual vs predicted fantasy scores with regression line
plot(HEC1B$Fantasy, predict(fit, newx = x), xlab = "Actual Fantasy Scores", ylab = "Predicted Fantasy Scores", main = "Actual vs Predicted Fantasy Scores")


# Perform cross-validation to find the optimal lambda
cv_fit <- cv.glmnet(x, y, alpha = 0)

# Print the optimal lambda
print(cv_fit$lambda.min)

lambda_grid <- grid_regular(penalty(), levels = 50)

# Perform lasso regression
fit <- glmnet(x, y, alpha = 1, lambda = 0.1)

# Print the coefficients
print(coef(fit))

# Predict fantasy scores using the lasso model
predictions <- predict(fit, newx = x)

# Calculate the R-squared (R^2)
sst <- sum((y - mean(y))^2)
sse <- sum((predictions - y)^2)
r_squared <- 1 - (sse / sst)
print(r_squared)



# Create a variable in HEC1B that is the predicted fantasy scores
HEC1B22$Predicted_Fantasy <- predictions


# Save the csv
write.csv(HEC1B, file = "HEC1B.csv")

# Plot predicted fantasy scores vs. actual fantasy scores
ggplot(HEC1B, aes(x = Fantasy, y = Predicted_Fantasy)) +
    geom_point() +
    geom_smooth(method = "lm", col = "red") +
    labs(x = "Actual Fantasy Scores", y = "Predicted Fantasy Scores", title = "Actual vs Predicted Fantasy Scores") +
    theme_minimal()

# Load the ggplot2 library
library(ggplot2)

# Create a data frame to hold the actual and predicted values
data <- data.frame(Actual = y, s0 = predictions)
print(data)

# Create the plot
ggplot(data, aes(x = Actual, y = s0)) +
    geom_point() +
    geom_smooth(method = "lm", col = "red") +
    labs(x = "Actual Fantasy Scores", y = "Predicted Fantasy Scores", title = "Actual vs Predicted Fantasy Scores") +
    theme_minimal()



asdfdf


# Import HEC1B22.csv
HEC1B22 <- read.csv("HEC1B22.csv")

# Creat the fantasy variable
HEC1B22$Fantasy <- ((HEC1B22$H - HEC1B22$HR - HEC1B22$X3B - HEC1B22$X2B) *3) + (6 * HEC1B22$HR) + (HEC1B22$SB) + (HEC1B22$BB * 1.5) + (HEC1B22$X2B * 4) + (HEC1B22$X3B * 5) - (HEC1B22$AB * 0.5) - (HEC1B22$CS * 2)

# Compute mean fantasy score
mean_fantasy_score <- mean(HEC1B22$Fantasy)
print(mean_fantasy_score)

# Prepare the data for glmnet
x <- model.matrix(Fantasy ~ Hard. + HR:FB. + G + Contact. , data = HEC1B22)[, -1]
y <- HEC1B22$Fantasy

fit <- glmnet(x, y, alpha = 1, lambda = 0.1)

# Print the coefficients
print(coef(fit))

# Predict fantasy scores using the lasso model
newx <- model.matrix(Fantasy ~ Hard. + HR:FB. + G + Contact. , data = HEC1B22)[, -1]
predictions <- predict(fit, newx = newx)

# Calculate the R-squared (R^2)
sst <- sum((y - mean(y))^2)
sse <- sum((predictions - y)^2)
r_squared <- 1 - (sse / sst)
print(r_squared)

# Create variable in HEC1B that is =-478.12 + 451.57(Hard.) + 5.02(HR:FB.) + 2.16(G) + 372.38(Contact.)
HEC1B22$Predicted <- -478.12 + 451.57*HEC1B$Hard. + 5.02*HEC1B$HR.FB + 2.16*HEC1B$G + 372.38*HEC1B$Contact.

# Add the predicted fantasy scores to the HEC1B data frame
HEC1B$Predicted_Fantasy <- HEC1B$Fantasy22

# Plot Predicted_Fantasy against Fantasy from HEC1B
ggplot(HEC1B, aes(x = Fantasy22, y = Fantasy)) +
    geom_point() +
    geom_smooth(method = "lm", col = "red") +
    labs(x = "Predicted Fantasy Scores", y = "Actual Fantasy Scores", title = "Predicted vs Actual Fantasy Scores") +
    theme_minimal()

# Save the updated data frame to a new CSV file
write.csv(HEC1B, file = "HEC1B.csv")


# Print the updated data frame
print(HEC1B)

# Use GGplot to plot Fantasy22 against Fantasy
ggplot(HEC1B, aes(x = Fantasy22, y = Fantasy)) +
    geom_point() +
    geom_smooth(method = "lm", col = "red") +
    labs(x = "Predicted Fantasy Scores", y = "Actual Fantasy Scores", title = "Predicted vs Actual Fantasy Scores") +
    theme_minimal()

# Load the glmnet library
library(glmnet)

names(HEC1B)

sgdsf

# Create a model matrix
x <- model.matrix(Fantasy ~ BB. + K. + Spd + LD. + GB. + FB. + HR:FB. + Hard. + Contact. + O.Swing. + G, data = HEC1B)[, -1]
y <- HEC1B$Fantasy

# Perform lasso regression with cross-validation to find the best lambda
cv.fit <- cv.glmnet(x, y, alpha = 1)

# Print the best lambda value
print(cv.fit$lambda.min)

# Fit the final lasso model with the best lambda
final_model <- glmnet(x, y, alpha = 1, lambda = cv.fit$lambda.min)

# Print the coefficients
print(coef(final_model))

# Predict fantasy scores using the final lasso model
predictions <- predict(final_model, newx = x)

# Calculate the R-squared (R^2)
sst <- sum((y - mean(y))^2)
sse <- sum((predictions - y)^2)
r_squared <- 1 - (sse / sst)
print(r_squared)


####

# Filter the data for the years 2021 and 2022
data_2021 <- HEC1B$Predicted_Fantasy
data_2022 <- HEC1B22$Fantasy

# Get the players that are present in both datasets
common_players <- intersect(HEC1B$Player, HEC1B22$Player)

# Filter the data for the common players
data_2021 <- HEC1B$Predicted_Fantasy[HEC1B$Player %in% common_players]
data_2022 <- HEC1B22$Fantasy[HEC1B22$Player %in% common_players]

# Plot the data
plot(data_2021, data_2022)


ajsdkas
### Here is the final code for the HEC1B.r file ###


Comp <- read.csv("TRUEHEC.csv")

# Create the Fantasy variable
Comp$Fantasy <- ((Comp$H - Comp$HR - Comp$X3B - Comp$X2B) *3) + (6 * Comp$HR) + (Comp$SB) + (Comp$BB * 1.5) + (Comp$X2B * 4) + (Comp$X3B * 5) - (Comp$AB * 0.5) - (Comp$CS * 2)

# Compute the mean of the fantasy score
mean_fantasy_score <- mean(Comp$Fantasy)
print(mean_fantasy_score)

# Prepare the data for glmnet
x <- model.matrix(Fantasy ~ Hard. + HR:FB. + G + O.Contact. , data = Comp)[, -1]
y <- Comp$Fantasy

fit <- glmnet(x, y, alpha = 1, lambda = 0.1)

# Print the coefficients
print(coef(fit))

fit <- lm(Fantasy ~ Hard. + HR:FB. + G + O.Contact., data = Comp)

# Print the summary of the model
summary(fit)

# Create a Fantasy points per game variable
Comp$FantasyPG <- Comp$Fantasy / Comp$G

# Create a predicted fantasy points per game variable
Comp$Predicted_FantasyPG <- predict(fit, newdata = Comp)

library(ggplot2)

# Create the scatter plot with a regression line
ggplot(Comp, aes(x = Predicted_FantasyPG, y = FantasyPG)) +
    geom_point() +
    geom_smooth(method = "lm", col = "red") +
    labs(x = "Predicted Fantasy Points per Game", y = "Actual Fantasy Points per Game") +
    ggtitle("Predicted vs Actual Fantasy Points per Game") +
    theme_minimal()

# Load in TRUEHEC22.csv
Comp22 <- read.csv("TRUEHEC22.csv")

# Create the Fantasy variable
Comp22$Fantasy <- ((Comp22$H - Comp22$HR - Comp22$X3B - Comp22$X2B) *3) + (6 * Comp22$HR) + (Comp22$SB) + (Comp22$BB * 1.5) + (Comp22$X2B * 4) + (Comp22$X3B * 5) - (Comp22$AB * 0.5) - (Comp22$CS * 2)

# Compute the mean of the fantasy score
mean_fantasy_score <- mean(Comp22$Fantasy)
print(mean_fantasy_score)

# Create fantasy points per game variable
Comp22$FantasyPG <- Comp22$Fantasy / Comp22$G

# Merge Comp and Comp22
data <- merge(Comp, Comp22, by = "Name", all = FALSE)


# Create the scatter plot with a regression line
ggplot(data, aes(x = Predicted_FantasyPG, y = FantasyPG.y)) +
    geom_point() +
    geom_smooth(method = "lm", col = "red") +
    labs(x = "Predicted Fantasy Points per Game", y = "Actual Fantasy Points per Game") +
    ggtitle("Predicted vs Actual Fantasy Points per Game") +
    theme_minimal()

# Use the same regression formula to create a predicted fantasy for Comp22
fit <- lm(Fantasy ~ Hard. + HR:FB. + G + O.Contact., data = Comp22)

# Create a predicted fantasy points variable
Comp22$Predicted_Fantasy <- predict(fit, newdata = Comp22)

# Create a predicted fantasy points per game variable
Comp22$Predicted_FantasyPG <- Comp22$Predicted_Fantasy / Comp22$G

# Import the TRUEHEC23.csv file
Comp23 <- read.csv("TRUEHEC23.csv")

# Create the Fantasy variable
Comp23$Fantasy <- ((Comp23$H - Comp23$HR - Comp23$X3B - Comp23$X2B) *3) + (6 * Comp23$HR) + (Comp23$SB) + (Comp23$BB * 1.5) + (Comp23$X2B * 4) + (Comp23$X3B * 5) - (Comp23$AB * 0.5) - (Comp23$CS * 2)

# Compute the mean of the fantasy score
mean_fantasy_score <- mean(Comp23$Fantasy)
print(mean_fantasy_score)

# Create fantasy points per game variable
Comp23$FantasyPG <- Comp23$Fantasy / Comp23$G

# Merge Comp and Comp23
data <- merge(Comp22, Comp23, by = "Name", all = FALSE)


# Create the scatter plot with a regression line
ggplot(data, aes(x = Predicted_FantasyPG, y = FantasyPG.y)) +
    geom_point() +
    labs(x = "Predicted Fantasy Points per Game", y = "Actual Fantasy Points per Game") +
    ggtitle("Predicted vs Actual Fantasy Points per Game") +
    theme_minimal()

# Save the ggplot as a png file
ggsave("Predicted_vs_Actual_Fantasy.png")

# Create the predicted fantasy variable in Comp23
fit <- lm(Fantasy ~ Hard. + HR:FB. + G + O.Contact., data = Comp23)

summary(fit)

# Create a predicted fantasy points variable
Comp23$Predicted_Fantasy <- predict(fit, newdata = Comp23)

# Create a predicted fantasy points per game variable
Comp23$Predicted_FantasyPG <- Comp23$Predicted_Fantasy / Comp23$G

summary(fit)

# Create a table that will have the mean fantasy score for Comp, Comp22, and Comp23
mean_fantasy_scores <- data.frame(
    Comp = mean(Comp$Fantasy),
    Comp22 = mean(Comp22$Fantasy),
    Comp23 = mean(Comp23$Fantasy)
)

# Print the mean fantasy scores
print(mean_fantasy_scores)

# Plot the predicted fantasy from 2023 against the actual fantasy from 2023
ggplot(Comp23, aes(x = Predicted_Fantasy, y = Fantasy)) +
    geom_point() +
    geom_smooth(method = "lm", col = "red") +
    labs(x = "Predicted Fantasy", y = "Actual Fantasy") +
    ggtitle("Predicted vs Actual Fantasy") +
    theme_minimal()

# Save the ggplot as a png file
ggsave("Predicted_vs_Actual_Fantasy_2023.png")
