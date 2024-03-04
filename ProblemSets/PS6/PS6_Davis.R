# Load the dataset
data(mtcars)

# Check for missing values
sum(is.na(mtcars))

# If there were missing values, we could use the following code to remove them
# mtcars <- na.omit(mtcars)

# Check for duplicate rows
anyDuplicated(mtcars)

# If there were duplicates, we could use the following code to remove them
# mtcars <- mtcars[!duplicated(mtcars), ]

# Normalize the data
normalize <- function(x) {
    return ((x - min(x)) / (max(x) - min(x)))
}

mtcars_norm <- as.data.frame(lapply(mtcars, normalize))

# Now mtcars_norm is the cleaned and normalized version of the mtcars dataset
head(mtcars_norm)

# Load the ggplot2 package
library(ggplot2)

# Scatter plot
ggplot(mtcars, aes(x = hp, y = mpg)) +
  geom_point() +
  labs(title = "Scatter plot of MPG vs Horsepower",
       x = "Horsepower",
       y = "Miles per Gallon")


# Histogram
ggplot(mtcars, aes(x = mpg)) +
  geom_histogram(binwidth = 2, fill = "blue", color = "black") +
  labs(title = "Histogram of MPG",
       x = "Miles per Gallon",
       y = "Frequency")

# Box plot
ggplot(mtcars, aes(x = factor(cyl), y = mpg)) +
  geom_boxplot() +
  labs(title = "Box plot of MPG by Number of Cylinders",
       x = "Number of Cylinders",
       y = "Miles per Gallon")

# Scatter plot
p1 <- ggplot(mtcars, aes(x = hp, y = mpg)) +
    geom_point() +
    labs(title = "Scatter plot of MPG vs Horsepower",
             x = "Horsepower",
             y = "Miles per Gallon")
ggsave("C:/Users/12kyl/Documents/OUHES/scatter_plot.png", p1)

# Histogram
p2 <- ggplot(mtcars, aes(x = mpg)) +
    geom_histogram(binwidth = 2, fill = "blue", color = "black") +
    labs(title = "Histogram of MPG",
             x = "Miles per Gallon",
             y = "Frequency")
ggsave("histogram.png", p2)

# Box plot
p3 <- ggplot(mtcars, aes(x = factor(cyl), y = mpg)) +
    geom_boxplot() +
    labs(title = "Box plot of MPG by Number of Cylinders",
             x = "Number of Cylinders",
             y = "Miles per Gallon")
ggsave("box_plot.png", p3)

# Specify the directory where you want to save the files
dir <- "C:/Users/12kyl/Documents/OUHES/PS6_Davis"

# Create the directory if it doesn't exist
if (!dir.exists(dir)) {
  dir.create(dir)
}

# Save the plots to the specified directory
ggsave(paste0(dir, "/scatter_plot.png"), p1)
ggsave(paste0(dir, "/histogram.png"), p2)
ggsave(paste0(dir, "/box_plot.png"), p3)
