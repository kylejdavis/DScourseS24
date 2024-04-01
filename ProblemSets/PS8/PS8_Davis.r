
# Set the seed of the random number generator
set.seed(100)

# Define constants
N <- 100000
K <- 10
sigma <- 0.5

# Create X matrix
X <- cbind(1, matrix(rnorm((K-1)*N), ncol = K-1, nrow = N))

# Create eps vector
eps <- rnorm(N, mean = 0, sd = sigma)

# Define beta vector
beta <- c(1.5, -1, -0.25, 0.75, 3.5, -2, 0.5, 1, 1.25, 2)

# Generate Y vector
Y <- X %*% beta + eps
head(Y)

# Compute beta OLS
beta_ols <- solve(t(X) %*% X) %*% t(X) %*% Y
head(beta_ols)

# Compare estimated beta with true beta
beta_diff <- beta_ols - beta
print(beta_diff)

# Initialize beta
beta_gd <- rep(0, K)

# Set learning rate
alpha <- 0.0000003

# Number of iterations
iterations <- 1000

# Gradient Descent
for (i in 1:iterations) {
    gradient <- -2 * t(X) %*% (Y - X %*% beta_gd)
    beta_gd <- beta_gd - alpha * gradient
}

print(beta_gd)

# Load nloptr package
library(nloptr)

# Define objective function
objective_function <- function(beta) {
    residuals <- Y - X %*% beta
    return(sum(residuals^2))
}

# Define gradient function
gradient_function <- function(beta) {
    return(-2 * t(X) %*% (Y - X %*% beta))
}

# Initial guess for beta
beta_init <- rep(0, K)

# Use L-BFGS algorithm from nloptr to compute beta
result <- nloptr(x0 = beta_init, eval_f = objective_function, eval_grad_f = gradient_function, opts = list(algorithm = "NLOPT_LD_LBFGS", print_level = 0))

# Print the estimated beta
print(result$solution)

# Use Nelder-Mead algorithm from nloptr to compute beta
result_nm <- nloptr(x0 = beta_init, eval_f = objective_function, opts = list(algorithm = "NLOPT_LN_NELDERMEAD", print_level = 0))

# Print the estimated beta
print(result_nm$solution)

# Compare the results of the two algorithms
beta_diff_nm <- result$solution - result_nm$solution
print(beta_diff_nm)

# Define likelihood function
likelihood_function <- function(theta) {
    beta <- theta[1:(length(theta) - 1)]
    sig <- theta[length(theta)]
    residuals <- Y - X %*% beta
    return(sum(residuals^2) / (2 * sig^2) + length(Y) * log(sig))
}

# Define gradient function
gradient_function <- function(theta) {
    grad <- as.vector(rep(0, length(theta)))
    beta <- theta[1:(length(theta) - 1)]
    sig <- theta[length(theta)]
    grad[1:(length(theta) - 1)] <- -t(X) %*% (Y - X %*% beta) / (sig^2)
    grad[length(theta)] <- dim(X)[1] / sig - crossprod(Y - X %*% beta) / (sig^3)
    return(grad)
}

# Initial guess for theta
theta_init <- c(rep(0, K), 1)

# Use L-BFGS algorithm from nloptr to compute theta
result_mle <- nloptr(x0 = theta_init, eval_f = likelihood_function, eval_grad_f = gradient_function, opts = list(algorithm = "NLOPT_LD_LBFGS", print_level = 0))

# Print the estimated theta
print(result_mle$solution)

# Compute beta OLS using lm()
model <- lm(Y ~ X - 1)

# Print the estimated beta
print(coef(model))

# Load stargazer package
library(stargazer)

# Generate LaTeX code
latex_code <- stargazer(model, title="Regression Results", header=FALSE, type='latex')

# Write LaTeX code to .tex file
writeLines(latex_code, con = "regression_output.tex")
