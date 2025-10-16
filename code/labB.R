# LAB B, Statistica III (Generalized Linear Models)
# Title: "IRLS algorithm". Dataset: Beetles
# Author: Tommaso Rigon

# Dataset 1: Beetles ----------------------------------------------------------------------------------

rm(list = ls())

library(MLGdata)
data("Beetles")

# A few useful quantities
m <- Beetles$num
s <- Beetles$uccisi
y <- s / m
x <- Beetles$logdose

# Other useful quantities
ytilde <- (s + 0.5) / (m + 1) # Corrected response
X <- cbind(1, x) # Design matrix

# Data overview
cbind(x, m, s, y, ytilde)
plot(x, y, pch = 16, main = "Beetles data", xlab = "log-dose", ylab = "Proportion killed")

# We are going to implement the IRLS algorithm for logistic regression.
# The key functions are qlogis() and plogis()

curve(qlogis(x), 0, 1, main = "Logit function: log(pi / (1 - pi))")
curve(plogis(x), -5, 5, main = "Inverse logit function: exp(x) / (1 + exp(x))")

# Manual implementation of the IRLS algorithm (overly manual version) -------------------------------

# STEP 0: Initialization -------------------------------------------------------------------------------

# As a starting point, we could use least squares estimates on the transformed response
beta0 <- solve(t(X) %*% X) %*% t(X) %*% qlogis(ytilde)
beta0

# However, in principle any choice would work, such as beta0 <- c(0, 0)

# STEP 1: Iteratively reweighted least squares -----------------------------------------------------------

# Compute the linear predictor
eta <- X %*% beta0
# Estimated probabilities
mu <- plogis(eta)
# Pseudo-responses
z <- eta + (y - mu) / (mu * (1 - mu))
# Weights
W <- diag(as.vector(m * mu * (1 - mu)))
# Update
beta1 <- solve(t(X) %*% W %*% X) %*% t(X) %*% W %*% z

cbind(beta0, beta1)

# Repeat one more iteration
eta <- X %*% beta1
mu <- plogis(eta)
z <- eta + (y - mu) / (mu * (1 - mu))
W <- diag(as.vector(m * mu * (1 - mu)))
beta2 <- solve(t(X) %*% W %*% X) %*% t(X) %*% W %*% z

cbind(beta0, beta1, beta2)

# IRLS algorithm, properly implemented -----------------------------------------------------------

iter <- 0
maxit <- 100 # Maximum number of iterations
tol <- 1e-10 # Tolerance level
converged <- FALSE

# Initial guess
beta_old <- c(0, 0)

while (!converged && iter < maxit) {
  iter <- iter + 1

  # Linear predictor
  eta <- X %*% beta_old
  # Estimated probabilities
  mu <- plogis(eta)
  # Pseudo-responses
  z <- eta + (y - mu) / (mu * (1 - mu))
  # Weights
  W <- diag(as.vector(m * mu * (1 - mu)))
  # Update estimates
  beta_new <- solve(t(X) %*% W %*% X) %*% t(X) %*% W %*% z

  # Check convergence
  if (max(abs(beta_new - beta_old)) < tol) converged <- TRUE

  beta_old <- beta_new
  cat("Iteration", iter, "---------------------------\n")
  cat("Coefficients: ", round(beta_new, 6), "\n")
  cat("Score: ", round(t(X) %*% diag(m) %*% (y - mu), 6), "\n\n")
}

# Final estimates ----------------------------------------------------------------

beta_hat <- beta_new
beta_hat

# Estimating equations
Omega <- diag(m)
t(X) %*% Omega %*% (y - mu)

# Variance / covariance matrix of the estimates
vars <- solve(t(X) %*% W %*% X)
vars
sqrt(diag(vars))

# Comparison with built-in tools ---------------------------------------------------

model <- glm(cbind(s, m - s) ~ x, family = binomial)
summary(model)

# Predicted values
cbind(mu, predict(model, type = "response"))

# Linear predictor
cbind(eta, predict(model, type = "link"))

# Response residuals
cbind(y - mu, residuals(model, type = "response"))
# Pearson residuals
cbind((y - mu) / sqrt(mu * (1 - mu) / m), residuals(model, type = "pearson"))
