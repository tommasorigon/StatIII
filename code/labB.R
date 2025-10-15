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
plot(x, y, pch = 16)

# We are going to implement the IRLS algorithm for logistic regression. The key functions are qlogis and plogis

curve(qlogis(x), 0, 1) # Logit function: log(pi/(1- pi))
curve(plogis(x), -5, 5) # Inverse logit function: exp(x)/(1+exp(x))

# Manual implementation of the IRLS algorithm described in Unit B - overly manual way

# STEP 0: Initialization -------------------------------------------------------------------------------

# As a starting point, we could use the least squares estimates on the transformed response
beta0 <- solve(t(X) %*% X) %*% t(X) %*% qlogis(ytilde)
beta0

# However, in principle any choice would work, such as beta0 <- c(0, 0)

# STEP 1: Iteratively reweighted least squares -----------------------------------------------------------

# Compute the linear predictor
eta <- X %*% beta0
eta

# Compute the estimated probabilities
mu <- plogis(eta)
mu

# Compute the pseudo-responses z:
z <- eta + (y - mu) / (mu * (1 - mu))
z

# Compute the weights W:
W <- diag(as.vector(m * mu * (1 - mu)))
W

# Update the estimates
beta1 <- solve(t(X) %*% W %*% X) %*% t(X) %*% W %*% z

cbind(beta0, beta1)

# And now let us repeat this until convergence

# STEP 2: Iteratively reweighted least squares -----------------------------------------------------------

# Compute the linear predictor
eta <- X %*% beta1
eta

# Compute the estimated probabilities
mu <- plogis(eta)
mu

# Compute the pseudo-responses z:
z <- eta + (y - mu) / (mu * (1 - mu))
z

# Compute the weights W:
W <- diag(as.vector(m * mu * (1 - mu)))
W

# Update the estimates
beta2 <- solve(t(X) %*% W %*% X) %*% t(X) %*% W %*% z

cbind(beta0, beta1, beta2)

# and so on...

# IRLS algorithm, "properly" implemented -----------------------------------------------------------

iter <- 0
maxit <- 100 # Maximum number of iterations
tol <- 1e-10 # Tolerance level
converged <- FALSE

beta_old <- solve(t(X) %*% X) %*% t(X) %*% qlogis(ytilde)
beta_old <- c(0, 0) # Initial guess

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

  # Update estimates using GLS
  beta_new <- solve(t(X) %*% W %*% X) %*% t(X) %*% W %*% z

  # Check convergence
  if (max(abs(beta_new - beta_old)) < tol) {
    converged <- TRUE
  }
  beta_old <- beta_new
  cat(beta_new, "\n")
}

# Important quantities ------------------------------------------------------------

# Maximum likelihood estimate
beta_hat <- beta_new
beta_hat

# Variance / covariance matrix of the estimates
vars <- solve(t(X) %*% W %*% X)
vars
sqrt(diag(vars))

# A comparison with built-in tools
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
