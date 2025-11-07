# LAB 6, Statistica III (Generalized Linear Models)
# Title: "Quasi-likelihoods". Dataset: Ants
# Author: Bernardo Nipoti, Tommaso Rigon


# Dataset 1: -----------------------------------------------------------------------

library("MLGdata")
data(Ants)
str(Ants)

Ants$Bread <- as.factor(Ants$Bread)
Ants$Butter <- as.factor(Ants$Butter)
Ants$Filling <- as.factor(Ants$Filling)
head(Ants[, 1:4], 8)

## Model estimation -----------------------------------------------------------------------

# Poisson model
m1 <- glm(Ant_count ~ Bread + Filling + Butter, family = poisson, data = Ants)
# Quasi-likelihood model
m2 <- glm(Ant_count ~ Bread + Filling + Butter, family = quasi(link = "log", variance = "mu"), data = Ants)

# define the type of sandwich for which we want to make a prediction
newdata <- data.frame(Bread = "2", Filling = "3", Butter = "1")

# make a prediction with the Poisson model
pred1 <- predict(m1, newdata = newdata, type = "link", se.fit = TRUE)
eta1 <- pred1$fit # estimate of the linear predictor
se1 <- pred1$se.fit # estimate of its standard error

# make a prediction with the quasi-Poisson model
pred2 <- predict(m2, newdata = new, type = "link", se.fit = TRUE)
eta2 <- pred2$fit # estimate of the linear predictor
se2 <- pred2$se.fit # estimate of its standard error

## Confidence intervals -----------------------------------------------------------------------

alpha <- 0.05
z <- qnorm(1 - alpha / 2) # standard normal quantile

# based on the Poisson model
mu_hat1 <- exp(eta1)
int1 <- c(eta1 - z * se1, eta1 + z * se1)
int_mu1 <- exp(int1)

# based on the overdispersed Poisson model
mu_hat2 <- exp(eta2) # the estimate coincides with that of the Poisson model
int2 <- c(eta2 - z * se2, eta2 + z * se2)
int_mu2 <- exp(int2)

# compare the two confidence intervals
diff(int_mu1) < diff(int_mu2) # the second interval is wider

## Prediction intervals -----------------------------------------------------------------------

# based on the Poisson model
int_prev1 <- qpois(p = c(alpha / 2, 1 - alpha / 2), lambda = mu_hat1)
diff(int_prev1) > diff(int_mu1) # the prediction interval for the response is wider than the confidence interval for the mean

# based on the overdispersed Poisson model
phi_hat <- summary(m2)$dispersion
int_prev2 <- qnorm(c(alpha / 2, 1 - alpha / 2), mean = mu_hat2, sd = sqrt(phi_hat * mu_hat2))
diff(int_prev2) > diff(int_mu2) # the prediction interval for the response is wider than the confidence interval for the mean

# compare the two prediction intervals
diff(int_prev2) > diff(int_prev1) # the second interval is wider