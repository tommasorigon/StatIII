# LAB 2, Statistica III (Generalized Linear Models)
# Title: "Generalized linear models". Datasets: Clotting, Chimps
# Author: Tommaso Rigon

# Overview of the glm() function in R -------------------------------------------------

# - The main function for fitting linear models in R is: lm()
# - The main function for fitting generalized linear models in R is: glm()

# The general syntax is:
#   glm(formula, family, data, weights)

# Arguments:
#   - formula: an object of class "formula", i.e. a symbolic description of the model to be fitted.
#              (This works in the same way as in lm().)
#
#   - family:  specifies the error distribution and link function.
#              Common families include:
#
#       * binomial(link = "logit")
#            Links: "logit", "probit", "cauchit" (logistic, normal, and Cauchy CDFs),
#                   "log", "cloglog" (complementary log-log).
#
#       * gaussian(link = "identity")
#            Links: "identity", "log", "inverse".
#
#       * Gamma(link = "inverse")
#            Links: "inverse", "identity", "log".
#
#       * inverse.gaussian(link = "1/mu^2")
#            Links: "1/mu^2", "inverse", "identity", "log".
#
#       * poisson(link = "log")
#            Links: "log", "identity", "sqrt".
#
#   - data:    a data frame containing the variables in the model.
#
#   - weights: optional vector of weights to be used in the fitting process.
#              These can be frequency weights (replicating observations)
#              or prior weights (adjusting contribution to the likelihood).

# -------------------------------------------------------------------
# Notes:
#   - The result of glm() is an object of class "glm".
#   - Standard extractor functions apply: summary(), coefficients(), fitted(),
#     residuals(), anova(), predict(), etc.
#   - Use ?family for more details on available families and links.

# -------------------------------------------------------------------
# Dataset 1: Clotting
# -------------------------------------------------------------------

# Load data: clotting times (seconds) for nine plasma concentrations and two clotting agents
# Source: McCullagh & Nelder (1989), Generalized Linear Models, 2nd Edition
library(MLGdata)
data("Clotting")

str(Clotting)
# View(Clotting) # Only for small datasets

# Descriptive statistics
summary(Clotting)

# Log-transform plasma concentration
Clotting$logu <- log(Clotting$u)

# Scatter plot: clotting time vs log-concentration
plot(Clotting$logu, Clotting$tempo,
  col = Clotting$lotto,
  xlab = "Log-plasma concentration", ylab = "Clotting time (s)", pch = 16
)

# -------------------------------------------------------------------
# Fit a Gamma GLM with canonical inverse link
m1 <- glm(tempo ~ logu + lotto, family = Gamma(link = "inverse"), data = Clotting)
summary(m1)
# Note: coefficients interpretation requires care; inverse link “inverts” effect direction

# Add interaction term
m1_bis <- update(m1, . ~ . + logu:lotto)
summary(m1_bis)

# Compare models
anova(m1, m1_bis, test = "Chisq")

# -------------------------------------------------------------------
# Predictions
newdata <- data.frame(
  logu = rep(seq(1.5, 4.8, length = 100), each = 2),
  lotto = rep(c("uno", "due"), 100)
)
pred_m1_bis <- predict(m1_bis, newdata = newdata, type = "response")

# Plot predicted lines
lines(newdata$logu[newdata$lotto == "uno"], pred_m1_bis[newdata$lotto == "uno"], lty = "dashed", col = "red")
lines(newdata$logu[newdata$lotto == "due"], pred_m1_bis[newdata$lotto == "due"], lty = "dashed")

plot(Clotting$tempo, fitted(m1_bis), pch = 16, xlab = "Observed values", ylab = "Fitted values")
abline(c(0, 1), lty = "dotted")

# -------------------------------------------------------------------
# Diagnostics

# Response residuals (not recommended for GLMs with non-identity link)
residuals(m1_bis, type = "response") # Coincides with Clotting$tempo - fitted(m1_bis)

# Deviance residuals (default in R)
residuals(m1_bis, type = "deviance")
# Pearson residuals
residuals(m1_bis, type = "pearson")

# Dispersion parameter estimate using Pearson residuals
sum(residuals(m1_bis, type = "pearson")^2) / m1_bis$df.residual

# Plot standardized residuals vs fitted values
plot(fitted(m1_bis), rstandard(m1, type = "pearson"), pch = 16)
plot(fitted(m1_bis), rstandard(m1, type = "deviance"), pch = 16)

# Standard diagnostic plots
par(mfrow = c(2, 2))
plot(m1_bis, which = 1:4) # Residuals, fitted values, leverage, Cook's distance
par(mfrow = c(1, 1))

# Identify observations with high Cook's distance
round(cooks.distance(m1_bis), 3)
Clotting[c(1, 10), ]
# Remarks: These points appear as "outliers" in diagnostics despite the fit being very good.
# This is because they are predicted with less precision compared to other points,
# while the prediction for most points is extremely precise.
# They are certainly leverage points. Excluding them would bias coefficient estimates and reduce reliability.

# Leverage values
influence(m1_bis)$hat
influence(m1_bis)$hat[c(1, 10)]

# Standardized residuals
rstandard(m1_bis, type = "deviance")
rstandard(m1_bis, type = "pearson")

# -------------------------------------------------------------------
# Additional notes
vcov(m1_bis) # Covariance matrix of coefficients (X'WX)^(-1) in IRLS
fitted(m1_bis) # Fitted values
predict(m1_bis, type = "response") # Fitted values on response scale
predict(m1_bis) # Linear predictor
1 / predict(m1_bis) # Same as above; demonstrates inverse link interpretation

# -------------------------------------------------------------------
# Dataset 2: Chimps
# -------------------------------------------------------------------

rm(list = ls())
data("Chimps")

str(Chimps)
# View(Chimps) # Use only for small datasets

# Times (in minutes) taken by four chimpanzees to learn each of four words
boxplot(y ~ chimp, data = Chimps)
boxplot(y ~ word, data = Chimps)

# Start with a Gamma model with the canonical link
m1 <- glm(y ~ chimp + word, family = Gamma, data = Chimps)
summary(m1)

# Predicted values
muhat <- fitted(m1)

phihat <- sum((Chimps$y - muhat)^2 / muhat^2) / m1$df.residual
phihat # Matches the dispersion estimate reported in the summary

# Check if the model is an improvement over the null
D0 <- m1$null.deviance # Null deviance
DC <- m1$deviance # Residual deviance
df <- m1$df.null - m1$df.residual # Parameter "q" in the slides
z_test <- (D0 - DC) / phihat
alphaoss <- 1 - pchisq(z_test, df)
alphaoss # The null hypothesis is rejected

# Alternatively, we could obtain the same results as follows:
m_null <- glm(y ~ 1, family = Gamma, data = Chimps) # Model with only the intercept
summary(m_null) # Null deviance and residual deviance coincide

anova(m_null, m1, test = "Chisq")
# Compare with:
D0
DC
D0 - DC
z_test # Not reported in the anova table (but used to compute the p-value)

# Test whether "word" can be removed
m1_reduced <- glm(y ~ chimp, family = Gamma, data = Chimps)
summary(m1_reduced)
anova(m1_reduced, m1) # Null hypothesis is rejected

# Another way is to use anova(m1), but note that this introduces covariates
# sequentially according to the formula order (sometimes useful, often not)
anova(m1)

# Diagnostics
Chimps$predicted1 <- muhat
View(Chimps)

# Observed vs fitted values
plot(Chimps$y, Chimps$predicted1,
  pch = 16,
  xlab = "Observed values", ylab = "Fitted values"
)
abline(c(0, 1), lty = "dotted")

# Standard diagnostic plots
par(mfrow = c(2, 2))
plot(m1, which = 1:4) # Residuals, fitted values, leverage, Cook's distance
par(mfrow = c(1, 1))

# --------------------------------------------------------------------
# Model 2: Gamma GLM with log link

m2 <- glm(y ~ chimp + word, family = Gamma(link = log), data = Chimps)
summary(m2)

# This model (with the log link) guarantees positive fitted values.
# The deviance is slightly lower compared to m1, but it is not straightforward
# to formally test whether the improvement is significant.

deviance(m1)
deviance(m2)

Chimps$predicted2 <- fitted(m2)
View(Chimps)

# Observed vs fitted values (log-link model)
plot(Chimps$y, Chimps$predicted2,
     pch = 16,
     xlab = "Observed values", ylab = "Fitted values (log-link model)"
)
abline(c(0, 1), lty = "dotted")

# Comparison of fitted values from m1 (canonical link) vs m2 (log link)
plot(Chimps$predicted1, Chimps$predicted2,
     pch = 16,
     xlab = "Fitted values (m1)", ylab = "Fitted values (m2)"
)
abline(c(0, 1), lty = "dotted")