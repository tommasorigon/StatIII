# LAB 4, Statistica III (Generalized Linear Models)
# Title: "Binary and binomial regression". Datasets: Beetles, Credit scoring
# Author: Tommaso Rigon


# -------------------------------------------------------------------
# Dataset 1: Beetles - THEORY AND MODELLING
# -------------------------------------------------------------------

rm(list = ls())

library(MLGdata)
data("Beetles")
data("Beetles10")

# QUESTION 1. The Beetles data can be "ungrouped". Fit a logistic regression model and verify that the maximum likelihood estimates and the standard errors coincide in the two representations. Verify that deviance and residuals do NOT coincide.

# QUESTION 2. Compare the predictions with the observed values. Perform a goodness of fit test against the saturated model.

# QUESTION 3. Obtain a Wald confidence interval for the quantity \beta_1 + \beta_2 x, with x = 1.8. Compare you results with those obtained using the function "predict". Then, compute a confidence interval for the logistic transform of the above quantity (i.e., for the probability of dying when x = 1.8).

# QUESTION 4. Propose alternative link functions, compare the results with the logistic model in terms of predicted curves, diagnostics and goodness of fit.

# QUESTION 5. Which model do you prefer in the end? Why?

# The Beetles and Beetles10 datasets are, respectively, the same data in a grouped and ungrouped format.
Beetles
Beetles10

# Indeed, note that
table(Beetles10)

m1_grouped <- glm(cbind(uccisi, num - uccisi) ~ logdose, data = Beetles, family = binomial(link = "logit"))
summary(m1_grouped)

m1_ungrouped <- glm(ucciso ~ log.dose10, data = Beetles10, family = binomial(link = "logit"))
summary(m1_ungrouped)

# COMMENT: the maximum likelihood estimates and the standard errors coincide.

deviance(m1_grouped)
deviance(m1_ungrouped)

# The residuals do NOT coincide. The number of observations is different...
residuals(m1_grouped)
residuals(m1_ungrouped)

# The first plot is meaningful and informative, the second one is not
plot(fitted(m1_grouped), rstandard(m1_grouped))
plot(fitted(m1_ungrouped), rstandard(m1_ungrouped))

# QUESTION 2
newdata <- data.frame(logdose = seq(from = 1.65, to = 1.90, length = 100))
pred_m1 <- predict(m1_grouped, type = "response", newdata = newdata)

# Plot
plot(Beetles$logdose, Beetles$uccisi / Beetles$num, pch = 16)
lines(newdata$logdose, pred_m1, lty = "dotted")

# Goodness of fit
deviance(m1_grouped)
X2 <- sum(residuals(m1_grouped, type = "pearson")^2)
X2
# df
q <- m1_grouped$df.residual
# p-value
1 - pchisq(X2, df = q)

# COMMENT: as shown in the slides, the p-value is borderline which can be interpreted as a slight lack of fit.

# QUESTION 3

# Manual answer

var_beta <- vcov(m1_grouped)
pred <- coef(m1_grouped)[1] + coef(m1_grouped)[2] * 1.8
se_pred <- sqrt(var_beta[1, 1] + 1.8^2 * var_beta[2, 2] + 2 * 1.8 * var_beta[1, 2])

predict(m1_grouped, se.fit = TRUE, newdata = data.frame(logdose = 1.8))


# -------------------------------------------------------------------
# Dataset 2: Credit scoring - APPLIED ANALYSIS
# -------------------------------------------------------------------

rm(list = ls())
library(MLGdata)
data("Credit")

# Data for 1000 clients of a south german bank, 700 good payers and 300 bad payers. They are used to construct a credit scoring method.
str(Credit)

summary(Credit)

# This is a case-control study, therefore logistic regression is very much a good idea
