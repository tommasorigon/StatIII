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

# The residuals do NOT coincide. The number of observations is actually different
residuals(m1_grouped)
residuals(m1_ungrouped)

# The first plot is meaningful and informative, the second one is not
plot(fitted(m1_grouped), rstandard(m1_grouped))
plot(fitted(m1_ungrouped), rstandard(m1_ungrouped))

# QUESTION 2

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