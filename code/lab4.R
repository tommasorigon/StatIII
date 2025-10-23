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

# QUESTION 3. Obtain a Wald confidence interval for the quantity \beta_1 + \beta_2 x, with x = 1.85. Compare you results with those obtained using the function "predict". Then, compute a confidence interval for the probability of dying when x = 1.85.

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
newdata <- data.frame(logdose = seq(from = 1.655, to = 1.90, length = 100))
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
eta <- coef(m1_grouped)[1] + coef(m1_grouped)[2] * 1.85
se_eta <- sqrt(var_beta[1, 1] + 1.85^2 * var_beta[2, 2] + 2 * 1.85 * var_beta[1, 2])

c(eta, se_eta)
predict(m1_grouped, se.fit = TRUE, newdata = data.frame(logdose = 1.85))

CI_eta <- eta + c(-1, 1) * qnorm(0.975) * se_eta
CI_eta

# The "wrong" Wald CI, based on a direct transformation of CI_eta
CI_pred1 <- plogis(CI_eta) # The "wrong" wald
CI_pred1

# The Wald CI, based on the delta method
m_pred <- predict(m1_grouped, se.fit = TRUE, newdata = data.frame(logdose = 1.85), type = "response")

CI_pred2 <- m_pred$fit + c(-1, 1) * qnorm(0.975) * m_pred$se.fit
CI_pred2

# QUESTION 4

m_logit <- glm(cbind(uccisi, num - uccisi) ~ logdose, data = Beetles, family = binomial(link = "logit"))
summary(m_logit)

m_probit <- glm(cbind(uccisi, num - uccisi) ~ logdose, data = Beetles, family = binomial(link = "probit"))
summary(m_probit)

m_cauchit <- glm(cbind(uccisi, num - uccisi) ~ logdose, data = Beetles, family = binomial(link = "cauchit"))
summary(m_cauchit)

m_cloglog <- glm(cbind(uccisi, num - uccisi) ~ logdose, data = Beetles, family = binomial(link = "cloglog"))
summary(m_cloglog)

pred_logit <- predict(m_logit, type = "response", newdata = newdata)
pred_probit <- predict(m_probit, type = "response", newdata = newdata)
pred_cauchit <- predict(m_cauchit, type = "response", newdata = newdata)
pred_cloglog <- predict(m_cloglog, type = "response", newdata = newdata)

# Plot
plot(Beetles$logdose, Beetles$uccisi / Beetles$num, pch = 16)
lines(newdata$logdose, pred_logit, lty = "dashed", col = "black")
lines(newdata$logdose, pred_probit, lty = "dashed", col = "red")
lines(newdata$logdose, pred_cauchit, lty = "dashed", col = "green")
lines(newdata$logdose, pred_cloglog, lty = "dashed", col = "blue")

# QUESTION 5: the graphical analysis and the residual deviances show that the cloglog model has an excellent fit. The cauchit model is much worse. The probit/logit are comparable and both have good fit, but not as good as the cloglog.

# -------------------------------------------------------------------
# Dataset 2: Juice data - APPLIED ANALYSIS
# -------------------------------------------------------------------

rm(list = ls())

# The data on fruit juice purchases are taken from Chapter 11 of Foster, Stine and Waterman "Business Analysis Using Regression".

# The data refer to 1070 fruit juice purchases of two different brands (MM and CH) in certain US supermarkets, supplied with some contributory variables. The variables are

# Variable     Description 

# choice       pre-chosen brand (factor, with 2 levels)
# id.cust      customer identification 
# week         identifier of week of purchase 
# priceCH      reference price for  brand CH (USD)
# priceMM      reference price for  brand MM (USD)
# discountCH   discount applied to  product CH (USD)
# discountMM   discount applied to product MM (USD)
# loyaltyCH    loyalty indicator for  product CH 
# loyaltyMM    loyalty indicator for  product MM 
# store        store identifier (factor, with 5 levels) 

# Variable loyaltyMM is constructed starting from the value 0.5 and updating with every purchase by the same customer, with a value which increases by 20% of the current difference between the current value and 1, if the customer chose MM, and falls by 20% of the  difference between the current value and 0 if the customer chose CH. The corresponding variable loyaltyCH is given by  1-loyaltyMM. 

juice <- read.table("../data/juice.txt", header = TRUE, stringsAsFactors = TRUE)
str(juice)

# QUESTION 1. We are interested in predicting the preference of customers towards CH and MM as a function of relevant covariates. What kind of model could be appropriate? What covariates do you think might be useful?

# QUESTION 2: let us consider the following "wrong" approach, which causes an error. Why do you think it happens?

# A WRONG idea - the "automatic" data scientist
m_wrong <- glm(choice ~ ., family = binomial, data = juice)
summary(m_wrong)

# QUESTION 3: provide some descriptive analysis that may be helpful to understand the relationship between variables and the response. 
