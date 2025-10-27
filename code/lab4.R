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
# ...          other variables obtained by combining the former

# Variable loyaltyMM is constructed starting from the value 0.5 and updating with every purchase by the same customer, with a value which increases by 20% of the current difference between the current value and 1, if the customer chose MM, and falls by 20% of the  difference between the current value and 0 if the customer chose CH. The corresponding variable loyaltyCH is given by  1-loyaltyMM.

juice <- read.table("../data/juice.txt", header = TRUE, stringsAsFactors = TRUE)
juice <- read.table("https://tommasorigon.github.io/StatIII/data/juice.txt", header = TRUE, stringsAsFactors = TRUE)
str(juice)

# QUESTION 1. We are interested in predicting the preference of customers towards CH and MM as a function of relevant covariates. What kind of model could be appropriate? What covariates do you think might be useful?

# QUESTION 2: let us consider the following "wrong" approach, which causes an error. Why do you think it happens?

# A WRONG idea - the "automatic" data scientist
m_wrong <- glm(choice ~ ., family = binomial, data = juice)
summary(m_wrong)

# QUESTION 3: Clean the data and provide some descriptive analysis that may be helpful to understand the relationship between variables and the response.

# COMMENT: in the dataset there are (i) irrelevant variables (ii) leaker variables such as buyCH (iii) redundant variables.

# buyCH and choice represent the same variable in different formats (factor vs int). One or the other must be removed
juice <- subset(juice, select = -c(buyCH))

# loyaltyCH and loyaltyMM are collinear, we cannot use both.

# id.cust is difficult to use, because there a lot of customers and very few observations per id
juice <- subset(juice, select = -c(id.cust))

# store, StoreID, store7 are referring to the same quantity
juice <- subset(juice, select = -c(StoreID, store7))
juice$store <- as.factor(juice$store)

# salepriceCH is the FINAL price, obtained as priceCH - discountCH, making these three variables COLLINEAR.
# We can keep them but we cannot use them together
plot(juice$priceCH - juice$discountCH, juice$salepriceCH)
plot(juice$priceMM - juice$discountMM, juice$salepriceMM)

# pricediff and listpricediff are also collinear variables, being equal to
# pricediff = salepriceMM - salepriceCH;
# listpricediff = priceMM - priceCH
plot(juice$salepriceMM - juice$salepriceCH, juice$pricediff)
plot(juice$priceMM - juice$priceCH, juice$listpricediff)

# pctdiscMM and pctdiscCH are also potentially problematic, albeit non-collinear. specialCH are "special weeks", again potentially problematic

# Descriptive statistics.

# Loyalty seems to be a MAJOR factor
boxplot(loyaltyCH ~ choice, data = juice)

# The difference in price is less obvious if it's important or not
boxplot(pricediff ~ choice, data = juice)
boxplot(listpricediff ~ choice, data = juice)

# QUESTION 4: identify a good model using a subset of the total number variables. Can we use all the available variables? Why not?

# Let us consider a FORWARD approach for building this model. The backward does not work (!)
m0 <- glm(choice ~ 1, family = binomial, data = juice)
summary(m0)

# Variables we would like to POTENTIALLY consider
scope <- choice ~ week + priceCH + priceMM + discountCH + discountMM + specialCH + specialMM + loyaltyCH + loyaltyMM + salepriceMM + salepriceCH + pricediff + pctdiscMM + pctdiscCH + listpricediff + store

# Which variable is the most "relevant", meaning that reduced the deviance the most?
add1(m0, scope = scope, test = "LRT")

# Loyalty is the most relevance variable. Let us include it (one of them, because they are obviously collinear)
m1 <- glm(choice ~ loyaltyMM, family = binomial, data = juice)
summary(m1)  

add1(m1, scope = scope, test = "LRT")
m2 <- glm(choice ~ loyaltyMM + pricediff, family = binomial, data = juice)
summary(m2)

add1(m2, scope = scope, test = "LRT")

m3 <- glm(choice ~ loyaltyMM + pricediff + store, family = binomial, data = juice)
summary(m3)
add1(m3, scope = scope, test = "LRT")

# Fully automatic procedures

# Forward selection (k = 2 is the AIC)
m_forward <- step(m0, scope = scope, direction = "forward", k = 2)
summary(m_forward)

# Stepwise selection
m_step <- step(m0, scope = scope, direction = "both", k = 2)
summary(m_step)

# QUESTION 5: evaluate the goodness of fit of the estimated model.

# Diagnostic plots are not usuful for binary data
par(mfrow = c(2, 2))
plot(m3, which = 1:4) # As usual, these graphs are not particularly informative
par(mfrow = c(1, 1))

pred_m3 <- predict(m3, type = "response")
class_m3 <- as.factor(pred_m3 > 0.5)

# Confusion matrix
table(class_m3, juice$choice)

# Accuracy of the model is about 83%:
sum(diag(table(juice$choice, class_m3))) / nrow(juice)

# Error is 1 - accuracy, is about 16%:
1 - sum(diag(table(juice$choice, class_m3))) / nrow(juice)

# Calibration plot
breaks <- seq(from = 0, to = 1, length = 20)
class_m3 <- cut(pred_m3, breaks = breaks)

pred_avg <- tapply(pred_m3, class_m3, mean)
prop <- tapply(as.numeric(juice$choice) - 1, class_m3, mean)

plot(prop, pred_avg, pch = 16, xlab = "Empirical proportions (binned)", ylab = "Predicted proportions (binned)")
abline(c(0, 1), lty = "dotted")
