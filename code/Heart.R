# ---------------------------------------------------------------------
# Heart dataset
# DISCLAIMER: This solution is provided in partial form. Certain steps, justifications, and details have been omitted for clarity and brevity.
# Author: Tommaso Rigon
# ---------------------------------------------------------------------

# The data in the Heart dataframe included in the MLGdata library report the number of confirmed myocardial infarctions in a sample of 360 patients hospitalized with suspected infarction. These data were originally collected in Edinburgh Royal Infirmary.

# For each level of the enzyme Creatine Kinase (IU per liter), grouped into classes (ck), the dataset provides:

# the number of confirmed infarctions (ha),
# the number of non-confirmed infarctions (nha),
# and the midpoint value of the variable ck (mck).

# The goal is to evaluate the influence of the Creatine Kinase enzyme level on the probability of infarction.

rm(list = ls())
library(MLGdata)
data("Heart")
str(Heart)

# (a) ---------------------------------------------------------------------

# Omitted

# (b) ---------------------------------------------------------------------

Heart$num <- Heart$ha + Heart$nha
Heart$prop <- Heart$ha / Heart$num

plot(Heart$mck, Heart$prop, pch = 16)
abline(lm(prop ~ mck, data = Heart), lty = "dotted")

# COMMENT: the linear probability model is catastrophically bad. It predicts probabilities > 1, the fit is terrible.

# (c) ---------------------------------------------------------------------

# ha ~ Binomial(num, pi_i), independently, with logit(pi_i) = \beta_1 + \beta_2 mck.

# (d) ---------------------------------------------------------------------

m1 <- glm(cbind(ha, nha) ~ mck, family = binomial, data = Heart)
summary(m1)

# (e) ---------------------------------------------------------------------

coef(m1)
confint(m1)

exp(coef(m1)[2])
exp(100 * coef(m1)[2])

# The value exp(beta_2) associated with mck has the usual interpretation as an odds ratio, which equals 1.03. Although this value may appear small, it should be interpreted in light of the fact that mck ranges roughly from 0 to 500. Therefore, the impact of a unit increase in mck is not particularly meaningful. In contrast, a change of 100 units in mck corresponds to an odds ratio of 33.46, indicating a substantial increase in the probability of infarction.

# (f) ---------------------------------------------------------------------

newdata <- data.frame(mck = seq(from = 0, to = 600, length = 200))
fit1 <- predict(m1, newdata = newdata, type = "response")

plot(Heart$mck, Heart$prop, pch = 16)
lines(newdata$mck, fit1)

# COMMENT: The fit is generally reasonable and in line with expectations, but a few points deviate significantly from the predictions. Further diagnostics may help confirm this issue (see below).

# (g) ---------------------------------------------------------------------

# Omitted

# (h) ---------------------------------------------------------------------

fit150 <- predict(m1, newdata = data.frame(mck = 150), type = "response", se.fit = TRUE)
fit150$fit + c(-1, 1) * qnorm(0.975) * fit150$se.fit

# (i) ---------------------------------------------------------------------

par(mfrow = c(2, 2))
plot(m1, which = 1:4)
par(mfrow = c(1, 1))

round(rstandard(m1, type = "pearson"), 2)
round(cooks.distance(m1), 2)

# COMMENT: There are several issues. Observations 1, 7, and 8 (and possibly 3) are clearly out of place: the standardized Pearson residuals are too large (-3.78, -4.70, -11.34). Observation 1 also has a very large Cook's distance (about 13.49). Note that the predictions are not necessarily poor, but there is some degree of model misspecification.

# Checking for overdispersion, we obtain the following estimate for phi:

phi_hat <- sum(residuals(m1, type = "pearson")^2) / m1$df.residual
phi_hat

# COMMENT: This value is very high and provides strong evidence of overdispersion. Consequently, all confidence intervals and tests performed so far (or to be considered in the following bullent points) are unreliable and overconfident, as the associated standard errors are underestimated.

# (l) ---------------------------------------------------------------------

m2 <- glm(cbind(ha, nha) ~ mck + I(mck^2), family = binomial, data = Heart)
summary(m2)

# Predictions
fit2 <- predict(m2, newdata = newdata, type = "response")
plot(Heart$mck, Heart$prop, pch = 16)
lines(newdata$mck, fit1, lty = "dotted")
lines(newdata$mck, fit2, col = "red")

# Diagnostics
round(rstandard(m2, type = "pearson"), 2)
round(cooks.distance(m2), 2)

# Potential overdispersion
sum(residuals(m2, type = "pearson")^2) / m2$df.residual

# COMMENT: The statistical test supports the idea that we should incorporate a quadratic term. HOWEVER, note that this test is not reliable due to the presence of overdispersion, so the result should be interpreted with caution. From a graphical inspection, the predictions appear to improve, but this is expected whenever a new variable is added to the model. The residual values also improve, and the estimate of the overdispersion parameter decreases substantially, although it remains significantly greater than 1. Finally, note that the quadratic model introduces a spurious deflection in the probability of infarction at high values of mck.

# (m) Evaluate whether the introduction of the quadratic term significantly increases the variability of the estimates. Discuss the interpretability of the quadratic model.

round(sqrt(diag(vcov(m1))), 3)
round(sqrt(diag(vcov(m2))), 3)

predict(m1, newdata = data.frame(mck= c(150, 300)), type = "response", se.fit = TRUE)$se.fit
predict(m2, newdata = data.frame(mck= c(150, 300)), type = "response", se.fit = TRUE)$se.fit

# COMMENT 1: The variability of the estimated coefficients appears to have improved (although the coefficients themselves are not directly comparable at this point). The standard errors of the predicted probabilities have also changed, but the situation is more nuanced: depending on the predicted value, the standard error may either increase or decrease.

# COMMENT 2: The quadratic model may yield better predictions but definitely complicates interpretation. The coefficients of m2 are no longer easily interpretable as log odds ratios.
