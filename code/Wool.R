# ---------------------------------------------------------------------
# Wool dataset
# DISCLAIMER: This solution is provided in a partial form. Certain steps, justifications, and details have been omitted for clarity and brevity.
# ---------------------------------------------------------------------

# The dataset `Wool` (Hand et al., 1994, p. 328) contained in the `MLGdata` library were obtained from an experiment  aimed at evaluating the effect of three variables, length (`x1`), width (`x2`), and load (`x3`), on the number of test cycles until rupture (`y`) of a wool yarn.

# For each of the three variables `x1`, `x2`, and `x3`, three levels were fixed:

# - Length: 250, 300, 350 mm (coded as `-1`, `0`, `1`)
# - Width: 8, 9, 10 mm (coded as `-1`, `0`, `1`)
# - Load: 40, 45, 50 g (coded as `-1`, `0`, `1`)

library(MLGdata)
data(Wool)
str(Wool)
summary(Wool)

# (a) ---------------------------------------------------------------------

# Omitted

# (b) ---------------------------------------------------------------------

# log(Y_i) = \beta_1 + \beta_2 x1 + \beta_3 x2 + \beta_4 x3 + \epsilon_i
# where \epsilon_i are iid normal with 0 mean and variance \sigma^2.

# What follows are some basic descriptive statistics (not required, but still useful)

boxplot(log(y) ~ x1, data = Wool)
boxplot(log(y) ~ x2, data = Wool)
boxplot(log(y) ~ x3, data = Wool)

# (c) ---------------------------------------------------------------------

m_log <- lm(log(y) ~ x1 + x2 + x3, data = Wool)
summary(m_log)

# (d) ---------------------------------------------------------------------

par(mfrow = c(2, 2))
plot(m_log, which = 1:4)
par(mfrow = c(1, 1))

plot(exp(fitted(m_log)), Wool$y, pch = 16, xlab = "Predicted values", ylab = "Observed values")
abline(c(0, 1), lty = "dotted")

library(sandwich)
library(lmtest)
coeftest(m_log)
coeftest(m_log, vcov. = vcovHC(m_log))

# COMMENT: The diagnostic plots appear generally satisfactory, with no major issues. There may be some (very) mild heteroskedasticity, but overdispersion is clearly negligible: the conventional and robust standard errors are nearly identical, and the inferential conclusions remain unchanged.

m_lin <- lm(y ~ x1 + x2 + x3, data = Wool)
library(MASS)
boxcox(m_lin)

# COMMENT: the Box-Cox transform actually support the log-transformation.

# (e) ---------------------------------------------------------------------

# E(log(Y_i)) = 6.3346 + 0.832384 x1 - 0.630992 x2 - 0.392494 x3.

# (f) ---------------------------------------------------------------------

newdata <- data.frame(x1 = 0, x2 = -1, x3 = -1)

# Confidence interval - This is NOT a Wald confidence interval, but it is a valid confidence interval
exp(predict(m_log, newdata = newdata, interval = "confidence"))

# Prediction interval
exp(predict(m_log, newdata = newdata, interval = "prediction"))

# COMMENT: the value "fit" is not the mean of Y_i, but it is nonetheless a "reasonable prediction".

# (g) ---------------------------------------------------------------------

# Y_i ~ Gamma(\mu_i, \phi),   E(Y_i) = \mu_i = exp(\beta_1 + \beta_2 x1 + \beta_3 x2 + \beta_4 x3)

# (h) ---------------------------------------------------------------------

m_gamma <- glm(y ~ x1 + x2 + x3, family = Gamma(link = "log"), data = Wool)
summary(m_gamma)

# (i) ---------------------------------------------------------------------

# E(Y_i) = exp(6.34891 + 0.84251 x1 - 0.63132 x2 - 0.38513 x3).

# (j) Report the estimates and confidence intervals for the coefficients. Provide an interpretation of the obtained values.

coef(m_gamma) # summary(m_gamma) is also ok
confint(m_gamma)

# COMMENT: All estimated coefficients are significantly different from zero. Moreover:

# Interpretation of \beta1 (intercept). The value
exp(coef(m_gamma)[1]) # 571.874
# represents the expected number of test cycles until rupture for a wool yarn with x_1 = x_2 = x_3 = 0, corresponding to the "typical" wool yarn.

# Interpretation of \beta2 (x1). The value
100 * (exp(coef(m_gamma)[2]) - 1) # 132.2%
# represents the percentage increase in the average number of test cycles until rupture when x_1 increases by one unit (i.e., from −1 to 0, or from 0 to 1). This indicates that longer wool yarns tend to withstand, on average, a higher number of test cycles before rupture.

# Interpretation of \beta3 (x2). The value
100 * (exp(coef(m_gamma)[3]) - 1) # -46.8%
# represents the percentage decrease in the average number of test cycles until rupture when x_2 increases by one unit (i.e., from −1 to 0, or from 0 to 1). This indicates that wider wool yarns tend to withstand, on average, a lower number of test cycles before rupture.

# Interpretation of \beta4 (x2). The value
100 * (exp(coef(m_gamma)[4]) - 1) # -31.9%
# represents the percentage decrease in the average number of test cycles until rupture when x_3 increases by one unit (i.e., from −1 to 0, or from 0 to 1). This indicates that more loaded wool yarns tend to withstand, on average, a lower number of test cycles before rupture.

# (k) ---------------------------------------------------------------------

# Omitted

# (l) ---------------------------------------------------------------------

par(mfrow = c(2, 2))
plot(m_gamma, which = 1:4)
par(mfrow = c(1, 1))

plot(fitted(m_gamma), Wool$y, pch = 16, xlab = "Predicted values", ylab = "Observed values")
abline(c(0, 1), lty = "dotted")

# COMMENT: the diagnostic plots are fully satisfactory with no major issues.

# (m) ---------------------------------------------------------------------

# Wald CI, based on the delta method
m_pred <- predict(m_gamma, se.fit = TRUE, newdata = newdata, type = "response")

CI_gamma <- m_pred$fit + c(-1, 1) * qnorm(0.975) * m_pred$se.fit
CI_gamma

# (n) ---------------------------------------------------------------------

fit_log <- exp(fitted(m_log))
fit_gamma <- fitted(m_gamma)

# The predictions of the two models are nearly indistinguishable.
plot(fit_log, fit_gamma, pch = 16)
abline(c(0, 1))

# The estimated values and the standard errors are also nearly identical:
coeftest(m_log)
coeftest(m_gamma)

# The correlation with the response are also extremely similar
cor(fit_log, Wool$y)
cor(fit_gamma, Wool$y)

# In practice, these two models yield almost identical results. I would personally favor the gamma model for its conceptual clarity—since the response is positive and the coefficients directly relate to the mean—but this is admittedly a subjective preference.
