# ---------------------------------------------------------------------
# Abrasion dataset
# DISCLAIMER: This solution is provided in partial form. Certain steps, justifications, and details have been omitted for clarity and brevity.
# Authors: Alice Giampino, Tommaso Rigon
# ---------------------------------------------------------------------

rm(list = ls())

library(MLGdata)
data(Abrasion)

head(Abrasion)

# (a) ---------------------------------------------------------------------

nrow(Abrasion)
str(Abrasion)

# (b) ---------------------------------------------------------------------

plot(Abrasion)

par(mfrow = c(1, 2))
plot(Abrasion$D, Abrasion$perdita, xlab = "D (Shore)", ylab = "Weight loss")
abline(lm(perdita ~ D, data = Abrasion))

plot(Abrasion$Re, Abrasion$perdita, xlab = "Re (kg/cm^2)", ylab = "Weight loss")
abline(lm(perdita ~ Re, data = Abrasion))
par(mfrow = c(1, 1))

cor(Abrasion)

# COMMENT: The data points closely follow a linear pattern. The response variable is positive, but there is little practical risk of obtaining negative predictions.

# (c) ---------------------------------------------------------------------

# perdita_i = \beta_1 + \beta_2 D_i + \epsilon_i,     i = 1, ... , 30
# eps_i ~ N(0, sigma2)

# COMMENT: D_i is chosen because is the most correlated variable.

# (d) ---------------------------------------------------------------------

m_D <- lm(perdita ~ D, data = Abrasion)
summary(m_D)

# (e) ---------------------------------------------------------------------

coef(m_D)
confint(m_D, level = 0.95)

# COMMENT: A unitary increase in hardness implies a decrease in weight loss of about 7 grams per hour. The coefficient is significanlty different from zero. 

# (f) ---------------------------------------------------------------------

# Omitted

# (g) ---------------------------------------------------------------------

par(mfrow = c(2, 2))
plot(m_D, which = 1:4)
par(mfrow = c(1, 1))

# COMMENT: The diagnostic plots do not reveal anything particularly concerning; the errors are not necessarily Gaussian, but they appear homoskedastic, and there are no outliers or high-leverage points.

# HOWEVER, let us consider the variable "Re".
plot(Abrasion$Re, residuals(m_D), xlab = "Re")  

# The correct plot is the so-called added-variable plot (see also the exercises), in which we compare two sets of residuals. They appear to be correlated.
plot(residuals(lm(Re ~ D, data = Abrasion)), residuals(m_D), xlab = "Re")  

# COMMENT: The variable "Re" is related to the residuals, but it should not be! This suggests that we should include it in the model.

# (h) ---------------------------------------------------------------------

m_full <- lm(perdita ~ D + Re, data = Abrasion)
summary(m_full)

# COMMENT: A unitary increase in hardness/resistenace implies a decrease in weight loss of about 6 grams per hour and 1.37 grams per hour, respectively. Both coefficients are significanlty different from zero. 

# (i) ---------------------------------------------------------------------

par(mfrow = c(2, 2))
plot(m_full, which = 1:4)
par(mfrow = c(1, 1))

# COMMENT: The F-test statistic suggests rejecting the null hypothesis that all coefficients are zero; as before, the diagnostic plots do not reveal anything particularly concerning, the errors are not necessarily Gaussian, but they appear homoskedastic, and there are no outliers or high-leverage points.

# (j) ---------------------------------------------------------------------

new <- data.frame(D = 70, Re = 180)
predict(mod, newdata = new, interval = "confidence", level = 0.95)
predict(mod, newdata = new, interval = "prediction", level = 0.95)
