# LAB 1, Statistica III (Generalized Linear Models)
# Title: "Linear models and misspecification". Datasets: Neonati, Clotting
# Author: Tommaso Rigon

# Dataset 1: Neonati --------------------------------------------------------------
# This is an example in which all the assumptions of LM are satisfied

library(MLGdata) # Library containing the dataset we need
rm(list = ls())

data(Neonati) # Weight at birth, data on the weight at birth. The duration of the gestation, and the smoke habit of the mother for n = 32 newborns. Daniel, W.W. (1999). Biostatistics: A Foundation for Analysis in the Health Sciences. New York: Wiley.

str(Neonati)
# View(Neonati) # Appropriate only if you have a small amount of data points

# Do not rush into modelling. Always start with some descriptive statistics.
summary(Neonati)

plot(Neonati$durata, Neonati$peso,
  col = Neonati$fumo,
  xlab = "Gestational Age", ylab = "Weight (g)", pch = 16
)
abline(v = 37, lty = "dotted") # Pre-term birth

# A linear model seems appropriate. There is a factor variable, therefore we may want to check its encoding
contrasts(Neonati$fumo)

# Fitting procedure
m1 <- lm(peso ~ durata + fumo, data = Neonati)

# The matrix X should be coherent with our expectations
head(Neonati) # Original data
head(model.matrix(~ durata + fumo, data = Neonati)) # Matrix X

summary(m1)

# $R^2$ is fairly high, and all regression coefficients are significantly different from zero. Confidence intervals for the model parameters can also be obtained.

confint(m1, level = 0.95)

# Diagnostic
par(mfrow = c(2, 2))
plot(m1, which = 1:4)

# Model predictions
par(mfrow = c(1, 1))
plot(Neonati$durata, Neonati$peso,
  col = Neonati$fumo,
  xlab = "Gestational Age", ylab = "Weight (g)", pch = 16
)

abline(a = coef(m1)[1], b = coef(m1)[2], lty = "dashed") # Non-smokers prediction
abline(a = coef(m1)[1] + coef(m1)[3], b = coef(m1)[2], lty = "dashed", col = "red") # Non-smokers prediction

# Model 2, let us consider an interaction term

m2 <- lm(peso ~ durata * fumo, data = Neonati)
head(model.matrix(~ durata * fumo, data = Neonati)) # Matrix X

# According to this model, expected birth weight is a linear function of gestation length, with different intercepts and slopes for infants of non-smoking and smoking mothers.

summary(m2)

# The additional term beta_4 can be probably omitted. Why do you think beta_3 is non significant anymore? Does it mean we should remove it?

# A similar conclusione can be obtained from here. Why the p-value is identical to that obtained in the summary? Is it a coincidence?
anova(m1, m2)

# Note that the model m2 would give the following predictions
plot(Neonati$durata, Neonati$peso,
  col = Neonati$fumo,
  xlab = "Gestational Age", ylab = "Weight (g)", pch = 16
)

# Predicted values
abline(a = coef(m2)[1], b = coef(m2)[2], lty = "dashed") # Non-smokers prediction
abline(a = coef(m2)[1] + coef(m2)[3], b = coef(m2)[2] + coef(m2)[4], lty = "dashed", col = "red") # Non-smokers prediction

# Predictions and confidence intervals
predict(m1, newdata = data.frame(
  fumo = c("F", "NF"),
  durata = rep(40, 2)
), interval = "confidence", level = 0.95)

# Predictions and prediction intervals
predict(m1, newdata = data.frame(
  fumo = c("F", "NF"),
  durata = rep(40, 2)
), interval = "prediction", level = 0.95)

# Playing with contrasts

contrasts(Neonati$fumo) # This is the current setup, that can be obtained as follows

contr.treatment(n = 2, base = 1) # Current situation
contr.treatment(n = 2, base = 2) # This changes the baseline

# Let us change it and refit the model
contrasts(Neonati$fumo) <- contr.treatment(n = 2, base = 2) # This changes the baseline

head(Neonati) # Original data
head(model.matrix(~ durata + fumo, data = Neonati)) # Matrix X

# Refitting the model
m3 <- lm(peso ~ durata + fumo, data = Neonati)
summary(m3)

# Is this a different model? Do we get different predictions?
# What if we run anova(m1, m3)?

# Zero-sum contrasts. This is less common, but very useful
contr.sum(n = 2)
contrasts(Neonati$fumo) <- contr.sum(n = 2) # This changes the baseline

head(Neonati) # Original data
head(model.matrix(~ durata + fumo, data = Neonati)) # Matrix X

# What is the interpretation of the parameters now?
m4 <- lm(peso ~ durata + fumo, data = Neonati)
summary(m4)

# Dataset 2: Clotting--------------------------------------------------------------------
rm(list = ls())

# Blood clotting times. Mean blood clotting times in seconds for nine percentage concentrations of normal plasma and two lots of clotting agent. McCullagh, P. and Nelder, J. A. (1989) Generalized Linear Models (2nd Edition). London: Chapman and Hall.
data("Clotting")

str(Clotting)

# We consider from the very beginning the log-plasma concentration
Clotting$logu <- log(Clotting$u)

plot(Clotting$logu, Clotting$tempo,
  col = Clotting$lotto,
  xlab = "Log-plasma concentration", ylab = "Clotting time (s)", pch = 16
)

# Several considerations:
# 1. The relationship is quite clearly non-linear.
# 2. The response variable, clotting time, is always positive.

# Approach 0, fit a linear model anyway.
m0 <- lm(tempo ~ logu * lotto, data = Clotting)

# A "wrong" model can still provide fairly good predictions
summary(m0)

# Data at which we would like to perform the predictions
newdata <- data.frame(
  logu = rep(seq(from = 1.5, to = 4.8, length = 100), each = 2),
  lotto = rep(c("uno", "due"), 100)
)
head(newdata)

pred_m0 <- predict(m0, newdata = newdata)
lines(newdata$logu[newdata$lotto == "uno"], pred_m0[newdata$lotto == "uno"], lty = "dashed", col = "red")
lines(newdata$logu[newdata$lotto == "due"], pred_m0[newdata$lotto == "due"], lty = "dashed")

# Diagnostic: the model display some issues:
par(mfrow = c(2, 2))
plot(m0, which = 1:4)
par(mfrow = c(1, 1))

# The first plot clearly shows a non-linear pattern. Moreover, the Cook's distance signals the presence of an influence point (the first observation).
# Here we are violating assumption A.1, i.e. linearity. The "outlier" is just a consequence of the bad fit.

# Approach 1, rely on the Box-Cox transformation
MASS::boxcox(m0)

# The Box-Cox transform suggest to consider the reciprocal transformation, namely to consider the following model
m1 <- lm(I(1 / tempo) ~ logu * lotto, data = Clotting)

summary(m1) # The R2, in the trasformed scale, is almost perfect.

# Predictions
plot(Clotting$logu, Clotting$tempo,
  col = Clotting$lotto,
  xlab = "Log-plasma concentration", ylab = "Clotting time (s)", pch = 16
)

pred_m1 <- 1 / predict(m1, newdata = newdata) # Remark: these are "predictions", but they do not coincide with an estimate for the mean of E(Y)
lines(newdata$logu[newdata$lotto == "uno"], pred_m1[newdata$lotto == "uno"], lty = "dashed", col = "red")
lines(newdata$logu[newdata$lotto == "due"], pred_m1[newdata$lotto == "due"], lty = "dashed")

# Diagnostic: the predicted values are good there are probably still some small heteroschedasticity issue (i.e., the last three observations)
par(mfrow = c(2, 2))
plot(m1, which = 1:4)
par(mfrow = c(1, 1))

# This might have an effect, for example, on the confidence intervals. Are they too narrow?
library(lmtest) # Using a different library, which we are going to need later
coeftest(m1) # Equivalent of summary(m1)
coefci(m1) # Equivalent of confint(m1)

# Adjust for heteroschedasticity using White's standard errors
library(sandwich)

vcov(m1) # Original covariance matrix
vcovHC(m1) # Heteroschedasticity corrected covariance matri

coeftest(m1, vcov. = vcovHC(m1)) # Equivalent of summary(m1)
coefci(m1, vcov. = vcovHC(m1)) # Equivalent of confint(m1)

# The wider standard errors suggests that the main effect could be negligible. However, for the sake of facilitating the interpretation, we can also keep it as is.

# Approach 2
# Variance stabilizing transformation: let us take the log, assuming a Gamma model

# Plot in the trasformed scale
plot(Clotting$logu, log(Clotting$tempo),
  col = Clotting$lotto,
  xlab = "Log-plasma concentration", ylab = "Log-Clotting time (s)", pch = 16
)

# From a graphical inspection, it looks there is some curvature
m2 <- lm(I(log(tempo)) ~ logu + I(logu^2) + lotto, data = Clotting)
summary(m2)

# Predictions
pred_m2 <- exp(predict(m2, newdata = newdata)) # As before, these are "predictions", but they do not coincide with an estimate for the mean of E(Y)
plot(Clotting$logu, Clotting$tempo,
  col = Clotting$lotto,
  xlab = "Log-plasma concentration", ylab = "Clotting time (s)", pch = 16
)
lines(newdata$logu[newdata$lotto == "uno"], pred_m2[newdata$lotto == "uno"], lty = "dashed", col = "red")
lines(newdata$logu[newdata$lotto == "due"], pred_m2[newdata$lotto == "due"], lty = "dashed")

# We considered a variance/stabilizing transform, therefore (if the gamma hypothesis is roughly correct), we expect no additional heteroschedasticity

# Diagnostic are fairly reasonable
par(mfrow = c(2, 2))
plot(m2, which = 1:4)
par(mfrow = c(1, 1))

# There are no substantial changes
coeftest(m2, vcov. = vcov(m2)) # Equivalent of summary(m1)
coeftest(m2, vcov. = vcovHC(m2)) # Equivalent of summary(m1)

# Overall, who get the best predictions?
fit0 <- predict(m0) # Wrong model
fit1 <- 1 / predict(m1) # Reciprocal transform + interaction
fit2 <- exp(predict(m2)) # Logarithmic model + quadratic term

# "R-squared" in the original scale
1 - sum((Clotting$tempo - fit0)^2) / sum((Clotting$tempo - mean(Clotting$tempo))^2)
1 - sum((Clotting$tempo - fit1)^2) / sum((Clotting$tempo - mean(Clotting$tempo))^2)
1 - sum((Clotting$tempo - fit2)^2) / sum((Clotting$tempo - mean(Clotting$tempo))^2)
