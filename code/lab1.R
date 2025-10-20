# LAB 1, Statistica III (Generalized Linear Models)
# Title: "Linear models and misspecification". Datasets: Neonati, Clotting
# Author: Tommaso Rigon

# -------------------------------------------------------------------
# Dataset 1: Neonati
# -------------------------------------------------------------------

rm(list = ls())
library(MLGdata)

# Weight at birth, gestational age, maternal smoking for n = 32 newborns
# Source: Daniel, W.W. (1999). Biostatistics: A Foundation for Analysis in Health Sciences
data(Neonati)

str(Neonati)

# Peso (g) is the response variable (Weight of the baby, in grams)
# Durata is the gestational age (weeks)
# Fumo is a factor variable indicating whether the mother smokes (F = smoker, NF = non-smoker)

# QUESTION 1: does the weight of newborns depend on gestational age?
# QUESTION 2: does the smoking habit of the mother affect the birth weight?
# QUESTION 3: is there an interaction between gestational age and maternal smoking?
# QUESTION 4: provide a predictive interval for a baby born 40 weeks of gestation, whose mother is a smoker and another one whose mother is a non-smoker.

# Descriptive statistics
summary(Neonati)

# Scatter plot: weight vs gestational age, colored by maternal smoking
plot(Neonati$durata, Neonati$peso,
  col = Neonati$fumo,
  xlab = "Gestational Age", ylab = "Weight (g)", pch = 16
)
abline(v = 37, lty = "dotted") # Pre-term birth

# Check encoding of factor variable
contrasts(Neonati$fumo)

# Fit linear model
m1 <- lm(peso ~ durata + fumo, data = Neonati)

# Inspect model matrix
head(Neonati) # Original data
head(model.matrix(~ durata + fumo, data = Neonati)) # Design matrix

# Model summary
summary(m1)

# Confidence intervals for regression coefficients (95% level)
confint(m1, level = 0.95)

# Diagnostic plots: residuals, leverage, etc.
par(mfrow = c(2, 2))
plot(m1, which = 1:4)

# Reset plotting layout
par(mfrow = c(1, 1))

# Model predictions vs. observed data
plot(Neonati$durata, Neonati$peso,
  col = Neonati$fumo,
  xlab = "Gestational Age", ylab = "Weight (g)", pch = 16
)

# Add regression lines for each smoking group
abline(a = coef(m1)[1], b = coef(m1)[2], lty = "dashed") # Non-smokers
abline(a = coef(m1)[1] + coef(m1)[3], b = coef(m1)[2], lty = "dashed", col = "red") # Smokers

# QUESTION 1: Yes, gestational age significantly affects birth weight (p-value about 0). Each additional week increases weight by about 140 g (beta coefficient), on average and all else equal.

# QUESTION 2: Yes, smoking has a significant negative effect on birth weight (p-value about 0). On average, infants of smoking mothers weigh about 244 g less than those of non-smoking mothers, all else equal.

# Model 2, let us consider an interaction term

# Fit linear model with interaction
m2 <- lm(peso ~ durata * fumo, data = Neonati)

# Inspect design matrix
head(model.matrix(~ durata * fumo, data = Neonati))

# Interpretation:
# Expected birth weight is a linear function of gestation length,
# with different intercepts and slopes for infants of non-smoking vs. smoking mothers

summary(m2)

# Note:
# - beta_4 (interaction term) may be omitted if not significant
# - beta_3 may change significance due to collinearity with interaction term

# Compare models m1 (additive) vs. m2 (interaction)
anova(m1, m2)
# Question: Why is the p-value identical to that in summary(m2)? (Not a coincidence; same F-test)

# Plot observed data and predicted regression lines
plot(Neonati$durata, Neonati$peso,
  col = Neonati$fumo,
  xlab = "Gestational Age", ylab = "Weight (g)", pch = 16
)

abline(a = coef(m2)[1], b = coef(m2)[2], lty = "dashed") # Non-smokers
abline(
  a = coef(m2)[1] + coef(m2)[3], b = coef(m2)[2] + coef(m2)[4],
  lty = "dashed", col = "red"
) # Smokers

# QUESTION 3: No, there is no significant interaction between gestational age and maternal smoking (p-value = 0.66). The effect of gestational age on birth weight appears similar for both groups.

# Predictions and 95% confidence intervals at 40 weeks
predict(m1,
  newdata = data.frame(fumo = c("F", "NF"), durata = rep(40, 2)),
  interval = "confidence", level = 0.95
)

# QUESTION 4: Predictions and 95% prediction intervals at 40 weeks, for a mother being smoker and non-smoker
predict(m1,
  newdata = data.frame(fumo = c("F", "NF"), durata = rep(40, 2)),
  interval = "prediction", level = 0.95
)

# Current contrasts setup
contrasts(Neonati$fumo) # Shows current coding
contr.treatment(n = 2, base = 1) # Current baseline (default)
contr.treatment(n = 2, base = 2) # Change baseline to second level

# Change baseline and refit the model
contrasts(Neonati$fumo) <- contr.treatment(n = 2, base = 2)

# Inspect data and design matrix
head(Neonati) # Original data
head(model.matrix(~ durata + fumo, data = Neonati)) # Design matrix

# Refit model with new baseline
m3 <- lm(peso ~ durata + fumo, data = Neonati)
summary(m3)

# Zero-sum contrasts (less common but useful)
contr.sum(n = 2)
contrasts(Neonati$fumo) <- contr.sum(n = 2)

# Inspect design matrix under zero-sum contrasts
head(Neonati)
head(model.matrix(~ durata + fumo, data = Neonati))

# Refit model with zero-sum contrasts
m4 <- lm(peso ~ durata + fumo, data = Neonati)
summary(m4)

# -------------------------------------------------------------------
# Dataset 2: Clotting
# -------------------------------------------------------------------

rm(list = ls())

library(MLGdata)
data("Clotting")

str(Clotting)

# Log-transform plasma concentration
Clotting$logu <- log(Clotting$u)

# Scatter plot: clotting time vs log-concentration
plot(Clotting$logu, Clotting$tempo,
  col = Clotting$lotto,
  xlab = "Log-plasma concentration", ylab = "Clotting time (s)", pch = 16
)

# Observations:
# 1. Non-linear relationship
# 2. Response variable always positive

# Approach 0: fit linear model anyway
m0 <- lm(tempo ~ logu * lotto, data = Clotting)
summary(m0)

# Predictions
newdata <- data.frame(
  logu = rep(seq(1.5, 4.8, length = 100), each = 2),
  lotto = rep(c("uno", "due"), 100)
)
pred_m0 <- predict(m0, newdata = newdata)
lines(newdata$logu[newdata$lotto == "uno"], pred_m0[newdata$lotto == "uno"], lty = "dashed", col = "red")
lines(newdata$logu[newdata$lotto == "due"], pred_m0[newdata$lotto == "due"], lty = "dashed")

# Diagnostics: non-linearity and influential points
par(mfrow = c(2, 2))
plot(m0, which = 1:4)
par(mfrow = c(1, 1))

# Approach 1: Box-Cox suggests reciprocal transformation
MASS::boxcox(m0)
m1 <- lm(I(1 / tempo) ~ logu * lotto, data = Clotting)
summary(m1)

pred_m1 <- 1 / predict(m1, newdata = newdata)
plot(Clotting$logu, Clotting$tempo,
  col = Clotting$lotto,
  xlab = "Log-plasma concentration", ylab = "Clotting time (s)", pch = 16
)
lines(newdata$logu[newdata$lotto == "uno"], pred_m1[newdata$lotto == "uno"], lty = "dashed", col = "red")
lines(newdata$logu[newdata$lotto == "due"], pred_m1[newdata$lotto == "due"], lty = "dashed")

# Diagnostics
par(mfrow = c(2, 2))
plot(m1, which = 1:4)
par(mfrow = c(1, 1))

# Robust standard errors for heteroscedasticity
library(lmtest)
library(sandwich)
coeftest(m1)
coeftest(m1, vcov. = vcovHC(m1))
coefci(m1, vcov. = vcovHC(m1))

# Approach 2: log-transform with quadratic term (variance-stabilizing)
m2 <- lm(I(log(tempo)) ~ logu + I(logu^2) + lotto, data = Clotting)
summary(m2)

pred_m2 <- exp(predict(m2, newdata = newdata))
plot(Clotting$logu, Clotting$tempo,
  col = Clotting$lotto,
  xlab = "Log-plasma concentration", ylab = "Clotting time (s)", pch = 16
)
lines(newdata$logu[newdata$lotto == "uno"], pred_m2[newdata$lotto == "uno"], lty = "dashed", col = "red")
lines(newdata$logu[newdata$lotto == "due"], pred_m2[newdata$lotto == "due"], lty = "dashed")

# Diagnostics
par(mfrow = c(2, 2))
plot(m2, which = 1:4)
par(mfrow = c(1, 1))

# Compare prediction performance
fit0 <- predict(m0)
fit1 <- 1 / predict(m1)
fit2 <- exp(predict(m2))

# "R-squared" in original scale
1 - sum((Clotting$tempo - fit0)^2) / sum((Clotting$tempo - mean(Clotting$tempo))^2)
1 - sum((Clotting$tempo - fit1)^2) / sum((Clotting$tempo - mean(Clotting$tempo))^2)
1 - sum((Clotting$tempo - fit2)^2) / sum((Clotting$tempo - mean(Clotting$tempo))^2)

coeftest(m2)
coeftest(m2, vcov. = vcovHC(m2))

coefci(m2)
coefci(m2, vcov. = vcovHC(m2))
