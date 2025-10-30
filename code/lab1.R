# LAB 1, Statistica III (Generalized Linear Models)
# Title: "Linear models and misspecification". Datasets: Neonati, Clotting
# Author: Tommaso Rigon

# -------------------------------------------------------------------
# Dataset 1: Neonati - APPLIED ANALYSIS
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

# Inspect design matrix
head(model.matrix(~ durata * fumo, data = Neonati))

# Fit linear model with interaction
m2 <- lm(peso ~ durata * fumo, data = Neonati)

summary(m2)

# Compare models m1 (additive) vs. m2 (interaction)
anova(m1, m2)
# The interaction term is not relevant

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

# -------------------------------------------------------------------
# Dataset 2: Clotting - THEORY AND MODELLING
# -------------------------------------------------------------------

rm(list = ls())

# Load data: clotting times (seconds) for nine plasma concentrations and two clotting agents
# Source: McCullagh & Nelder (1989), Generalized Linear Models, 2nd Edition
library(MLGdata)
data("Clotting")

str(Clotting)

# QUESTION 0: Create a new variable called logu which corresponds to the logarithm of the plasma concentration
# QUESTION 1: Explore the relationship between clotting time and plasma concentration. Is a linear model appropriate?
# QUESTION 2: Propose alternative modeling strategies to improve model fit. Make use to the Box-Cox transformation, of variance-stabilizing transformations, and other tools at your disposal to fix potential heteroskedasticity issues.

# Log-transform plasma concentration
Clotting$logu <- log(Clotting$u)

# Scatter plot: clotting time vs log-concentration
plot(Clotting$logu, Clotting$tempo,
  col = Clotting$lotto,
  xlab = "Log-plasma concentration", ylab = "Clotting time (s)", pch = 16
)

# Approach 0: fit linear model anyway (with an interaction term)
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

# QUESTION 1. The linear model is definitely NOT APPROPRIATE. The predictions are inaccurate, the diagnostics are terrible.

# Alternative approach 1: Box-Cox transformation, which suggests the reciprocal transformation
library(MASS)
boxcox(m0)

# Is it a good idea? Let's confirm it by having a look at the plot
plot(Clotting$logu, 1 / Clotting$tempo,
  col = Clotting$lotto,
  xlab = "Log-plasma concentration", ylab = "Reciprocal of clotting time (1/s)", pch = 16
)
# In the transformed scale, the relationship looks indeed much more linear

# Let us fit the suggested model
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

# COMMENT: the predictions are significantly improved compared to the linear model m0. There is MAYBE some heteroskedasticity (look at observations 16, 17, and 18 from the residual plots), which we may correct using robust standard-errors. Otherwise, this is a decent model.

# Robust standard errors for heteroskedasticity
library(lmtest)
library(sandwich)
coeftest(m1)
coeftest(m1, vcov. = vcovHC(m1))
coefci(m1, vcov. = vcovHC(m1))

# The adjusted standard errors do not alter the main conclusions: logu and lotto are significant predictors.
# The main effect of "lotto" becomes not significant after the heteroskedasticity correction, but for interpretability reasons I would not remove it from the model (at least as long as the interaction term is present).

# Alternative approach 2: log-transform (variance-stabilizing transform, assuming a gamma model)

# Let us first have a look at the log-transformation. This is motivated by the fact that if the response is Gamma distributed, then a log-transformation stabilizes the variance (making it homoskedastic).
plot(Clotting$logu, log(Clotting$tempo),
  col = Clotting$lotto,
  xlab = "Log-plasma concentration", ylab = "Logarithm of Clotting time", pch = 16
)

# The relationship looks non-linear, but it may be fixed with a quadratic term
# Moreover, it does not seem we need an interaction between logu and lotto.
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

# COMMENT: the predictions are extremely accurate, and the diagnostics do not show major issues. However, there is a single observation (the first one) which has a very high Cook's distance, indicating that it is highly influential and with high residual. We should not remove it, because it is not a "contaminated" data: it is simply a data point that we fail to accurately predict. This is probably an indication of some form of misspecification at the extreme low values of logu (maybe the response variable was not a gamma? maybe the relationship is not quadratic? with this limited amount of data it is hard to say).

# With that said, this is an excellent model in terms of prediction accuracy. We can "forgive" the presence of a single influential point, given the overall quality of the fit and account for its uncertainty by using a sandwich estimator for the variance.

# Robust standard errors for heteroscedasticity
coeftest(m2)
coeftest(m2, vcov. = vcovHC(m2))
coefci(m2, vcov. = vcovHC(m2))
# The robust standard errors do not alter the main conclusions: logu, logu^2, and lotto are significant predictors.


# OVERALL PREDICTIVE EVALUTATION. Let us compare predictive performance on the same scale
fit0 <- predict(m0)
fit1 <- 1 / predict(m1)
fit2 <- exp(predict(m2))

# "R-squared" in original scale
1 - sum((Clotting$tempo - fit0)^2) / sum((Clotting$tempo - mean(Clotting$tempo))^2)
1 - sum((Clotting$tempo - fit1)^2) / sum((Clotting$tempo - mean(Clotting$tempo))^2)
1 - sum((Clotting$tempo - fit2)^2) / sum((Clotting$tempo - mean(Clotting$tempo))^2)

# In terms of predictive accuracy, m2 > m1 >> m0. I would choose m2 as the final model.

