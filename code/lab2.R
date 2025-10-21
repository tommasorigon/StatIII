# LAB 2, Statistica III (Generalized Linear Models)
# Title: "Generalized linear models". Datasets: Clotting, Chimps
# Author: Tommaso Rigon

# -------------------------------------------------------------------
# Dataset 1: Clotting - THEORY AND MODELLING
# -------------------------------------------------------------------

? glm

# Let us consider again the Clotting datataset we have already analyzed in lab1.R
library(MLGdata)
data("Clotting")
str(Clotting)

# QUESTION 0: Create a new variable called logu which corresponds to the logarithm of the plasma concentration
# QUESTION 1: Fit a Gamma GLM for clotting time as a function of logu and lotto, with canonical inverse link. Is the interaction between logu and lotto significant?
# QUESTION 2: Perform an overall goodness of test for model with the interaction term, against the null hypothesis. Answer this using by "manually" performing the LRT test; then, compare the results with the anova command.
# QUESTION 3: Validate the quality of the predictions using informal graphical tools.
# QUESTION 4: Compute the residuals (response, deviance and pearson, and their standardized versions) of the model with the interaction term. Are there outliers?
# QUESTION 5: Produce the standard diagnostic plots for the model with the interaction term. Are there influential points? Identify them and comment on their presence.

# Log-transform plasma concentration
Clotting$logu <- log(Clotting$u)

# Scatter plot: clotting time vs log-concentration
plot(Clotting$logu, Clotting$tempo,
  col = Clotting$lotto,
  xlab = "Log-plasma concentration", ylab = "Clotting time (s)", pch = 16
)

# -------------------------------------------------------------------
# Fit a Gamma GLM with canonical inverse link
m1 <- glm(tempo ~ logu + lotto, family = Gamma(link = "inverse"), data = Clotting)
summary(m1)

# Add interaction term
m1_bis <- update(m1, . ~ . + logu:lotto)
summary(m1_bis)

# QUESTION 1: YES, the interaction term is significant according to the WALD test (p-value about 0)

# Predicted values
muhat <- fitted(m1_bis)
phihat <- sum((Clotting$tempo - muhat)^2 / muhat^2) / m1_bis$df.residual
phihat # Matches the dispersion estimate reported in the summary

# Check if the model is an improvement over the null
D0 <- m1_bis$null.deviance # Null deviance
DC <- m1_bis$deviance # Residual deviance
df <- m1_bis$df.null - m1_bis$df.residual # Parameter "q" in the slides
W_test <- (D0 - DC) / phihat
alphaoss <- 1 - pchisq(W_test, df)
alphaoss # The null hypothesis is rejected

# Alternatively, we could obtain the same results as follows:
m_null <- glm(tempo ~ 1, family = Gamma(link = "inverse"), data = Clotting) # Model with only the intercept
summary(m_null) # Null deviance and residual deviance coincide
anova(m_null, m1_bis, test = "Chisq")

D0
DC
D0 - DC
df
W_test # Not reported in the anova table (but used to compute the p-value)

# QUESTION 2: Done by the commands above

# Prediction
newdata <- data.frame(
  logu = rep(seq(1.5, 4.8, length = 100), each = 2),
  lotto = rep(c("uno", "due"), 100)
)
pred_m1_bis <- predict(m1_bis, newdata = newdata, type = "response")

plot(Clotting$logu, Clotting$tempo,
  col = Clotting$lotto,
  xlab = "Log-plasma concentration", ylab = "Clotting time (s)", pch = 16
)
lines(newdata$logu[newdata$lotto == "uno"], pred_m1_bis[newdata$lotto == "uno"], lty = "dashed", col = "red")
lines(newdata$logu[newdata$lotto == "due"], pred_m1_bis[newdata$lotto == "due"], lty = "dashed")

plot(Clotting$tempo, fitted(m1_bis), pch = 16, xlab = "Observed values", ylab = "Fitted values")
abline(c(0, 1), lty = "dotted")

# QUESTION 3: Based on those plots, the quality of the predictions is excellent

# Response residuals (not recommended for GLMs with non-identity link)
residuals(m1_bis, type = "response") # Coincides with Clotting$tempo - fitted(m1_bis)

# Deviance residuals (default in R)
residuals(m1_bis, type = "deviance")
# Pearson residuals
residuals(m1_bis, type = "pearson")

# Standardized residuals
rstandard(m1_bis, type = "deviance")
rstandard(m1_bis, type = "pearson")

# Plot standardized residuals vs fitted values
par(mfrow = c(2, 2))
plot(fitted(m1_bis), residuals(m1_bis, type = "pearson"), pch = 16)
plot(fitted(m1_bis), residuals(m1_bis, type = "deviance"), pch = 16)

plot(fitted(m1_bis), rstandard(m1_bis, type = "pearson"), pch = 16)
plot(fitted(m1_bis), rstandard(m1_bis, type = "deviance"), pch = 16)

# QUESTION 4: According to the standardized Pearson residuals, observations 1, 2 and 10 could be considered outliers, as their absolute value is close or higher than 2.

# Standard diagnostic plots
par(mfrow = c(2, 2))
plot(m1_bis, which = 1:4) # Residuals, fitted values, leverage, Cook's distance
par(mfrow = c(1, 1))

# Leverage values
influence(m1_bis)$hat
influence(m1_bis)$hat[c(1, 10)]

# Residual values
rstandard(m1_bis, type = "pearson")[c(1, 10)]

# Identify observations with high Cook's distance
round(cooks.distance(m1_bis), 3)
round(cooks.distance(m1_bis)[c(1, 10)], 3)
Clotting[c(1, 10), ]

# QUESTION 5: the Cook's distance of observations 1 and 10, previosly identified as outliers, is massively high. These points appear as "influentials" (leverage points + outliers) in diagnostics despite the predictions look fairly good. This is because they are predicted with less precision compared to other points, while the prediction for most points is extremely precise. Excluding them would bias coefficient estimates and reduce reliability.

# The solution to this situation depends on the goal. If are only interested in making predictions, we can just use model m1_bis even if it is a bit misspecified. Alternatively, we may use sandwich estimators for robustifying the estimates of the standard errors. Indeed, the work even for GLMs.

# -------------------------------------------------------------------
# Dataset 2: Chimps - APPLIED ANALYSIS
# -------------------------------------------------------------------

rm(list = ls())
library(MLGdata)
data("Chimps")

str(Chimps)
# View(Chimps) # Use only for small datasets

# ----------------------------------------------------------
# y (learning times) is the response variable
# word is encoding the 10 words
# chimp is encoding the 4 chimps


# QUESTION 1: are there different difficulties among words?
# QUESTION 2: are there different capabilities among chimps?
# QUESTION 3: explain in plain words what an interaction effect between word and chimp would mean. Is it present in these data?

# Times (in minutes) taken by four chimpanzees to learn each of four words
boxplot(y ~ chimp, data = Chimps)
boxplot(y ~ word, data = Chimps)

# Start with a Gamma model with the canonical link
m1 <- glm(y ~ chimp + word, family = Gamma, data = Chimps)
summary(m1)

# Test whether "word" can be removed
m1_no_word <- glm(y ~ chimp, family = Gamma, data = Chimps)
summary(m1_no_word)
anova(m1_no_word, m1, test = "Chisq") # Null hypothesis is rejected --> QUESTION 1

# Another way is to use anova(m1), but note that this introduces covariates
# sequentially according to the formula order (sometimes useful, often not)
anova(m1)

# QUESTION 1: YES, there are different difficulties among words, because the associated estimated coefficients (i.e. the average learning times) are significantly different.

# Test whether "chimp" can be removed
m1_no_chimp <- glm(y ~ word, family = Gamma, data = Chimps)
anova(m1_no_chimp, m1, test = "Chisq") # Conclusion is dubious, p-value is borderline --> QUESTION 2

# QUESTION 2: UNSURE, there is weak evidence that different chimps have different learning capabilities, as the associated p-value is borderline (about 0.05).

# Diagnostics
Chimps$predicted1 <- predict(m1, type = "response")
View(Chimps)

# Observed vs fitted values
plot(Chimps$y, Chimps$predicted1,
  pch = 16,
  xlab = "Observed values", ylab = "Fitted values"
)
abline(c(0, 1), lty = "dotted")

# Standard diagnostic plots
par(mfrow = c(2, 2))
plot(m1, which = 1:4) # Residuals, fitted values, leverage, Cook's distance
par(mfrow = c(1, 1))

# Can an interaction effect between word and chimp be present in these data?
m1_interaction <- glm(y ~ word * chimp, family = Gamma, data = Chimps)

# We get a warning! Why? This is the saturated model, with p = n (the same chimp cannot learn the same word twice!). The following output clarifies this
summary(m1_interaction)

# QUESTION 3: an interaction effect would mean that each chimp has its own "ranking" of word difficulties, i.e. that some chimps find certain words easier or harder to learn compared to other chimps. In these data, an interaction effect cannot be reliably estimated, as the model is saturated (each chimp learns each word only once).

# Model 2: Gamma GLM with log link

m2 <- glm(y ~ chimp + word, family = Gamma(link = log), data = Chimps)
summary(m2)

# This model (with the log link) guarantees positive fitted values. The deviance is slightly lower compared to m1, but it is not straightforward to formally test whether the improvement is significant.

deviance(m1)
deviance(m2)

cor(Chimps$y, Chimps$predicted1)
cor(Chimps$y, Chimps$predicted2)

Chimps$predicted2 <- fitted(m2)

# Observed vs fitted values (log-link model)
plot(Chimps$y, Chimps$predicted2,
  pch = 16,
  xlab = "Observed values", ylab = "Fitted values (log-link model)"
)
abline(c(0, 1), lty = "dotted")

# Comparison of fitted values from m1 (canonical link) vs m2 (log link)
plot(Chimps$predicted1, Chimps$predicted2,
  pch = 16,
  xlab = "Fitted values (m1)", ylab = "Fitted values (m2)"
)
abline(c(0, 1), lty = "dotted")

# EXERCISE. Answer to questions 1,2 and 3 using the log link as in m2. Do you get the same conclusions?

# QUESTION 1:
# QUESTION 2:
# QUESTION 3:
