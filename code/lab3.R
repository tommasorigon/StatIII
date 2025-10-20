# LAB 3, Statistica III (Generalized Linear Models)
# Title: "Generalized linear models". Dataset: DDE, Stress
# Author: Tommaso Rigon

# Dataset 1: DDE ----------------------------------------------------------------------------------

# DDT is extremely effective against malaria mosquitoes and is therefore widely used in areas where malaria is endemic. At the same time, DDT may pose a health risk, especially for pregnant women.

# In a sample of n = 2312 pregnant women, DDE is measured, i.e., a substance related to DDT, present in maternal serum during the third trimester of pregnancy. The variable GAD (Gestational Age at Delivery) measures the day of pregnancy on which delivery occurred.

rm(list = ls())
dde <- read.csv("../data/dde.csv")

# As an alternative
dde <- read.csv("https://tommasorigon.github.io/StatIII/data/dde.csv") # Download the file from the internet

str(dde)
summary(dde)

# Scatterplot of the data
plot(dde, pch = 16)

# QUESTION 1: Doctors are typically interested in measuring the risk of preterm delivery (before the 37th week of gestation). WHat is a good model for estimating it? 
# QUESTION 2: Is there an effect of DDE on preterm delivery?
# QUESTION 3: Provide an estimate of the "increase in risk" of preterm delivery associated with a 10-unit increase in DDE. Use various methods for measuring it. 

# Instead of considering GAD as the response variable (which is very noisy), we can dichotomize it in a clinically relevant way, i.e., at the 37th week of gestation (preterm vs. full-term).
dde$preterm <- as.numeric(dde$GAD < 37 * 7) # Preterm delivery group

# Preterm vs. non-preterm
table(dde$preterm)

# There seems to be a difference between the two distributions
boxplot(DDE ~ preterm, data = dde)
tapply(dde$DDE, dde$preterm, mean)

# Binary regression is the right model for these data. Let us first consider a logistic regression model.
m1 <- glm(preterm ~ DDE, data = dde, family = binomial(link = "logit"))
summary(m1)

# There is a positive effect of DDE on preterm delivery.
# Is this a good model? The diagnostic plots are not very informative in this case...
plot(m1)

# Let us first examine the implied predictions. The "inverse logistic" exp(x) / (1 + exp(x)) is implemented in the plogis(x) R function. The fitted curve is:
curve(plogis(coef(m1)[1] + coef(m1)[2] * x), 0, 200,
  xlab = "DDE",
  ylab = "Estimated probability of preterm"
)
rug(dde$DDE) # Add tick marks

# The predicted values are as follows
predict(m1, type = "response")

# Add the points to the plot
points(dde$DDE, predict(m1, type = "response"), cex = 0.7, col = "red", pch = 16)

# At this point it is still unclear whether the model is correctly specified. What about aggregating the data? What about alternative models?

# A more elaborate model using polynomials
m2 <- glm(preterm ~ DDE + I(DDE^2), data = dde, family = binomial(link = "logit"))
summary(m2)

# The residual deviance decreased. Is this a coincidence?

# The Wald test is borderline. This is a typically "uncertain" situation and is probably a matter of personal preference. My tendency is conservative, preferring the simpler model (m1) over the more elaborate one. The LRT test gives almost the same indication. Do not be fooled by the stars: moving from a p-value of 0.055 to 0.049 is not a big difference.

anova(m1, m2)

# Perhaps the strongest argument against the use of this model is the following plot, unless we believe that a massive dose of DDT is actually beneficial for health...
curve(plogis(coef(m2)[1] + coef(m2)[2] * x + coef(m2)[3] * x^2), 0, 200,
  xlab = "DDE",
  ylab = "Estimated probability of preterm"
)
rug(dde$DDE) # Add tick marks

# Let us aggregate the data
breaks <- 10 * (0:18)
breaks

dde$classes <- cut(dde$DDE, breaks = breaks)
dde$classes

# Estimated proportions after splitting the data into 10 classes
prop <- tapply(dde$preterm, dde$classes, mean)
dde_means <- tapply(dde$DDE, dde$classes, mean)

curve(plogis(coef(m1)[1] + coef(m1)[2] * x), 0, 200,
  xlab = "DDE",
  ylab = "Estimated probability of preterm", lty = "dotted"
)
rug(dde$DDE) # Add tick marks
points(dde_means, prop, pch = 16)

# The empirical proportions are very noisy for large values of DDE, so some dispersion is expected. The initial data points are based on a much larger number of observations and more stable.

# This is also a nice example of the usefulness of "indirect evidence": the empirical proportions are less reliable estimates than the fitted GLM.

# Dataset 2: Stress ----------------------------------------------------------------------------------

# The data in Table 5.1 (Haberman, 1978, p. 3) concern individuals who, within a larger randomly selected sample, reported having experienced, during the past 18 months, an event—chosen from a list of 41—that caused them exceptional nervous tension. The responses are classified according to how many months ago the anxiety-inducing event occurred.

rm(list = ls())
stress <- read.csv("../data/stress.csv", sep = ";")

# As an alternative
stress <- read.csv("https://tommasorigon.github.io/StatIII/data/stress.csv", sep = ";") # Download the file from the internet

summary(stress)
plot(stress, pch = 16)

# Fitting a Poisson model with canonical link
m1 <- glm(answer ~ month, data = stress, family = poisson(link = "log"))
summary(m1)

# The effect of month is negative, indicating that the number of individuals reporting anxiety decreases as time passes since the event.
plot(stress, pch = 16)
curve(exp(coef(m1)[1] + coef(m1)[2] * x), add = TRUE, col = "red")

# Tests and confidence intervals --------------------------------------------

# Let us fit the null model, i.e. the model without the covariate
m_null <- glm(answer ~ 1, family = poisson, data = stress) # Model with only the intercept
summary(m_null) # Null deviance and residual deviance coincide

phi <- summary(m1)$dispersion # This is equal 1

# Three test connected to the log-likelihood

# Wald test
W_e <- lmtest::waldtest(m_null, m1, test = "Chisq")$Chisq[2]

# Note that z_value^2 displayed in summary(m1) coincide with (-4.986)^2 = 24.86
# summary(m1)

# Rao-score test
W_u <- anova(m_null, m1, test = "Rao")$Rao[2]

# Log-likelihood ratio test
W <- (deviance(m_null) - deviance(m1)) / phi

# Creating a table with all three test
tests <- data.frame(value = c(W_e, W_u, W))
tests$q <- 1
tests$pvalue <- pchisq(tests$value, tests$q, lower.tail = FALSE)
rownames(tests) <- c("Wald test", "Rao-score test", "Log-likelihood ratio test")
tests

# Alternative commands
anova(m_null, m1, test = "LRT")
anova(m_null, m1, test = "Rao")

# Wald confidence intervals
lmtest::coefci(m1)
# Alternatively, we can compute them "manually"
coef(m1)[1] + c(-1,1) * qnorm(0.975) * sqrt(diag(vcov(m1)))[1]
coef(m1)[2] + c(-1,1) * qnorm(0.975) * sqrt(diag(vcov(m1)))[2]

# Rao confidence intervals
confint(m1, test = "Rao")

# Log-likelihood ratio confidence intervals
confint(m1, test = "LRT")

# Diagnostics ------------------------------------------------

stress$predicted1 <- fitted(m1)
View(stress)

# Observed vs fitted values
plot(stress$answer, stress$predicted1,
  pch = 16,
  xlab = "Observed values", ylab = "Fitted values"
)
abline(c(0, 1), lty = "dotted")

# Standard diagnostic plots
par(mfrow = c(2, 2))
plot(m1, which = 1:4) # Residuals, fitted values, leverage, Cook's distance
par(mfrow = c(1, 1))

# Overall goodness of fit
X2 <- sum(residuals(m1, type = "pearson")^2)
X2
# This simply coincides with
sum((stress$answer - stress$predicted1)^2 / stress$predicted1)

# p-value of the overall goodness of fit. It is somewhat borderline...
pchisq(q = X2, df = m1$df.residual, lower.tail = FALSE)

