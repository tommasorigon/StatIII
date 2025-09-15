# LAB 3, Statistica III (Generalized Linear Models)
# Title: "Generalized linear models". Dataset: DDE, Stress
# Author: Tommaso Rigon

# Dataset 1: DDE ----------------------------------------------------------------------------------

# DDT is extremely effective against malaria mosquitoes and is therefore widely used in areas where malaria is endemic. At the same time, DDT may pose a health risk, especially for pregnant women.

# In a sample of n=2312 pregnant women, DDE is measured, i.e., a substance related to DDT, present in maternal serum during the third trimester of pregnancy. The variable GAD (Gestational Age at Delivery) measures the day of pregnancy on which delivery occurred.

# Research question: Is the amount of DDE higher among women who delivered prematurely?

rm(list = ls())
dde <- read.csv("../data/dde.csv")

# As an alternative
dde <- read.csv("https://tommasorigon.github.io/StatIII/data/dde.csv") # Download the file from the internet

str(dde)
summary(dde)

# Scatterplot of the data
plot(dde, pch = 16)

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



