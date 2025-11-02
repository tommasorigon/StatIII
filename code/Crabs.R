rm(list = ls())

Crabs <- read.table("https://tommasorigon.github.io/StatIII/data/Crabs.dat", header = TRUE)

plot(Crabs$weight, Crabs$y, col = Crabs$color, pch = 16, ylab = "Number of satellites", xlab = "Weight")

m_lin_full <- lm(y ~ weight + as.factor(color), data = Crabs)
summary(m_lin_full)

m_lin_red <- lm(y ~ weight, data = Crabs)
summary(m_lin_red)

# Heteroskedasticity
plot(m_lin_full, which = 1:4) # The variance increases with the mean
anova(m_lin_red, m_lin_full) # This is not very trusthworty

library(sandwich)
library(lmtest)
waldtest(m_lin_red, m_lin_full, vcov = vcovHC(m_lin_full))

m_pois_full <- glm(y ~ weight + as.factor(color), data = Crabs, family = poisson)
summary(m_pois_full)
m_pois_red <- glm(y ~ weight, data = Crabs, family = poisson)
summary(m_pois_red)

anova(m_pois_red, m_pois_full, test = "Chisq")

par(mfrow = c(2, 2))
plot(m_pois_full, which = 1:4)
par(mfrow = c(1, 1))

m_quasi_full <- glm(y ~ weight + as.factor(color), data = Crabs, family = quasipoisson)
summary(m_quasi_full)
m_quasi_red <- glm(y ~ weight, data = Crabs, family = quasipoisson)
summary(m_quasi_red)

par(mfrow = c(2, 2))
plot(m_quasi_full, which = 1:4)
par(mfrow = c(1, 1))

Crabs[which.max(cooks.distance(m_quasi_red)), ]

anova(m_quasi_red, m_quasi_full, test = "Chisq")
