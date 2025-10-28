# LAB 5, Statistica III (Generalized Linear Models)
# Title: "Poisson regression". Datasets: Cancer, Drugs
# Author: Tommaso Rigon

# -------------------------------------------------------------------
# Dataset 1: Cancer - APPLIED ANALYSIS
# -------------------------------------------------------------------

rm(list = ls())

# The data, from Holford (1980), report survival and death outcomes for 539 males diagnosed with lung cancer. The prognostic factors considered are histology and stage of disease, with observations grouped into 2-month intervals of follow-up after diagnosis. For each combination of follow-up interval, histology, and disease stage, the data provides the number of deaths and the total number of person-months at risk during that interval. We treat the death counts in the table as independent Poisson variates.

cancer <- read.table("../data/Cancer.dat", header = TRUE)
cancer <- read.table("https://tommasorigon.github.io/StatIII/data/Cancer.dat", header = TRUE)
str(cancer)

# Type of histology
cancer$histology <- as.factor(cancer$histology)
# Stage of cancer
cancer$stage <- as.factor(cancer$stage)
# Follow-up time
cancer$time <- as.factor(cancer$time)

# Risk time is = number of patients still alive in that period * months at risk in that period (rounded to the nearest integer). 
# We are interested in predicting the ratio \mu_i / t_i, where Y_i ~ Pois(\mu_i)

# This can be done by including an offset
cancer$logrisktime <- log(cancer$risktime)

m_full <- glm(count ~ histology + stage + time, family = poisson(link = log), offset = logrisktime, data = cancer)
summary(m_full)

# Diagnostics plots
par(mfrow = c(2, 2))
plot(m_full, which = 1:4)
par(mfrow = c(1, 1))

X2 <- sum(residuals(m_full, type = "pearson")^2)
X2 / m_full$df.residual

# COMMENTS

# Let us compute
exp(confint(m_full))
# The estimated death rate at the third stage of disease is 3.76 = exp(1.32431) times that at the first stage, adjusting for follow-up time and histology. The corresponding 95% confidence interval is (2.81, 5.10), confirming a clear increase in death rate with advancing stage.

# COMMENTS: the histology is not important IF we already know the cancer stage
m_no_histology <- glm(count ~ stage + time, family = poisson(link = log), offset = logrisktime, data = cancer)
anova(m_no_histology, m_full)

# The risk of death appears to be roughly constant over time (the p-value is borderline, suggesting a possible slight negative trend). In other words, the probability of survival does not change significantly over time, provided that the cancer stage remains the same.
m_stage <- glm(count ~ stage, family = poisson(link = log), offset = logrisktime, data = cancer)
anova(m_stage, m_full)

X2 <- sum(residuals(m_stage, type = "pearson")^2)
X2 / m_stage$df.residual

# Dataset 2: Drugs -----------------------------------------------------------------------

