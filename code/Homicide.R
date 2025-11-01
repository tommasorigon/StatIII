# ---------------------------------------------------------------------
# Homicide dataset
# DISCLAIMER: This solution is provided in partial form. Certain steps, justifications, and details have been omitted for clarity and brevity.
# ---------------------------------------------------------------------

# The data contained in the data frame Homicide, available in the MLGdata R package, report the responses of n = 1308 individuals in the United States to the question:

#  "How many people do you personally know who have been victims of homicide in the past 12 months?"

# count: the reported number of victims
# race: the race of the respondent (0 = White, 1 = Black)

library(MLGdata)

data("Homicide")
str(Homicide)

boxplot(count ~ race, data = Homicide)

y_white <- Homicide$count[Homicide$race == 0] # Observations of group A
y_black <- Homicide$count[Homicide$race == 1] # Observations of group B

par(mfrow = c(1, 2))
plot(table(y_white))
plot(table(y_black))
par(mfrow = c(1, 1))

# In a Poisson model, y_white and y_black are assumed to be i.i.d. draws from a Poisson distribution, with different means for each group. From the graphical inspection above, we can already see some issues (too many zeros!).

# (a) ---------------------------------------------------------------------

m_log <- glm(count ~ race, data = Homicide, family = poisson(link = "log"))
summary(m_log)

# COMMENT: The coefficient associated with race is positive, indicating that Black people know many more homicide victims than White people (the effect is highly significant).

# The usual way of interpreting coefficients under a logarithmic link is in terms of relative changes, that is:
100 * (exp(coef(m_log)[2]) - 1) # 465% change between the two means.

# In this specific example, however, the interpretation is much simpler (see the theoretical exercises in Unit B).
# Indeed, these coefficients are directly related to the means:

avg_white <- mean(y_white)
avg_white
exp(coef(m_log)[1]) # Average for White people

avg_black <- mean(y_black)
avg_black
exp(coef(m_log)[1] + coef(m_log)[2]) # Average for Black people

100 * (avg_black - avg_white) / avg_white # Relative change, same number as before

# (b) ---------------------------------------------------------------------

# Note: avg_white and avg_black, the means of each group, are in this case the model predictions.

freq_white <- table(factor(y_white, levels = 0:6))
teo_white <- round(dpois(0:6, lambda = avg_white) * length(y_white), 1)
cbind(freq_white, teo_white)

freq_black <- table(factor(y_black, levels = 0:6))
teo_black <- round(dpois(0:6, lambda = avg_black) * length(y_black), 1)
cbind(freq_black, teo_black)

# COMMENT: The theoretical and observed frequencies are similar, but in both groups (Black and White) there are notable discrepancies: there are fewer zeros than expected and many more values at higher frequencies than expected. This suggests the presence of zero-inflation in the data.

# (c) ---------------------------------------------------------------------

X2 <- sum(residuals(m_log, type = "pearson")^2)
X2

1 - pchisq(X2, df = m_log$df.residual)

# COMMENT: The p-value is essentially 0, suggesting that the overall goodness of fit is quite poor. HOWEVER, note that this test is valid only when the Poisson means are large, while in this dataset most observations are "0". In other words, there are no theoretical guarantees that the Pearson X2 statistic follows a chi-squared distribution with n - p degrees of freedom.

# To confirm this, recall that the deviance should be similar to the X2 statistic when the means are large. However, with these data we obtain a quite different value:
deviance(m_log)

# A test based on the deviance leads to the opposite conclusion (!?). Again, this indicates that the goodness-of-fit tests in this example are not reliable.
1 - pchisq(deviance(m_log), df = m_log$df.residual)

# Summarizing, the X2 and deviance goodness-of-fit tests are unreliable (and yield opposite conclusions). This occurs because the data mean is close to zero.

# (d) ---------------------------------------------------------------------

# An estimate of the overdispersion parameter is the following, which is slightly higher than 1
X2 / m_log$df.residual

# If we fit a quasi-likelihood model, we obtain the following:
m_quasi <- glm(count ~ race, family = quasipoisson(link = "log"), data = Homicide)
summary(m_quasi)

# However, the residual plots are still quite problematic, as some standardized residuals remain much higher than they should be.
par(mfrow = c(2, 2))
plot(m_quasi, which = 1:4)
par(mfrow = c(1, 1))

# COMMENT: The original Poisson model "m_log" is not compatible with the data, as noted from the analysis of the observed and expected frequencies. However, the quasi-Poisson model "m_quasi" does NOT fix the issue, which appears to be of a different nature (i.e., zero-inflation). Most likely, the data are neither Poisson nor overdispersed-Poisson.

# (e) ---------------------------------------------------------------------

# YES, a zero-inflated model would be appropriate in this case. In this simple case, the two groups (y_white, y_black) are i.i.d. from some distribution that is clearly not Poisson. 

# OPTIONAL, NOT REQUIRED: Fit a zero-inflated model. Spoiler: it works very well!
library(pscl)
m_zeroinf <- zeroinfl(count ~ race | race, data = Homicide)
summary(m_zeroinf)

# COMMENT: There is strong evidence of zero-inflation. Moreover:
teo_white_zeroinf <- round((plogis(coef(m_zeroinf)[3]) * I(0:6 == 0) + (1 - plogis(coef(m_zeroinf)[3])) * dpois(0:6, lambda = exp(coef(m_zeroinf)[1]))) * length(y_white), 1)

# Theoretical and observed frequencies now match almost perfectly.
cbind(freq_white, teo_white, teo_white_zeroinf)

teo_black_zeroinf <- round((plogis(coef(m_zeroinf)[3] + coef(m_zeroinf)[4]) * I(0:6 == 0) + (1 - plogis(coef(m_zeroinf)[3] + coef(m_zeroinf)[4])) * dpois(0:6, lambda = exp(coef(m_zeroinf)[1] + coef(m_zeroinf)[2]))) * length(y_black), 1)

# Theoretical and observed frequencies now match almost perfectly.
cbind(freq_black, teo_black, teo_black_zeroinf)
