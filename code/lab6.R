# LAB 6, Statistica III (Generalized Linear Models)
# Title: "Quasi-likelihoods". Dataset: Rats
# Author: Bernardo Nipoti, Tommaso Rigon

# -------------------------------------------------------------------
# Dataset 1: Rats - APPLIED ANALYSIS
# -------------------------------------------------------------------

# Data refers to a teratology study (Moore and Tsiatis, 1991). Female rats on an iron-deficient diet were randomly assigned to four groups: placebo (group 1), iron supplement on days 7 and 10 (group 2), on days 0 and 7 (group 3), weekly (group 4). The rats were made pregnant and, after three weeks, the number of dead fetuses (s) out of the litter size (n) was recorded, together with the haemoglobin level of the mother (h).

# QUESTION 1: does the iron supplement reduce the probability of death of the fetuses?
# QUESTION 2: is the haemoglobin level of the mother relevant?

library(MLGdata)
data(Rats)
str(Rats)
head(Rats)

## Exploratory analysis -------------------------------------------------------------

# Proportion of dead fetuses within each group
with(Rats, tapply(s, group, sum) / tapply(n, group, sum))

# The placebo group is very different from the others, whereas the three treated groups are similar. We define a binary variable distinguishing placebo and treated rats.
Rats$placebo <- ifelse(Rats$group == 1, 1, 0)

plot(Rats$h, Rats$s / Rats$n,
  pch = 16, col = Rats$placebo + 1,
  xlab = "Haemoglobin level", ylab = "Proportion of dead fetuses"
)
legend("topright", legend = c("Treated", "Placebo"), col = c(1, 2), pch = 16)

# The iron supplement is likely to affect the haemoglobin level of the mother. Let us compare the two groups.
with(Rats, tapply(h, placebo, mean))

# The mean haemoglobin level is about 11.5 in the treated group and 4.6 in the placebo group: the two variables are strongly associated, and their effects are difficult to disentangle.

## Binomial regression -------------------------------------------------------------

m_bin <- glm(cbind(s, n - s) ~ placebo + h, family = binomial, data = Rats)
summary(m_bin)

# Odds ratios
exp(coef(m_bin))

# For a given haemoglobin level, the odds of death in the placebo group are about 14 times those of the treated group. For a given group, one additional unit of haemoglobin reduces the odds of death by about 17%. Both effects appear to be significant.

## Is there overdispersion? -------------------------------------------------------------

X2 <- sum(residuals(m_bin, type = "pearson")^2)
X2
m_bin$df.residual
pchisq(X2, df = m_bin$df.residual, lower.tail = FALSE)

phi_hat <- X2 / m_bin$df.residual
phi_hat

# The Pearson statistic is about three times its degrees of freedom: there is strong evidence of overdispersion.
# A plausible mechanism: the probability of death varies across litters, even within the same group and with the same haemoglobin level (e.g. because of genetic factors). Equivalently, fetuses of the same litter are positively correlated.

# Under positive correlation, the inflation of the variance should increase with the litter size n. Let us check it graphically.
plot(Rats$n, rstandard(m_bin, type = "pearson"),
  pch = 16,
  xlab = "Litter size", ylab = "Standardized Pearson residuals"
)
abline(h = c(-2, 0, 2), lty = "dotted")

# Small litters (n <= 8) display noticeably smaller residuals than the larger ones, in agreement with the correlated-trials mechanism.

## Quasi-binomial model -------------------------------------------------------------

m_quasi <- glm(cbind(s, n - s) ~ placebo + h, family = quasibinomial, data = Rats)
summary(m_quasi)

# The estimates are identical to those of the binomial model
cbind(binomial = coef(m_bin), quasi = coef(m_quasi))

# The standard errors are multiplied by sqrt(phi_hat)
summary(m_quasi)$coefficients[, 2] / summary(m_bin)$coefficients[, 2]
sqrt(summary(m_quasi)$dispersion)

## Testing hypotheses -------------------------------------------------------------

# Is the haemoglobin level relevant, once overdispersion is accounted for?
m_quasi0 <- update(m_quasi, . ~ . - h)
anova(m_quasi0, m_quasi, test = "Chisq")

# The haemoglobin level is not significant anymore (p-value about 0.13), whereas the placebo effect remains strong.

# Is the dichotomization of the variable group reasonable? We compare the model with the full factor.
m_quasi_group <- glm(cbind(s, n - s) ~ factor(group), family = quasibinomial, data = Rats)
anova(m_quasi0, m_quasi_group, test = "Chisq")

# There is no evidence of differences among the three treated groups (p-value about 0.51): the dichotomization is reasonable.

## Final model and interpretation -------------------------------------------------------------

summary(m_quasi0)

# Odds ratio between placebo and treated rats, with a 95% Wald confidence interval
exp(coef(m_quasi0)[2])
exp(confint.default(m_quasi0)[2, ])

# NOTE: the odds ratio (about 43) is much larger than the one obtained adjusting for h (about 14). The iron supplement increases the haemoglobin level, which in turn reduces mortality: h is part of the mechanism through which the treatment acts. Hence, the model without h estimates the overall effect of the treatment.

# Comparison with the confidence interval obtained by ignoring overdispersion (too narrow!)
m_bin0 <- update(m_bin, . ~ . - h)
exp(confint.default(m_bin0)[2, ])

# Estimated probabilities of death, with 95% confidence intervals
newdata <- data.frame(placebo = c(0, 1))
pred <- predict(m_quasi0, newdata = newdata, type = "link", se.fit = TRUE)
alpha <- 0.05
z <- qnorm(1 - alpha / 2)
cbind(
  estimate = plogis(pred$fit),
  lower = plogis(pred$fit - z * pred$se.fit),
  upper = plogis(pred$fit + z * pred$se.fit)
)
