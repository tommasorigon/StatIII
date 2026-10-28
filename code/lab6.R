# LAB 6, Statistica III (Generalized Linear Models)
# Title: "Quasi-likelihoods". Datasets: Rats, Leaf blotch
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

# The iron supplement affects the haemoglobin level of the mother: the mean is about 11.5 in the treated group and 4.6 in the placebo group. The two variables are strongly associated, and their effects are difficult to disentangle.
with(Rats, tapply(h, placebo, mean))

## Binomial regression -------------------------------------------------------------

m_bin <- glm(cbind(s, n - s) ~ placebo + h, family = binomial, data = Rats)
summary(m_bin)

# For a given haemoglobin level, the odds of death in the placebo group are about 14 times those of the treated group. For a given group, one additional unit of haemoglobin reduces the odds of death by about 17%. Both effects appear to be significant.
exp(coef(m_bin))

## Is there overdispersion? -------------------------------------------------------------

X2 <- sum(residuals(m_bin, type = "pearson")^2)
X2
pchisq(X2, df = m_bin$df.residual, lower.tail = FALSE)

phi_hat <- X2 / m_bin$df.residual
phi_hat

# The Pearson statistic is about three times its degrees of freedom: there is strong evidence of overdispersion.
# A plausible mechanism: the probability of death varies across litters (e.g. because of genetic factors), that is, fetuses of the same litter are positively correlated. In this case, the variance inflation should increase with the litter size n.
plot(Rats$n, rstandard(m_bin, type = "pearson"),
  pch = 16,
  xlab = "Litter size", ylab = "Standardized Pearson residuals"
)
abline(h = c(-2, 0, 2), lty = "dotted")

# Small litters (n <= 8) display noticeably smaller residuals than the larger ones, in agreement with the correlated-trials mechanism.

## Quasi-binomial model -------------------------------------------------------------

m_quasi <- glm(cbind(s, n - s) ~ placebo + h, family = quasibinomial, data = Rats)
summary(m_quasi)

# The estimates are identical to those of the binomial model, whereas the standard errors are multiplied by sqrt(phi_hat)
cbind(binomial = coef(m_bin), quasi = coef(m_quasi))
summary(m_quasi)$coefficients[, 2] / summary(m_bin)$coefficients[, 2]
sqrt(phi_hat)

## Testing hypotheses -------------------------------------------------------------

# Is the haemoglobin level relevant, once overdispersion is accounted for? Not anymore (p-value about 0.13), whereas the placebo effect remains strong.
m_quasi_placebo <- glm(cbind(s, n - s) ~ placebo, family = quasibinomial, data = Rats)
anova(m_quasi_placebo, m_quasi, test = "Chisq")

# Is the dichotomization of the variable group reasonable? There is no evidence of differences among the three treated groups (p-value about 0.51).
m_quasi_group <- glm(cbind(s, n - s) ~ factor(group), family = quasibinomial, data = Rats)
anova(m_quasi_placebo, m_quasi_group, test = "Chisq")

## Final model and interpretation -------------------------------------------------------------

summary(m_quasi_placebo)

# Odds ratio between placebo and treated rats, with a 95% Wald confidence interval
exp(coef(m_quasi_placebo)[2])
exp(confint.default(m_quasi_placebo)[2, ])

# NOTE: the odds ratio (about 43) is much larger than the one adjusted for h (about 14). The iron supplement increases the haemoglobin level, which in turn reduces mortality: h is part of the mechanism through which the treatment acts. Hence, the model without h estimates the overall effect of the treatment.

# The confidence interval obtained by ignoring overdispersion is too narrow
m_bin_placebo <- glm(cbind(s, n - s) ~ placebo, family = binomial, data = Rats)
exp(confint.default(m_bin_placebo)[2, ])

# Estimated probabilities of death (treated and placebo), with 95% confidence intervals
pred <- predict(m_quasi_placebo, newdata = data.frame(placebo = c(0, 1)), se.fit = TRUE)
plogis(pred$fit)
plogis(pred$fit - qnorm(0.975) * pred$se.fit)
plogis(pred$fit + qnorm(0.975) * pred$se.fit)

# -------------------------------------------------------------------
# Dataset 2: Leaf blotch - APPLIED ANALYSIS
# -------------------------------------------------------------------

# Data refers to the incidence of leaf blotch (Rhynchosporium secalis) on 10 varieties of barley grown at 9 sites (Wedderburn, 1974; McCullagh and Nelder, 1989, Section 9.2.4). The response blotch is the proportion of leaf area affected, a continuous variable in [0, 1].

# QUESTION 1: which varieties are the most resistant to leaf blotch?
# QUESTION 2: is the binomial variance function adequate for these data?

library(faraway)
data(leafblotch)
str(leafblotch)

# Two-way table: varieties (rows) and sites (columns)
round(xtabs(blotch ~ variety + site, data = leafblotch), 3)

## Exploratory analysis -------------------------------------------------------------

par(mfrow = c(1, 2))
boxplot(blotch ~ site, data = leafblotch, xlab = "Site", ylab = "Proportion of leaf area affected")
boxplot(blotch ~ variety, data = leafblotch, xlab = "Variety", ylab = "Proportion of leaf area affected")
par(mfrow = c(1, 1))

# Both the site and the variety strongly affect the incidence of the disease.

## Quasi-binomial model -------------------------------------------------------------

# The response is a continuous proportion, not a number of successes out of m trials: there is no binomial distribution here. However, a natural choice is var(Y_i) = phi * mu_i * (1 - mu_i), together with a logit link and an additive model in site and variety.
m1 <- glm(blotch ~ site + variety, family = quasibinomial, data = leafblotch)
summary(m1)

# The estimate of phi is about 0.089. Since the data are not counts, there is no reason to expect phi to be close to 1: "underdispersion" is not meaningful here.
X2 <- sum(residuals(m1, type = "pearson")^2)
X2 / m1$df.residual

# Is the variance function mu * (1 - mu) adequate? Let us look at the Pearson residuals.
plot(predict(m1), residuals(m1, type = "pearson"),
  pch = 16,
  xlab = "Linear predictor", ylab = "Pearson residuals"
)
abline(h = 0, lty = "dotted")

# The residuals are much less variable for small (and, to a lesser extent, large) values of the linear predictor: for extreme proportions, the variability is smaller than predicted by the variance function mu * (1 - mu).

## A different variance function -------------------------------------------------------------

# Following Wedderburn (1974), we consider the variance function v(mu) = mu^2 * (1 - mu)^2, which decreases faster at the extremes. This variance function does not correspond to any exponential dispersion family: we are outside the GLM world. We modify the quasi family accordingly.
wedderburn <- quasi(link = "logit", variance = "mu(1-mu)")
wedderburn$variance <- function(mu) mu^2 * (1 - mu)^2

m2 <- glm(blotch ~ site + variety, family = wedderburn, data = leafblotch)
summary(m2)

# NOTE: the quasi-likelihood associated with this variance function is not defined when y_i = 0 or y_i = 1. Since some proportions are equal to 0, the quasi-deviance cannot be computed and the "deviance" reported by summary must not be used.

# The estimate of phi is now close to 1
X2 <- sum(residuals(m2, type = "pearson")^2)
X2 / m2$df.residual

# The pattern of the residuals is substantially reduced
plot(predict(m2), residuals(m2, type = "pearson"),
  pch = 16,
  xlab = "Linear predictor", ylab = "Pearson residuals"
)
abline(h = 0, lty = "dotted")

## Comparison of the two models -------------------------------------------------------------

# Variety effects (contrasts with variety 1, rows 10 to 18) with their standard errors
round(summary(m1)$coefficients[10:18, 1:2], 2)
round(summary(m2)$coefficients[10:18, 1:2], 2)

# 1) The estimates are different: unlike variance inflation, changing the variance function changes the estimating equations and therefore the estimates. The ordering of the varieties changes slightly as well.
# 2) Under the second model, all the variety contrasts have the same standard error. Indeed, with the logit link g'(mu) = 1 / {mu (1 - mu)}, hence the weights w_i = 1 / {phi v(mu_i) g'(mu_i)^2} = 1 / phi are all equal.
# 3) In both models, varieties 1-3 are the most resistant, whereas varieties 8-10 are the least resistant.
