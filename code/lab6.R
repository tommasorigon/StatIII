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

X2 <- sum(residuals(m1, type = "pearson")^2)
X2
X2 / m1$df.residual

# The estimate of phi is about 0.089. Since the data are not counts, there is no reason to expect phi to be close to 1: "underdispersion" is not meaningful here.

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
wedderburn$validmu <- function(mu) all(mu > 0 & mu < 1)

m2 <- glm(blotch ~ site + variety, family = wedderburn, data = leafblotch)
summary(m2)

# The estimate of phi is now close to 1
sum(residuals(m2, type = "pearson")^2) / m2$df.residual

# NOTE: the quasi-likelihood associated with this variance function is not defined when y_i = 0 or y_i = 1. Since some proportions are equal to 0, the quasi-deviance cannot be computed and the "deviance" reported by summary must not be used.

plot(predict(m2), residuals(m2, type = "pearson"),
  pch = 16,
  xlab = "Linear predictor", ylab = "Pearson residuals"
)
abline(h = 0, lty = "dotted")

# The pattern is substantially reduced. The largest residuals are
r2 <- residuals(m2, type = "pearson")
head(leafblotch[order(r2, decreasing = TRUE), ], 3)
head(sort(r2, decreasing = TRUE), 3)

## Comparison of the two models -------------------------------------------------------------

# Variety effects (contrasts with variety 1) and their standard errors
idx <- grep("variety", names(coef(m1)))
round(cbind(
  binomial_est = coef(m1)[idx], binomial_se = summary(m1)$coefficients[idx, 2],
  wedderburn_est = coef(m2)[idx], wedderburn_se = summary(m2)$coefficients[idx, 2]
), 2)

# 1) The estimates are different: unlike variance inflation, changing the variance function changes the estimating equations and therefore the estimates. The ordering of the varieties changes slightly as well.
# 2) Under the second model, all the variety contrasts have the same standard error. Indeed, with the logit link g'(mu) = 1 / {mu (1 - mu)}, hence the weights w_i = 1 / {phi v(mu_i) g'(mu_i)^2} = 1 / phi are all equal.
# 3) In both models, varieties 1-3 are the most resistant, whereas varieties 8-10 are the least resistant.
