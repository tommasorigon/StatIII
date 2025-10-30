# ---------------------------------------------------------------------
# dde dataset
# DISCLAIMER: This solution is provided in partial form. Certain steps, justifications, and details have been omitted for clarity and brevity.
# ---------------------------------------------------------------------

# The `dde` dataset contains data from a sample of n = 2312 pregnant women. The original data are described in:
# Longnecker, M. P., Klebanoff, M. A., Zhou, H., & Brock, J. W. (2001). Association between maternal serum concentration of the DDT metabolite DDE and preterm and small-for-gestational-age babies at birth. *Lancet*, **358**(9276), 110–114.

# `DDE` measures the concentration of a substance related to DDT, present in maternal serum during the third trimester of pregnancy. The variable `GAD` (Gestational Age at Delivery) records the day of pregnancy on which delivery occurred.

# DDT is extremely effective against malaria-carrying mosquitoes and is therefore widely used in areas where malaria is endemic. At the same time, DDT may pose a health risk, particularly for pregnant women.

# We are interested in understanding the impact of `DDE` on `GAD`, and in particular, in estimating the probability of preterm delivery (< 37th week, i.e., < 259 days).

rm(list = ls())

dde <- read.csv("../data/dde.csv")
dde <- read.csv("https://tommasorigon.github.io/StatIII/data/dde.csv") # Alternatively

# (a) ---------------------------------------------------------------------

# Let us begin with some descriptive statistics
plot(dde$DDE, dde$GAD)

# OPTION 1: simple linear model
m_linear <- lm(GAD ~ DDE, data = dde)
summary(m_linear)

plot(dde$DDE, dde$GAD, pch = 16)
abline(m_linear, col = "green")

# COMMENT: The R-squared is very low, but this is expected: gestational age at delivery is highly unpredictable. The effect of DDE on GAD is negative, which confirms that preterm births are positively associated with high DDE values. However, let us check whether there are any issues in the diagnostic plots:
par(mfrow = c(2, 2))
plot(m_linear, which = 1:4)
par(mfrow = c(1, 1))

# COMMENT: The residuals are clearly non-Gaussian (they show some asymmetry), but this is not necessarily a major issue, especially given the large sample size. Let us adjust the standard errors to account for potential heteroskedasticity.
library(lmtest)
library(sandwich)
coeftest(m_linear, vcov. = vcovHC(m_linear)) # Heteroskedasticity does not seem to be a serious concern, since the standard errors are almost identical to the previous ones.

# OPTION 2: variable transformation
# Let us consider a variable transformation. The first idea is to use the Box–Cox method:
MASS::boxcox(m_linear, lambda = seq(-2, 10, 1 / 10)) # (Manually adjusted the range for lambda)

# The suggested value for lambda is about 4.5, which is somewhat debatable as it lacks strong theoretical justification. We can therefore ignore it (even though it is supported by the data).

# OPTION 3: gamma models
# Since the outcome variable (GAD) is positive, it makes sense to consider a Gamma model. We consider two options: a Gamma model with a logarithmic link and one with an identity (!) link. The practical risk of obtaining negative predictions is negligible, so using the identity link is not necessarily problematic.

m_gamma_lin <- glm(GAD ~ DDE, data = dde, family = Gamma(link = "identity"))
summary(m_gamma_lin)

# COMMENT: This model yields almost the same results as the linear model but accounts for heteroskedasticity by assuming that Y_i follows a Gamma distribution. The point estimates are nearly identical.
par(mfrow = c(2, 2))
plot(m_gamma_lin, which = 1:4)
par(mfrow = c(1, 1))

# COMMENT: The diagnostic plots show that the residuals are somewhat asymmetric, but otherwise there are no major issues.

# Let us also consider the log link
m_gamma_log <- glm(GAD ~ DDE, data = dde, family = Gamma(link = "log"))
summary(m_gamma_log)

par(mfrow = c(2, 2))
plot(m_gamma_log, which = 1:4)
par(mfrow = c(1, 1))

deviance(m_gamma_lin)
deviance(m_gamma_log)

# COMMENT: In terms of deviance, the two models are almost identical and exhibit similar residual asymmetry. However, the identity link is preferable here because it facilitates the interpretation of the parameters.

# (b) ---------------------------------------------------------------------

# COMMENT: Yes, the DDT does have an impact of the delivery date: in all considered models, after adjustments, the p-value associated to beta_2 and DDE is highly significant. The impact of DDE on GAD is modest, but definitely present. The interpretation is the following

# In models m_linear and m_gamma_lin, each 100 mg/L dose of DDE implies a change in GAD
100 * coef(m_linear)[2] # About 11 days less compared to no exposure
100 * coef(m_gamma_lin)[2] # About 11 days less compared to no exposure

# When using the logarithmic link, each 100 mg/L dose of DDE implies a relative change in GAD
100 * (exp(100 * coef(m_gamma_log)[2]) - 1) # About -3.92% change compared to no exposure

# (c) ---------------------------------------------------------------------

# Linear model
predict(m_linear, newdata = data.frame(DDE = c(0, 100)))

# Gamma model, identity link
predict(m_gamma_lin, newdata = data.frame(DDE = c(0, 100)), type = "response")

# Gamma model, log link
predict(m_gamma_log, newdata = data.frame(DDE = c(0, 100)), type = "response")

# (d) ---------------------------------------------------------------------

# Compute the parameters a gamma distribution (see slides and ? dgamma documentation)
alpha_lin <- 1 / summary(m_gamma_lin)$dispersion
lambda_lin <- alpha_lin / fit_gamma_lin

# Gamma model with log-ling
alpha_log <- 1 / summary(m_gamma_log)$dispersion
lambda_log <- alpha_log / fit_gamma_log

# Psi under normal model
probs_linear <- pnorm(259, fitted(m_linear), sd = summary(m_linear)$sigma)
# Psi under gamma model, identity link
probs_gamma_lin <- pgamma(259, shape = alpha_lin, rate = alpha_lin / fitted(m_gamma_lin))
# Psi under gamma model, log-link
probs_gamma_log <- pgamma(259, shape = alpha_log, rate = alpha_log / fitted(m_gamma_log))

cor(cbind(probs_linear, probs_gamma_lin, probs_gamma_log)) # The probabilities are ALMOST identical

# (e) ---------------------------------------------------------------------

# This is just a special case of the former point.

# Normal model
pnorm(259, predict(m_linear, newdata = data.frame(DDE = c(0, 100))), sd = summary(m_linear)$sigma)
# Gamma model, identity link
pgamma(259, shape = alpha_lin, rate = alpha_lin / predict(m_gamma_lin, newdata = data.frame(DDE = c(0, 100)), type = "response"))
# Gamma model, log-link
pgamma(259, shape = alpha_log, rate = alpha_log / predict(m_gamma_lin, newdata = data.frame(DDE = c(0, 100)), type = "response"))

# (f) OPTIONAL ---------------------------------------------------------------------

# Linear model
predict(m_linear, newdata = data.frame(DDE = c(0, 100)), interval = "prediction")

# Gamma model, identity link
fit_gamma_lin <- predict(m_gamma_lin, newdata = data.frame(DDE = c(0, 100)), type = "response")

# Compute a prediction intervals based on quantiles
tab_lin <- rbind(
  c(fit_gamma_lin[1], qgamma(p = c(0.025, 0.975), shape = alpha_lin, rate = lambda_lin[1])),
  c(fit_gamma_lin[2], qgamma(p = c(0.025, 0.975), shape = alpha_lin, rate = lambda_lin[2]))
)
colnames(tab_lin) <- c("fit", "lwr", "upr")
tab_lin

# Gamma model, log link
fit_gamma_log <- predict(m_gamma_log, newdata = data.frame(DDE = c(0, 100)), type = "response")

# Compute a prediction intervals based on quantiles
tab_log <- rbind(
  c(fit_gamma_log[1], qgamma(p = c(0.025, 0.975), shape = alpha_log, rate = lambda_log[1])),
  c(fit_gamma_log[2], qgamma(p = c(0.025, 0.975), shape = alpha_log, rate = lambda_log[2]))
)
colnames(tab_log) <- c("fit", "lwr", "upr")
tab_log
