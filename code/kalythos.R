# ---------------------------------------------------------------------
# Kalythos dataset
# DISCLAIMER: This solution is provided in partial form. Certain steps, justifications, and details have been omitted for clarity and brevity.
# Author: Tommaso Rigon
# ---------------------------------------------------------------------

# Male inhabitants of the Greek island of Kalythos suffer from a congenital eye disease, whose effects become more pronounced at older ages.  A sample of male islanders of different ages was examined, and the number of blind individuals was recorded.

kalythos <- data.frame(
  age   = c(20, 35, 45, 55, 70),
  blind = c(6, 17, 26, 37, 44),
  total = c(50, 50, 50, 50, 50)
)
kalythos$prop <- kalythos$blind / kalythos$total
kalythos

plot(kalythos$age, kalythos$prop, pch = 16)

#  Using a logit or probit model:

# (a) ---------------------------------------------------------------------

fit_logit <- glm(cbind(blind, total - blind) ~ age, data = kalythos, family = binomial("logit"))
summary(fit_logit)
fit_probit <- glm(cbind(blind, total - blind) ~ age, data = kalythos, family = binomial("probit"))
summary(fit_probit)

# The age at which the probability of blindness is p corresponds to the case when the linear predictor is equal to logit(p). When p = 0.5, we get eta = 0, where eta = \beta_1 + \beta_2 x. Solving it over x, we get x = (eta - beta1) / beta2

p <- 0.5
eta <- qlogis(p)

LD_logit <- (eta - coef(fit_logit)[1]) / coef(fit_logit)[2]
LD_logit

LD_probit <- (eta - coef(fit_probit)[1]) / coef(fit_probit)[2]
LD_probit

# (b) ---------------------------------------------------------------------

deviance(fit_logit)
deviance(fit_probit)

newdata <- data.frame(age = seq(from = 10, to = 90))
pred_logit <- predict(fit_logit, type = "response", newdata = newdata)
pred_probit <- predict(fit_probit, type = "response", newdata = newdata)

plot(newdata$age, pred_logit, type = "l", lty = "dotted")
lines(newdata$age, pred_probit, type = "l", lty = "dotted", col = "red", add = TRUE)
points(kalythos$age, kalythos$prop, pch = 16)

# The deviance of the logit model is slightly lower, but in practice the two models are virtually indistinguishable.

# (c) OPTIONAL ---------------------------------------------------------------------

beta_logit <- coef(fit_logit)
vars_logit <- vcov(fit_logit)
psi <- (eta - coef(fit_logit)[1]) / coef(fit_logit)[2]

# Using the multivariate delta method (details here: https://www.stat.cmu.edu/~hseltman/files/ratio.pdf) we can compute the variance of a ratio, which is equal to

varLD <- (eta - beta_logit[1])^2 / beta_logit[2]^2 * (vars_logit[1, 1] / (eta - beta_logit[1])^2 + vars_logit[2, 2] / beta_logit[2]^2 + 2 * vars_logit[1, 2] / ((eta - beta_logit[1]) * beta_logit[2]))

c(psi, sqrt(varLD), varLD)
psi + c(-1, 1) * qnorm(0.975) * sqrt(varLD)

# ADVANCED AND EVEN MORE OPTIONAL ----------------------------------------------------

# This is a bootstrap approach, which you will encounter, for example, at CLAMSES. It provides an alternative and often more reliable estimator for the variance ands confidence intervals, based on simulation.

boot.fn <- function(data, index = 1:nrow(data), p = 0.5) {
  fit <- glm(cbind(blind, total - blind) ~ age, data = data, subset = index, family = "binomial")
  eta <- qlogis(p)
  psi <- (eta - coef(fit)[1]) / coef(fit)[2]
  psi
}

ran.gen <- function(data, mle) {
  out <- data
  out$blind <- rbinom(nrow(out), size = out$total, prob = mle)
  out
}

set.seed(123)

library(boot)
boot_est <- boot(data = kalythos, statistic = boot.fn, 
                 R = 1000, sim = "parametric", ran.gen = ran.gen, mle = fitted(fit_logit))

boot_est
boot.ci(boot_est,type = c("norm", "basic", "perc"))

# COMMENT: The bootstrap approach produced the same confidence intervals and standard errors as those based on asymptotic considerations. However, it did not require any additional calculus.