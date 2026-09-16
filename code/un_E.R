set.seed(123)
n_sim <- 2000
x_sim <- rbinom(n_sim, size = 1, prob = 0.5) # binary covariate
y_sim <- rpois(n_sim, lambda = exp(1 + 3 * x_sim)) # no overdispersion at all

c(mean = mean(y_sim), variance = var(y_sim))

ggplot(data.frame(y = y_sim, x = factor(x_sim)), aes(x = y, fill = x)) +
  geom_histogram(binwidth = 2, position = "identity", alpha = 0.6) +
  theme_light() +
  scale_fill_tableau(palette = "Color Blind") +
  xlab("y") +
  ylab("Frequency") +
  labs(fill = "x")

m_sim <- glm(y_sim ~ x_sim, family = poisson)
sum(residuals(m_sim, type = "pearson")^2) / m_sim$df.residual # phi_hat

data(Ants)
Ants$Bread <- as.factor(Ants$Bread)
Ants$Filling <- as.factor(Ants$Filling)
Ants$Butter <- as.factor(Ants$Butter)
head(Ants[, 1:4], 6)

par(mfrow = c(1, 3))
boxplot(Ant_count ~ Bread, data = Ants, col = "grey85")
boxplot(Ant_count ~ Filling, data = Ants, col = "grey85")
boxplot(Ant_count ~ Butter, data = Ants, col = "grey85")
par(mfrow = c(1, 1))

m_pois <- glm(Ant_count ~ Bread + Filling * Butter,
  family = poisson, data = Ants
)
summary(m_pois)

X2 <- sum(residuals(m_pois, type = "pearson")^2)
c(X2 = X2, df = m_pois$df.residual, pvalue = pchisq(X2, m_pois$df.residual, lower.tail = FALSE))

phi_hat <- X2 / m_pois$df.residual
phi_hat

m_quasi <- glm(Ant_count ~ Bread + Filling * Butter,
  family = quasi(link = "log", variance = "mu"), data = Ants
)
summary(m_quasi)

# Point estimates are identical
max(abs(coef(m_pois) - coef(m_quasi)))

# Standard errors are inflated by the same factor
se_ratio <- summary(m_quasi)$coefficients[, 2] / summary(m_pois)$coefficients[, 2]
round(se_ratio, 4)
sqrt(summary(m_quasi)$dispersion)

# library(gee)
# obs <- 1:nrow(Ants) # the gee function requires an id for each observation
# summary(gee(Ant_count ~ Bread + Filling * Butter,
#   id = obs, family = poisson, data = Ants, scale.fix = TRUE
# ))

drop1(m_quasi, test = "F")

m_quasi1 <- update(m_quasi, . ~ . - Bread)
drop1(m_quasi1, test = "F")

m_quasi2 <- update(m_quasi1, . ~ . - Filling:Butter)
summary(m_quasi2)

newdata <- data.frame(Filling = "3", Butter = "1")
pred <- predict(m_quasi2, newdata = newdata, type = "link", se.fit = TRUE)

alpha <- 0.05
z <- qnorm(1 - alpha / 2)
mu_hat <- exp(pred$fit)
phi_tilde <- summary(m_quasi2)$dispersion

# Confidence interval for the mean mu
ci_mean <- exp(c(pred$fit - z * pred$se.fit, pred$fit + z * pred$se.fit))

# Approximate prediction interval for a new observation
ci_pred <- qnorm(c(alpha / 2, 1 - alpha / 2), mean = mu_hat, sd = sqrt(phi_tilde * mu_hat))
round(rbind(ci_mean, ci_pred), 2)

data(Rats)
head(Rats)

# Mortality rate within each group
with(Rats, tapply(s, group, sum) / tapply(n, group, sum))

Rats$placebo <- ifelse(Rats$group == 1, 1, 0)

m_bin <- glm(s / n ~ placebo + h, weights = n, family = binomial, data = Rats)
summary(m_bin)

X2_rats <- sum(residuals(m_bin, type = "pearson")^2)
c(X2 = X2_rats, df = m_bin$df.residual)

phi_rats <- X2_rats / m_bin$df.residual
phi_rats

m_bin_q <- glm(s / n ~ placebo + h,
  weights = n, data = Rats,
  family = quasi(link = "logit", variance = "mu(1-mu)")
)
summary(m_bin_q)
