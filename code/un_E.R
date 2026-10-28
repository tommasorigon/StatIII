# Data preparation and all the fitted models used in this unit
data(Ants)
Ants$Bread <- as.factor(Ants$Bread)
Ants$Filling <- as.factor(Ants$Filling)
Ants$Butter <- as.factor(Ants$Butter)

pv <- function(p) ifelse(p < 0.001, "<0.001", sprintf("%.3f", p))

# Poisson model
m_pois <- glm(Ant_count ~ Bread + Filling * Butter, family = poisson, data = Ants)
cf_pois <- summary(m_pois)$coefficients
X2 <- sum(residuals(m_pois, type = "pearson")^2)
X2_pvalue <- pchisq(X2, df = m_pois$df.residual, lower.tail = FALSE)

# Quasi-Poisson model
m_quasi <- glm(Ant_count ~ Bread + Filling * Butter,
  family = quasi(link = "log", variance = "mu"), data = Ants
)
cf_quasi <- summary(m_quasi)$coefficients
phi_hat <- summary(m_quasi)$dispersion

# Model simplification
d1 <- drop1(m_quasi, test = "Chisq")
m_quasi1 <- update(m_quasi, . ~ . - Bread)
d2 <- drop1(m_quasi1, test = "Chisq")

# Final model
m_final <- update(m_quasi1, . ~ . - Filling:Butter)
cf_final <- summary(m_final)$coefficients
phi_final <- summary(m_final)$dispersion
rel_change <- round(100 * (exp(coef(m_final)) - 1))

# Confidence and prediction intervals
newdata <- data.frame(Filling = "3", Butter = "1")
pred <- predict(m_final, newdata = newdata, type = "link", se.fit = TRUE)
mu_new <- exp(pred$fit)
ci_mean <- exp(pred$fit + c(-1, 1) * qnorm(0.975) * pred$se.fit)
pi_quasi <- qnorm(c(0.025, 0.975), mean = mu_new, sd = sqrt(phi_final * mu_new))
pi_pois <- qpois(c(0.025, 0.975), lambda = mu_new)

# Simulated data without overdispersion
set.seed(123)
x_sim <- rbinom(2000, size = 1, prob = 0.5)
y_sim <- rpois(2000, lambda = exp(1 + 3 * x_sim))
m_sim <- glm(y_sim ~ x_sim, family = poisson)
phi_sim <- sum(residuals(m_sim, type = "pearson")^2) / m_sim$df.residual

par(mfrow = c(1, 3))
boxplot(Ant_count ~ Bread, data = Ants, col = "grey85")
boxplot(Ant_count ~ Filling, data = Ants, col = "grey85")
boxplot(Ant_count ~ Butter, data = Ants, col = "grey85")
par(mfrow = c(1, 1))

knitr::kable(
  data.frame(
    Estimate = cf_pois[, 1], `Std. Error` = cf_pois[, 2], `p-value` = pv(cf_pois[, 4]),
    check.names = FALSE
  ),
  digits = 3, align = "r"
)

knitr::kable(
  data.frame(
    Estimate = cf_quasi[, 1],
    `SE (Poisson)` = cf_pois[, 2], `SE (quasi)` = cf_quasi[, 2],
    `p-value (Poisson)` = pv(cf_pois[, 4]), `p-value (quasi)` = pv(cf_quasi[, 4]),
    check.names = FALSE
  ),
  digits = 3, align = "r"
)

knitr::kable(
  data.frame(
    Model = c("Full model", "Full model", "Without Bread"),
    `Removed term` = c("Bread", "Filling:Butter", "Filling:Butter"),
    q = c(d1["Bread", "Df"], d1["Filling:Butter", "Df"], d2["Filling:Butter", "Df"]),
    W = c(d1["Bread", "scaled dev."], d1["Filling:Butter", "scaled dev."], d2["Filling:Butter", "scaled dev."]),
    `p-value` = pv(c(d1["Bread", "Pr(>Chi)"], d1["Filling:Butter", "Pr(>Chi)"], d2["Filling:Butter", "Pr(>Chi)"])),
    check.names = FALSE
  ),
  digits = 3, align = "llrrr"
)

knitr::kable(
  data.frame(
    Estimate = cf_final[, 1], `Std. Error` = cf_final[, 2], `p-value` = pv(cf_final[, 4]),
    `exp(Estimate)` = exp(cf_final[, 1]),
    check.names = FALSE
  ),
  digits = 3, align = "r"
)
