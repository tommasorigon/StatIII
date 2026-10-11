mu <- seq(0.001, 0.999, length.out = 400)

# Compute probabilities for each link
df <- data.frame(
  mu = mu,
  logit = qlogis(mu),
  probit = qnorm(mu),
  cloglog = log(-log(1 - mu)),
  cauchit = qcauchy(mu)
)

# Reshape for ggplot
df_long <- reshape2::melt(df,
  id.vars = "mu",
  variable.name = "Link",
  value.name = "eta"
)

# Plot
ggplot(df_long, aes(x = mu, y = eta, color = Link)) +
  geom_line(linewidth = 1.1) +
  theme_bw() +
  labs(
    x = expression(mu),
    y = expression(eta),
    color = "Link function"
  ) +
  scale_color_tableau(palette = "Color Blind") +
  theme(legend.position = "top") +
  ylim(c(-4, 4))

eta <- seq(-4, 4, length.out = 400)

# Compute probabilities for each link
df <- data.frame(
  eta = eta,
  logit = plogis(eta),
  probit = pnorm(eta),
  cloglog = 1 - exp(-exp(eta)),
  cauchit = pcauchy(eta)
)

# Reshape for ggplot
df_long <- reshape2::melt(df,
  id.vars = "eta",
  variable.name = "Link",
  value.name = "Probability"
)

# Plot
ggplot(df_long, aes(x = eta, y = Probability, color = Link)) +
  geom_line(linewidth = 1.1) +
  theme_bw() +
  labs(
    x = expression(eta),
    y = expression(mu),
    color = "Link function"
  ) +
  scale_color_tableau(palette = "Color Blind") +
  theme(legend.position = "top")

data("shuttle", package = "SMPracticals")
rownames(shuttle) <- NULL # Old-style row names, otherwise glm returns an error
head(shuttle)

set.seed(123) # The x-values are jittered to avoid overplotting
shuttle_long <- rbind(
  data.frame(x = jitter(shuttle$temperature, amount = 0.5), y = shuttle$r / shuttle$m, variable = "Temperature (°F)"),
  data.frame(x = jitter(shuttle$pressure, amount = 5), y = shuttle$r / shuttle$m, variable = "Pressure (psi)")
)
shuttle_long$variable <- factor(shuttle_long$variable, levels = c("Temperature (°F)", "Pressure (psi)"))
shuttle_long$plotted <- factor(ifelse(shuttle_long$y > 0, "Plotted the night before", "Left out"),
  levels = c("Plotted the night before", "Left out")
)

ggplot(shuttle_long, aes(x = x, y = y, color = plotted)) +
  geom_point() +
  geom_vline(data = data.frame(variable = factor("Temperature (°F)", levels = levels(shuttle_long$variable)), x = 31), aes(xintercept = x), linetype = "dotted", inherit.aes = FALSE) +
  geom_blank(data = data.frame(variable = factor("Temperature (°F)", levels = levels(shuttle_long$variable)), x = 30, y = 0), inherit.aes = FALSE, aes(x = x, y = y)) +
  facet_wrap(~variable, scales = "free_x", strip.position = "bottom") +
  ylim(c(0, 1)) +
  scale_color_manual(values = c("#FF8C00", "#3232AA")) +
  theme_light() +
  theme(
    strip.placement = "outside", strip.background = element_blank(), strip.text = element_text(color = "black", size = 11),
    legend.position = "top", legend.title = element_blank()
  ) +
  xlab(NULL) +
  ylab("Proportion of distressed O-rings")

fit_full <- glm(cbind(r, m - r) ~ temperature + pressure,
  family = binomial, data = shuttle
)
summary(fit_full)

fit_temp <- update(fit_full, . ~ . - pressure)
fit_null <- update(fit_full, . ~ 1)

summary(fit_temp)

OR_1 <- exp(coef(fit_temp)[2])
OR_10 <- exp(-10 * coef(fit_temp)[2])
OR_10_CI <- exp(-10 * confint.default(fit_temp)[2, ])

temp_grid <- c(70, 60, 50, 31)
pi_hat <- predict(fit_temp, newdata = data.frame(temperature = temp_grid), type = "response")
odds_hat <- pi_hat / (1 - pi_hat)
tab_prob <- rbind(pi_hat, odds_hat)
rownames(tab_prob) <- c("Probability $\\hat{\\pi}(x)$", "Odds $\\widehat{\\text{odds}}(x)$")
colnames(tab_prob) <- paste0("$x = ", temp_grid, "$°F")
knitr::kable(tab_prob, digits = 3)

tab_risk <- data.frame(
  `Temperature (°F)` = paste0(temp_grid[1:2], " → ", temp_grid[2:3]),
  `Risk difference` = pi_hat[2:3] - pi_hat[1:2],
  `Relative risk` = pi_hat[2:3] / pi_hat[1:2],
  `Odds ratio` = odds_hat[2:3] / odds_hat[1:2],
  check.names = FALSE
)
knitr::kable(tab_risk, digits = 3, row.names = FALSE)

pred <- predict(fit_temp, newdata = data.frame(temperature = 31), se.fit = TRUE)
psi_hat <- plogis(pred$fit)
se_psi <- psi_hat * (1 - psi_hat) * pred$se.fit # Delta method

CIs <- rbind(
  psi_hat + c(-1, 1) * qnorm(0.975) * se_psi,
  plogis(pred$fit + c(-1, 1) * qnorm(0.975) * pred$se.fit)
)
colnames(CIs) <- c("Lower", "Upper")
rownames(CIs) <- c("Wald, delta method", "Wald, logit scale")

knitr::kable(CIs, digits = 3)

new_temp <- data.frame(temperature = seq(30, 85, length.out = 200))
pred_temp <- predict(fit_temp, newdata = new_temp, se.fit = TRUE)
new_temp$fit <- plogis(pred_temp$fit)
new_temp$lower <- plogis(pred_temp$fit - qnorm(0.975) * pred_temp$se.fit)
new_temp$upper <- plogis(pred_temp$fit + qnorm(0.975) * pred_temp$se.fit)

ggplot(new_temp, aes(x = temperature, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  geom_line() +
  geom_point(data = shuttle, aes(x = temperature, y = r / m)) +
  geom_vline(xintercept = 31, linetype = "dotted") +
  theme_bw() +
  xlab("Temperature (°F)") +
  ylab("Proportion of distressed O-rings")

fit_probit <- update(fit_temp, family = binomial(link = "probit"))
fit_cloglog <- update(fit_temp, family = binomial(link = "cloglog"))

df_links <- data.frame(
  temperature = new_temp$temperature,
  logit = predict(fit_temp, newdata = new_temp, type = "response"),
  probit = predict(fit_probit, newdata = new_temp, type = "response"),
  cloglog = predict(fit_cloglog, newdata = new_temp, type = "response")
)
df_links <- reshape2::melt(df_links,
  id.vars = "temperature",
  variable.name = "Link", value.name = "Probability"
)

ggplot(df_links, aes(x = temperature, y = Probability, color = Link)) +
  geom_line(linewidth = 1.1) +
  geom_point(data = shuttle, aes(x = temperature, y = r / m), inherit.aes = FALSE) +
  geom_vline(xintercept = 31, linetype = "dotted") +
  scale_color_tableau(palette = "Color Blind") +
  theme_bw() +
  labs(color = "Link function") +
  theme(legend.position = "top") +
  xlab("Temperature (°F)") +
  ylab("Proportion of distressed O-rings")
