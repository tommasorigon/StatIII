data("ships", package = "MASS")
head(ships)

ships[ships$service == 0, ]

ships <- subset(ships, service > 0)
ships$year <- factor(ships$year)
ships$period <- factor(ships$period)

ships$rate <- 1000 * ships$incidents / ships$service # Incidents per 1000 months of service
levels_period <- c("60" = "Operated in 1960-74", "75" = "Operated in 1975-79")

ggplot(ships, aes(x = year, y = rate, color = type, group = type)) +
  geom_point(size = 2) +
  geom_line() +
  facet_wrap(~period, labeller = labeller(period = levels_period)) +
  scale_color_tableau(palette = "Color Blind") +
  theme_bw() +
  labs(color = "Type") +
  xlab("Year of construction") +
  ylab("Incidents per 1000 months")

fit_rate <- glm(incidents ~ type + year + period + offset(log(service)),
  family = poisson, data = ships
)
summary(fit_rate)

fit_null <- update(fit_rate, . ~ 1 + offset(log(service)))
lrt_null <- deviance(fit_null) - deviance(fit_rate)

fit_free <- glm(incidents ~ type + year + period + log(service),
  family = poisson, data = ships
)
round(summary(fit_free)$coefficients["log(service)", 1:2], 3)
anova(fit_rate, fit_free, test = "Chisq")

tab_coef <- exp(cbind(coef(fit_rate), confint.default(fit_rate)))[-1, ]
colnames(tab_coef) <- c("$\\exp(\\hat{\\beta}_j)$", "Lower 95%", "Upper 95%")
knitr::kable(tab_coef, digits = 3)

X2 <- sum(residuals(fit_rate, type = "pearson")^2)
phi_hat <- X2 / df.residual(fit_rate)

fit_quasi <- update(fit_rate, family = quasipoisson)
drop1(fit_quasi, test = "F")
