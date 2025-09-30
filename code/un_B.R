data("Beetles")
colnames(Beetles) <- c("m", "deaths", "logdose")
knitr::kable(Beetles)

ggplot(data = Beetles, aes(x = logdose, y = deaths / m)) +
  geom_point() +
  theme_light() +
  xlab("log-dose") +
  ylab("Proportion of deaths")

fit_Beetles <- glm(cbind(deaths, m - deaths) ~ logdose, family = "binomial", data = Beetles)

ggplot(data = Beetles, aes(x = logdose, y = deaths / m)) +
  geom_point() +
  geom_function(fun = function(x) plogis(coef(fit_Beetles)[1] + coef(fit_Beetles)[2] * x), linetype = "dashed", linewidth = 0.6) +
  theme_light() +
  xlab("log-dose") +
  ylab("Proportion of deaths")

fit_Beetles_lm <- lm(I(deaths / m) ~ logdose, data = Beetles)
fit_Beetles_logit <- lm(qlogis((deaths + 0.5) / (m + 1)) ~ logdose, data = Beetles)

ggplot(data = Beetles, aes(x = logdose, y = deaths / m)) +
  xlim(c(1.65, 1.92)) +
  geom_point() +
  geom_function(fun = function(x) plogis(coef(fit_Beetles)[1] + coef(fit_Beetles)[2] * x), linetype = "dashed", linewidth = 0.6) +
  geom_function(fun = function(x) coef(fit_Beetles_lm)[1] + coef(fit_Beetles_lm)[2] * x, linetype = "dashed", col = "darkorange", linewidth = 0.6) +
  geom_function(fun = function(x) plogis(coef(fit_Beetles_logit)[1] + coef(fit_Beetles_logit)[2] * x), linetype = "dashed", col = "darkblue", linewidth = 0.6) +
  theme_light() +
  xlab("log-dose") +
  ylab("Proportion of deaths")

data(Aids)
colnames(Aids) <- c("deaths", "period")
rownames(Aids) <- paste(1983:1986, rep(1:4, each = 4), sep = "-")[-c(15:16)]
knitr::kable(t(Aids[1:7, ]))
knitr::kable(t(Aids[8:14, ]))

ggplot(data = Aids, aes(x = period, y = deaths)) +
  geom_point() +
  theme_light() +
  xlab("Period") +
  ylab("Deaths")

fit_Aids <- glm(deaths ~ period, family = "poisson", data = Aids)

ggplot(data = Aids, aes(x = period, y = deaths)) +
  geom_point() +
  geom_function(fun = function(x) exp(coef(fit_Aids)[1] + coef(fit_Aids)[2] * x), linetype = "dashed", linewidth = 0.6) +
  theme_light() +
  xlab("Period") +
  ylab("Deaths")

fit_Aids_lm <- lm(sqrt(deaths) ~ period, data = Aids)
fit_Aids_sqrt <- glm(deaths ~ period, family = poisson(link = "sqrt"), data = Aids)

ggplot(data = Aids, aes(x = period, y = deaths)) +
  geom_point() +
  geom_function(fun = function(x) exp(coef(fit_Aids)[1] + coef(fit_Aids)[2] * x), linetype = "dashed", linewidth = 0.6) +
  geom_function(fun = function(x) (coef(fit_Aids_lm)[1] + coef(fit_Aids_lm)[2] * x)^2, linetype = "dashed", col = "darkorange", linewidth = 0.6) +
  geom_function(fun = function(x) (coef(fit_Aids_sqrt)[1] + coef(fit_Aids_sqrt)[2] * x)^2, linetype = "dashed", col = "darkblue", linewidth = 0.6) +
  theme_light() +
  xlab("Period") +
  ylab("Deaths")

Beetles$Proportions <- Beetles$deaths / Beetles$m
Beetles$predictions <- predict(fit_Beetles, type = "response")

# knitr::kable(Beetles, digits = 3)

sum(Aids$deaths)
sum(Aids$period * Aids$deaths)

Aids$predictions <- predict(fit_Aids, type = "response")
# knitr::kable(Aids, digits = 3)

# round(solve(vcov(fit_Beetles)), 3)
# round(vcov(fit_Beetles), 3)

# solve(vcov(fit_Aids))
# round(vcov(fit_Aids), 3)

lmtest::coeftest(fit_Beetles)

lmtest::coefci(fit_Beetles)

lmtest::coeftest(fit_Aids)

lmtest::coefci(fit_Aids)

fit_Beetles0 <- glm(cbind(deaths, m - deaths) ~ 1,
  family = "binomial",
  data = Beetles
)
phi <- summary(fit_Beetles)$dispersion # Yes, I know this is equal 1, but it is here for conceptual clarity

W_e <- lmtest::waldtest(fit_Beetles0, fit_Beetles, test = "Chisq")$Chisq[2]
W_u <- anova(fit_Beetles, test = "Rao")$Rao[2]
W <- (deviance(fit_Beetles0) - deviance(fit_Beetles)) / phi

tests <- data.frame(value = c(W_e, W_u, W))
tests$q <- 1
tests$pvalue <- pchisq(tests$value, tests$q, lower.tail = FALSE)
rownames(tests) <- c("Wald test", "Rao-score test", "Log-likelihood ratio test")

# knitr::kable(tests, digits = 3)

CI_Wald <- lmtest::coefci(fit_Beetles)[2, ]
CI_Rao <- confint(fit_Beetles, test = "Rao")[2, ]
CI_LRT <- confint(fit_Beetles, test = "LRT")[2, ]

CIs <- rbind(CI_Wald, CI_Rao, CI_LRT)
# knitr::kable(CIs)

fit_Aids0 <- glm(deaths ~ 1, family = "poisson", data = Aids)
phi <- summary(fit_Aids)$dispersion # Yes, I know this is equal 1, but it is here for conceptual clarity

W_e <- lmtest::waldtest(fit_Aids0, fit_Aids, test = "Chisq")$Chisq[2]
W_u <- anova(fit_Aids, test = "Rao")$Rao[2]
W <- (deviance(fit_Aids0) - deviance(fit_Aids)) / phi

tests <- data.frame(value = c(W_e, W_u, W))
tests$q <- 1
tests$pvalue <- pchisq(tests$value, tests$q, lower.tail = FALSE)
rownames(tests) <- c("Wald test", "Rao-score test", "Log-likelihood ratio test")

# knitr::kable(tests, digits = 3)

CI_Wald <- lmtest::coefci(fit_Aids)[2, ]
CI_Rao <- confint(fit_Aids, test = "Rao")[2, ]
CI_LRT <- confint(fit_Aids, test = "LRT")[2, ]

CIs <- rbind(CI_Wald, CI_Rao, CI_LRT)
# knitr::kable(CIs, digits = 3)

# Rao-score and LRT confidence interval for 100 (exp(beta_2) -1)
CIs <- rbind(100 * (exp(CI_Rao) - 1), 100 * (exp(CI_LRT) - 1))
# knitr::kable(CIs, digits = 2)

beta2 <- coef(fit_Aids)[2]
var_beta_2 <- vcov(fit_Aids)[2, 2]
var_exp_beta2 <- 100^2 * exp(2 * beta2) * var_beta_2
# 100*(exp(beta_2)-1) + c(-1, 1) * qnorm(0.975) * sqrt(var_exp_beta2)
# round(100*(exp(CI_Wald) - 1), 2)

summary(fit_Beetles)

anova(fit_Beetles0, fit_Beetles)

dev_Beetles <- deviance(fit_Beetles)
X_Beetles <- sum(residuals(fit_Beetles, type = "pearson")^2)

curve(dchisq(x, df = 6), 0, 30)
polygon(c(seq(X_Beetles, 30, length = 100), X_Beetles),
  c(dchisq(seq(X_Beetles, 30, length = 100), 6), 0),
  col = "slateblue1",
  border = 1
)
abline(v = X_Beetles, lty = "dotted")

summary(fit_Aids)

anova(fit_Aids0, fit_Aids)

lmtest::lrtest(fit_Aids0, fit_Aids)

X_Aids <- sum(residuals(fit_Aids, type = "pearson")^2)

par(mfrow = c(1, 2))
plot(predict(fit_Beetles, type = "response"), rstandard(fit_Beetles, type = "deviance"),
  pch = 16, xlab = expression(mu), ylab = "Standardized deviance residuals"
)
abline(h = 0, lty = "dotted")
plot(predict(fit_Beetles, type = "response"), rstandard(fit_Beetles, type = "pearson"),
  pch = 16, xlab = expression(mu), ylab = "Standardized Pearson residuals"
)
abline(h = 0, lty = "dotted")
par(mfrow = c(1, 1))

plot(fit_Beetles, 4)

par(mfrow = c(1, 2))
plot(predict(fit_Aids, type = "response"), rstandard(fit_Aids, type = "deviance"),
  pch = 16, xlab = expression(mu), ylab = "Standardized deviance residuals"
)
abline(h = 0, lty = "dotted")
plot(predict(fit_Aids, type = "response"), rstandard(fit_Aids, type = "pearson"),
  pch = 16, xlab = expression(mu), ylab = "Standardized Pearson residuals"
)
abline(h = 0, lty = "dotted")
par(mfrow = c(1, 1))

par(mfrow = c(1, 2))
plot(predict(fit_Aids_sqrt, type = "response"), rstandard(fit_Aids_sqrt, type = "deviance"),
  pch = 16, xlab = expression(mu), ylab = "Standardized deviance residuals"
)
abline(h = 0, lty = "dotted")
plot(predict(fit_Aids_sqrt, type = "response"), rstandard(fit_Aids_sqrt, type = "pearson"),
  pch = 16, xlab = expression(mu), ylab = "Standardized Pearson residuals"
)
abline(h = 0, lty = "dotted")
