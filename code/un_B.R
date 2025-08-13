data("Beetles")
colnames(Beetles) <- c("n", "deaths", "logdose")
knitr::kable(Beetles)

ggplot(data = Beetles, aes(x = logdose, y = deaths / n)) +
  geom_point() +
  theme_light() +
  xlab("log-dose") +
  ylab("Proportion of deaths")

fit_Beetles <- glm(cbind(deaths, n - deaths) ~ logdose, family = "binomial", data = Beetles)

ggplot(data = Beetles, aes(x = logdose, y = deaths / n)) +
  geom_point() +
  geom_function(fun = function(x) plogis(coef(fit_Beetles)[1] + coef(fit_Beetles)[2] * x), linetype = "dashed", linewidth = 0.6) +
  theme_light() +
  xlab("log-dose") +
  ylab("Proportion of deaths")

fit_Beetles_lm <- lm(I(deaths / n) ~ logdose, data = Beetles)
fit_Beetles_logit <- lm(qlogis((deaths + 0.5) / (n + 1)) ~ logdose, data = Beetles)

ggplot(data = Beetles, aes(x = logdose, y = deaths / n)) +
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

Beetles$Proportions <- Beetles$deaths / Beetles$n
Beetles$predictions <- predict(fit_Beetles, type = "response")

# knitr::kable(Beetles, digits = 3)

sum(Aids$deaths)
sum(Aids$period * Aids$deaths)

# solve(vcov(fit_Aids))
# round(vcov(fit_Aids), 3)

lmtest::coeftest(fit_Beetles)

lmtest::coefci(fit_Beetles)

lmtest::coeftest(fit_Aids)

lmtest::coefci(fit_Aids)


fit_Aids0 <- glm(deaths ~ 1, family = "poisson", data = Aids)
phi <- summary(fit_Aids)$dispersion # Yes, I know this is equal 1, but it is here for conceptual clarity

W_e <- lmtest::waldtest(fit_Aids0, fit_Aids, test = "Chisq")$Chisq[2]
W_u <- anova(fit_Aids, test = "Rao")$Rao[2]
W <- (deviance(fit_Aids0) - deviance(fit_Aids)) / phi

beta_2 <- coef(fit_Aids)[2]
var_beta_2 <- vcov(fit_Aids)[2, 2]
CI_Wald <- lmtest::coefci(fit_Aids)[2, ]
CI_Wald

CI_Rao <- confint(fit_Aids, test = "Rao")[2, ]
CI_Rao

CI_LRT <- confint(fit_Aids, test = "LRT")[2, ]
CI_LRT

exp(CI_Rao) # Rao/Score confidence interval for exp(beta_2)
exp(CI_LRT) # LRT confidence interval for exp(beta_2)

var_exp_beta2 <- exp(2 * beta_2) * var_beta_2
exp(beta_2) + c(-1, 1) * qnorm(0.975) * sqrt(var_exp_beta2)
