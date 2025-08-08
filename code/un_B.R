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

sum(Aids$deaths)
sum(Aids$period * Aids$deaths)

# vcov(fit_Aids)

lmtest::coeftest(fit_Aids)

lmtest::coefci(fit_Aids)
confint(fit_Aids)
