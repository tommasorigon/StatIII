data("Beetles")
colnames(Beetles) <- c("n", "deaths", "logdose")
knitr::kable(Beetles)

ggplot(data = Beetles, aes(x = logdose, y = deaths / n)) +
  geom_point() +
  theme_light() +
  theme(legend.position = "right") +
  scale_color_tableau(palette = "Color Blind") +
  xlab("log-dose") +
  ylab("Proportion of deaths")

fit_Beetles <- glm(cbind(deaths, n - deaths) ~ logdose, family = "binomial", data = Beetles)

ggplot(data = Beetles, aes(x = logdose, y = deaths / n)) +
  geom_point() +
  geom_function(fun = function(x) plogis(coef(fit_Beetles)[1] + coef(fit_Beetles)[2] * x), linetype = "dashed") +
  theme_light() +
  theme(legend.position = "right") +
  scale_color_tableau(palette = "Color Blind") +
  xlab("log-dose") +
  ylab("Proportion of deaths")
