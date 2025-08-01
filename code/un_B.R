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
