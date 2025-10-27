library(ggplot2)
library(ggthemes)

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
  geom_line(size = 1.1) +
  theme_bw() +
  labs(
    x = expression(mu),
    y = expression(eta),
    color = "Link function"
  ) +
  scale_color_tableau(palette = "Color Blind") +
  theme(legend.position = "top") +
  ylim(c(-4, 4))

library(ggplot2)
library(ggthemes)

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
  geom_line(size = 1.1) +
  theme_bw() +
  labs(
    x = expression(eta),
    y = expression(mu),
    color = "Link function"
  ) +
  scale_color_tableau(palette = "Color Blind") +
  theme(legend.position = "top")
