library(tidyverse)
library(broom)
library(knitr)
library(ggplot2)
library(GGally)
library(ggthemes)

rm(list = ls())
# The dataset can also be downloaded here: https://tommasorigon.github.io/datamining/data/auto.txt
auto <- read.table("../data/auto.txt", header = TRUE) %>% select(city.distance, engine.size, n.cylinders, curb.weight, fuel)

p0 <- ggpairs(auto,
  columns = 1:4, aes(colour = fuel),
  lower = list(continuous = wrap("points", size = 0.5)),
  upper = list(continuous = wrap("points", size = 0.5)),
  diag = "blank"
) +
  theme_light() +
  scale_color_tableau(palette = "Color Blind") +
  xlab("") +
  ylab("")
p0

ggplot(data = auto, aes(x = engine.size, y = city.distance, col = fuel)) +
  geom_point() +
  theme_light() +
  scale_color_tableau(palette = "Color Blind") +
  theme(legend.position = "top") +
  xlab("Engine size (L)") +
  ylab("Urban distance (km/L)")

x <- seq(0, 1, length = 30)
set.seed(12)
y <- 3 + 3 * x + rnorm(30, 0, 0.5)

dcook <- cooks.distance(lm(c(y, 3 + 3 * 2) ~ c(x, 2)))
plot(x, y, xlim = c(0, 2.3), ylim = c(2, 12), pch = 16, main = "Cook's distance = 0.6")
abline(lm(y ~ x), lty = "dashed")
abline(lm(c(y, 3 + 3 * 2) ~ c(x, 2)), lty = "dashed", col = "red")
points(2, 3 + 3 * 2, col = "red", pch = 16)

dcook <- cooks.distance(lm(c(y, 10) ~ c(x, mean(x))))
plot(x, y, xlim = c(0, 2.3), ylim = c(2, 12), pch = 16, main = "Cook's distance = 0.42")
abline(lm(y ~ x), lty = "dashed")
abline(lm(c(y, 10) ~ c(x, mean(x))), lty = "dashed", col = "red")
points(mean(x), 10, col = "red", pch = 16)

dcook <- cooks.distance(lm(c(y, 4) ~ c(x, 2)))
plot(x, y, xlim = c(0, 2.3), ylim = c(2, 12), pch = 16, main = "Cook's distance = 9.88")
abline(lm(y ~ x), lty = "dashed")
abline(lm(c(y, 4) ~ c(x, 2)), lty = "dashed", col = "red")
points(2, 4, col = "red", pch = 16)

m1 <- lm(city.distance ~ engine.size + I(engine.size^2) + I(engine.size^3) + fuel, data = auto)
# kable(tidy(m1, conf.int = FALSE), digits = 3)

kable(glance(m1)[c(1, 3, 10)])

augmented_m1 <- augment(m1)
ggplot(data = augmented_m1, aes(x = engine.size, y = city.distance, col = fuel)) +
  geom_point() +
  geom_line(aes(y = .fitted)) +
  theme_light() +
  theme(legend.position = "right") +
  scale_color_tableau(palette = "Color Blind") +
  xlab("Engine size (L)") +
  ylab("Urban distance (km/L)")

ggplot(data = augmented_m1, aes(x = .fitted, y = .resid, col = fuel)) +
  geom_point() +
  geom_hline(aes(yintercept = 0), linetype = "dotted") +
  theme_light() +
  theme(legend.position = "right") +
  scale_color_tableau(palette = "Color Blind") +
  xlab("Fitted values") +
  ylab("Residuals")

m2 <- lm(log(city.distance) ~ I(log(engine.size)) + fuel, data = auto)
kable(tidy(m2, conf.int = FALSE), digits = 3)

augmented_m2 <- augment(m2, data = auto)
ggplot(data = augmented_m2, aes(x = engine.size, y = city.distance, col = fuel)) +
  geom_point() +
  geom_line(aes(y = exp(.fitted))) +
  theme_light() +
  theme(legend.position = "right") +
  scale_color_tableau(palette = "Color Blind") +
  xlab("Engine size (L)") +
  ylab("Urban distance (km/L)")

ggplot(data = augmented_m2, aes(x = .fitted, y = .resid, col = fuel)) +
  geom_point() +
  geom_hline(aes(yintercept = 0), linetype = "dotted") +
  theme_light() +
  theme(legend.position = "right") +
  scale_color_tableau(palette = "Color Blind") +
  xlab("Fitted values") +
  ylab("Residuals")

r.squared.original <- 1 - sum(mean((auto$city.distance - exp(predict(m2)))^2)) / sum(mean((auto$city.distance - mean(auto$city.distance))^2))
kable(data.frame(r.squared.original = r.squared.original, glance(m2)[c(1, 3, 10)]))

auto$cylinders2 <- factor(auto$n.cylinders == 2)
m3 <- lm(log(city.distance) ~ I(log(engine.size)) + I(log(curb.weight)) + fuel + cylinders2, data = auto)
kable(tidy(m3, conf.int = FALSE), digits = 3)

augmented_m3 <- augment(m3, data = auto)
ggplot(data = augmented_m3, aes(x = .fitted, y = .resid, col = fuel)) +
  geom_point() +
  geom_hline(aes(yintercept = 0), linetype = "dotted") +
  theme_light() +
  theme(legend.position = "right") +
  scale_color_tableau(palette = "Color Blind") +
  xlab("Fitted values") +
  ylab("Residuals")

r.squared.original <- 1 - sum(mean((auto$city.distance - exp(predict(m3)))^2)) / sum(mean((auto$city.distance - mean(auto$city.distance))^2))
kable(data.frame(r.squared.original = r.squared.original, glance(m3)[c(1, 3, 10)]))

m_box <- lm(city.distance ~ engine.size + curb.weight + fuel + cylinders2, data = auto)
MASS::boxcox(m_box)

m_box <- lm(I(1 / city.distance) ~ engine.size + curb.weight + fuel + cylinders2, data = auto)

augmented_m_box <- augment(m_box, data = auto)
ggplot(data = augmented_m_box, aes(x = .fitted, y = .resid, col = fuel)) +
  geom_point() +
  geom_hline(aes(yintercept = 0), linetype = "dotted") +
  theme_light() +
  theme(legend.position = "right") +
  scale_color_tableau(palette = "Color Blind") +
  xlab("Fitted values") +
  ylab("Residuals")
