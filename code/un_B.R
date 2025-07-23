library(tidyverse)

rm(list = ls())
dataset <- read_delim("../data/Stazione_SanGiorgio.csv", delim = ";", col_types = "c")
dataset <- dataset %>%
  transmute(date = force_tz(as_datetime(Data), tzone = "Europe/Rome"), wind_dir = `San Giorgio D.Vento med. 10m`, `Wind speed` = `San Giorgio V.Vento max`) %>%
  mutate(year = year(date)) %>%
  mutate(month = month(date)) %>%
  mutate(day = day(date)) %>%
  mutate(hour = hour(date))

# Filtering a bunch of them
dataset <- filter(dataset, date >= "2025-04-14 00:00:00")

print(subset(dataset, select = c(date, wind_dir, `Wind speed`))[1:10, ])

ggplot(data = dataset, aes(x = date, y = wind_dir, col = `Wind speed`)) +
  geom_line() +
  labs(x = "Date", y = "Wind direction (degrees)") +
  theme_bw()

clifro::windrose(speed = dataset$`Wind speed`, direction = dataset$wind_dir, n_directions = 20)

y <- dataset$wind_dir / 360 * 2 * pi
s1 <- sum(sin(y))
s2 <- sum(cos(y))

# Estimate the concentration parameter
gamma <- atan2(s1, s2)
tau <- circular::A1inv(mean(cos(y - gamma)))

theta1 <- tau * sin(gamma)
theta2 <- tau * cos(gamma)

# library(circular)
dvonmises <- function(x, gamma, tau) {
  1 / (2 * pi * circular::A1(tau)) * exp(tau * cos(x - gamma))
}

dvonmises <- Vectorize(dvonmises, vectorize.args = "x")
x_seq <- seq(0, 2 * pi, length.out = 500)
plot(x_seq / (2 * pi) * 360, dvonmises(x_seq, gamma, tau), type = "l", xlab = "Degrees", ylab = "Density")
rug(y / (2 * pi) * 360)

ggplot(data = NULL, aes(x = sin(x_seq), y = cos(x_seq), col = dvonmises(x_seq, gamma, tau), size = dvonmises(x_seq, gamma, tau))) +
  geom_point() +
  xlim(c(-1, 1)) +
  ylim(c(-1, 1)) +
  labs(x = "Easting", y = "Northing") +
  theme_bw() +
  theme(legend.position = "none") +
  coord_fixed(ratio = 1)
