# ---------------------------------------------------------------------
# Basketball dataset
# DISCLAIMER: This solution is provided in partial form. Certain steps, justifications, and details have been omitted for clarity and brevity.
# ---------------------------------------------------------------------

# The `Basketball` dataset shows the three-point shooting, by game, of Ray Allen of the Boston Celtics during the 2010 NBA (basketball) playoffs (e.g, he made 0 of 4 shots in game 1). Commentators remarked that his shooting varied dramatically from game to game.

basket <- read.table("https://tommasorigon.github.io/StatIII/data/Basketball.dat", header = TRUE)
basket

# (a) ---------------------------------------------------------------------

m_null <- glm(cbind(made, attempts - made) ~ 1, data = basket, family = binomial)
summary(m_null)

# The coefficient represents the overall proportion of made shots
plogis(coef(m_null))
sum(basket$made) / sum(basket$attempts)

# Its standard error can be computed in the usual way. Let us consider the Rao test (this is the best option, although explaining why would take hours).
plogis(confint(m_null, test = "Rao"))

# SIDE NOTE: this model is a "fake" glm because there are no covariates.  The same result can be easily obtained by simply using the binom.test function (confidence interval is exact, not approximate)
binom.test(x = sum(basket$made), n = sum(basket$attempts))

# (b) ---------------------------------------------------------------------

X2 <- sum(residuals(m_null, type = "pearson")^2)
X2 / m_null$df.residual # Estimated phi = 2.03

1 - pchisq(X2, df = m_null$df.residual)

# Yes, there is strong evidence of overdispersion (dispersion is about 2). This indicates that the observations are not iid samples from a Binomial distribution. Consequently, the confidence intervals are likely too narrow.

# (c) ---------------------------------------------------------------------

# Overdispersion is likely due to the fact that each game has its own individual probability of scoring, which varies from match to match. Marginally, this leads to overdispersion. In other words, the data confirms what the commentators were saying: shooting performance varied dramatically from game to game.

m_quasi <- glm(cbind(made, attempts - made) ~ 1, data = basket, family = quasibinomial)
summary(m_quasi)

plogis(confint(m_quasi, test = "Rao"))

# (d) ---------------------------------------------------------------------

plogis(confint(m_null, test = "Rao"))
plogis(confint(m_quasi, test = "Rao"))

# The second confidence interval adjusts for overdispersion, resulting in a wider interval.
