# ---------------------------------------------------------------------
# Seed dataset
# DISCLAIMER: This solution is provided in a partial form. Certain steps, justifications, and details have been omitted for clarity and brevity.
# ---------------------------------------------------------------------

library(MLGdata)
data("Seed")

summary(Seed)
str(Seed)

# (a) ---------------------------------------------------------------------

# Omitted.

# (b) --------------------------------------------------------------------- 

boxplot(fert ~ x, data = Seed)
# There is a clear and strong dependence between germination and fertilizer amount. 

# (c) ---------------------------------------------------------------------

# The response variable is "germination" (i.e. x). Since the response variable is binary, the appropriate model is a binary regression model, that is

# Y_i ~ Bernoulli(\pi_i), for i = 1, ..., n
# g(\pi_i) = \beta_1 + \beta_2 \, fert_i

# As a starting point, we can specify a logistic regression model, where
# \pi_i = \exp(\beta_1 + \beta_2 \, fert_i) / (1 + \exp(\beta_1 + \beta_2 \, fert_i)).

# (d) ---------------------------------------------------------------------

m1 <- glm(x ~ fert, data = Seed, family = binomial(link = "logit"))
summary(m1)

# (e) ---------------------------------------------------------------------

coef(m1)      # Maximum likelihood estimates (same as in the summary)
confint(m1)   # Likelihood ratio-based confidence intervals

# The value of the intercept (\beta_1), once transformed back to the probability scale, represents the probability of germination when no fertilizer is used. This probability is about 3%, and can be computed as follows:
plogis(coef(m1)[1])

# The coefficient \beta_2 is positive and significantly different from 0 at a 5% level (see the corresponding p-value in the summary), indicating that fertilizer increases the probability of germination. Moreover, the quantity
exp(coef(m1)[2])
# represents the odds ratio associated with a one-unit increase in fertilizer, which turns out to be extremely large. Another way appreciating this is computing the probabilities when fert = 0 and fert = 1, namely
plogis(coef(m1)[1])
plogis(coef(m1)[1] + coef(m1)[2])
# The difference is very high. 

# (f) ---------------------------------------------------------------------

# Omitted. 
