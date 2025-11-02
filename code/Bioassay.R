# ---------------------------------------------------------------------
# Bioassay dataset
# DISCLAIMER: This solution is provided in partial form. Certain steps, justifications, and details have been omitted for clarity and brevity.
# ---------------------------------------------------------------------

# The data in the Bioassay dataset refer to a biological experiment. The variable y represents the number of observed events out of a total number of subjects (den) exposed to a dose z.

rm(list = ls())
library(MLGdata)

data(Bioassay)
str(Bioassay)

plot(Bioassay$z, Bioassay$y / Bioassay$den, pch = 16)

# (a) ---------------------------------------------------------------------

fit_probit <- glm(cbind(y, den - y) ~ z, family = binomial(link = "probit"), data = Bioassay)
summary(fit_probit)

# (b) ---------------------------------------------------------------------

fit_quasi <- glm(cbind(y, den - y) ~ z, family = quasibinomial(link = "probit"), data = Bioassay)
summary(fit_quasi)

# COMMENT: There is strong evidence of overdispersion (\phi = 4.14). A formal chi-squared test (not shown here) would confirm it.

# (c) ---------------------------------------------------------------------

var(rstandard(fit_probit, type = "pearson"))
var(rstandard(fit_quasi, type = "pearson"))

# COMMENT: The variance of the standardized residuals for the probit model is much higher than it should be, indeed because of overdispersion.
