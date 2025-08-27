# LAB 2, Statistica III (Generalized Linear Models)
# Title: "Generalized linear models". Datasets: Clotting, Chimps
# Author: Tommaso Rigon

# Overview of the glm() function in R -------------------------------------------------

# - The main function for fitting linear models in R is: lm()
# - The main function for fitting generalized linear models in R is: glm()

# The general syntax is:
#   glm(formula, family, data, weights)

# Arguments:
#   - formula: an object of class "formula", i.e. a symbolic description of the model to be fitted.
#              (This works in the same way as in lm().)
#
#   - family:  specifies the error distribution and link function.
#              Common families include:
#
#       * binomial(link = "logit")
#            Links: "logit", "probit", "cauchit" (logistic, normal, and Cauchy CDFs),
#                   "log", "cloglog" (complementary log-log).
#
#       * gaussian(link = "identity")
#            Links: "identity", "log", "inverse".
#
#       * Gamma(link = "inverse")
#            Links: "inverse", "identity", "log".
#
#       * inverse.gaussian(link = "1/mu^2")
#            Links: "1/mu^2", "inverse", "identity", "log".
#
#       * poisson(link = "log")
#            Links: "log", "identity", "sqrt".
#
#   - data:    a data frame containing the variables in the model.
#
#   - weights: optional vector of weights to be used in the fitting process.
#              These can be frequency weights (replicating observations)
#              or prior weights (adjusting contribution to the likelihood).

# -------------------------------------------------------------------
# Notes:
#   - The result of glm() is an object of class "glm".
#   - Standard extractor functions apply: summary(), coefficients(), fitted(),
#     residuals(), anova(), predict(), etc.
#   - Use ?family for more details on available families and links.

# Dataset 1: Clotting---------------------------------------------------------------------

rm(list = ls())
library(MLGdata)

# Blood clotting times (seconds) for nine plasma concentrations and two clotting agents
# Source: McCullagh & Nelder (1989), Generalized Linear Models, 2nd Edition
data("Clotting")

str(Clotting)
# View(Clotting) # Only for small datasets

# Descriptive statistics
summary(Clotting)

# Quick plot: clotting time vs concentration, colored by clotting agent
plot(Clotting$concentration, Clotting$time,
     col = Clotting$lot,
     xlab = "Plasma Concentration (%)", ylab = "Clotting Time (s)", pch = 16)

# Fit a Gamma GLM (log link suggested by variance stabilizing transformation)
m2 <- glm(time ~ concentration + lot, family = Gamma(link = "log"), data = Clotting)

# Inspect model matrix
head(Clotting)                     # Original data
head(model.matrix(~ concentration + lot, data = Clotting))  # Design matrix

# Model summary
summary(m2)



# Dataset 2: Chimps ----------------------------------------------------------------------
rm(list = ls())
data("Chimps")

str(Chimps)
