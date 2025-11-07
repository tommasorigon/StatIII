# ---------------------------------------------------------------------
# FEV dataset
# DISCLAIMER: This solution is provided in a partial form. Certain steps, justifications, and details have been omitted for clarity and brevity.
# ---------------------------------------------------------------------

# Littell et al. (2000) reported a pharmaceutical clinical trial involving n = 72 patients, randomly assigned to three treatment groups (drug A, drug B, and placebo), with 24 patients per group. The outcome of interest was respiratory function, measured as FEV1 (forced expiratory volume in one second, in liters, variable fev1 in the dataset).

# In this analysis, let y denote the FEV1 value after one hour of treatment. Define:
# x_1: baseline FEV1 measurement prior to treatment (base)
# x_2: treatment group (categorical variable drug with labels a, b, p)

rm(list = ls())
FEV <- read.table("https://tommasorigon.github.io/StatIII/data/FEV.dat", header = TRUE)
str(FEV)
head(FEV)

# (a) ---------------------------------------------------------------------

# Let us start with a graphical analysis
plot(FEV$base, FEV$fev1, pch = 16, col = as.numeric(as.factor(FEV$drug)))
boxplot(fev1 ~ drug, data = FEV)

# COMMENT: The points are roughly aligned along a line, although slight curvature may be present. Moreover, both variables appear to be important. Drug A and B seem to have similar effects, and both differ from P (placebo).

m1 <- lm(fev1 ~ base, data = FEV)
summary(m1)

m2 <- lm(fev1 ~ drug, data = FEV)
summary(m2)

m3 <- lm(fev1 ~ base + drug, data = FEV)
summary(m3)

# (b) ---------------------------------------------------------------------

m4 <- lm(fev1 ~ base * drug, data = FEV)
anova(m3, m4)

# COMMENT: Based on the F-test above, an interaction term is NOT needed.

# (c) ---------------------------------------------------------------------

# Both variables are important, as confirmed by the formal tests and diagnostic analyses below.

# Diagnostics for m1 (base only)
anova(m1, m3)

par(mfrow = c(2, 2))
plot(m1, which = 1:4)
par(mfrow = c(1, 1))

# COMMENT: Removing drug is not advisable; the p-value is extremely small and the reduction in deviance is substantial. Therefore, we prefer m3 over m1. Note, however, that the residual plots for m1 are not particularly problematic.

# Diagnostics for m2 (drug only)
anova(m2, m3)

par(mfrow = c(2, 2))
plot(m2, which = 1:4)
par(mfrow = c(1, 1))

# COMMENT: Removing base is also not advisable, for the same reasons as above. Again, the diagnostic plots are not severely problematic, but m3 is clearly preferable.

# Diagnostics for m3 (base + drug)
library(MASS)
boxcox(m3)

# COMMENT: The Box-Cox analysis suggests that λ = 1 is within the confidence interval, indicating that no transformation of the response is needed.

par(mfrow = c(2, 2))
plot(m3, which = 1:4)
par(mfrow = c(1, 1))

# COMMENT: the diagnostic plots do not show anything particularly concerning.

# (d) --------------------------------------------------------------------

# Let us consider m3. The estimates obtained under m1 and m2 are almost identical.
coef(m3)
confint(m3)

# The coefficient associated with base indicates that, for each unit increase in the baseline FEV1 measurement (base), we expect approximately a one-unit increase (about 0.89, to be precise) in FEV1 after treatment. Since 1 lies within the confidence interval, the effect is essentially proportional. In other words, the patient's initial respiratory condition is directly reflected in the post-treatment measurement.

# The coefficients associated with drug should be interpreted relative to the reference category. Note that the coefficient for drugb includes 0 in its confidence interval, suggesting that drugs A and B have similar effects. Moreover, the output indicates that the placebo group differs significantly from drug A (and likely from B as well, although the latter comparison is not directly provided). More precisely, the placebo reduces FEV1 by approximately -0.6448 relative to drug A, ceteris paribus.

# ADVANCED ANALYSIS (not required for the exam, but useful in practice) ---------------------------------------

# This is a randomized clinical trial. By design, baseline FEV1 (base) and treatment group (drug) are independent. Indeed, the baseline measurements appear very similar across treatment groups:

boxplot(base ~ drug, data = FEV)

# As a result, the predictors base and drug can be regarded as “orthogonal.”  Omitting one predictor has little effect on the estimated coefficient of the other. This explains why the diagnostic plots for m1 and m2 were not problematic: due to randomization, the effect of the omitted variable can be absorbed into the error term.

# Indeed, these estimates are nearly identical:
coef(m1)[2]
coef(m3)[2]

# And these are also nearly identical:
coef(m2)[c(2, 3)]
coef(m3)[c(3, 4)]

# However, including both predictors has practical benefits. The standard errors in model m3 are smaller than those in m1 and m2. In other words, the estimates in m3 are (slightly) more precise.