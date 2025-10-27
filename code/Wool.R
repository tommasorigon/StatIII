# ---------------------------------------------------------------------
# Wool dataset
# DISCLAIMER: This solution is provided in a partial form. Certain steps, justifications, and details have been omitted for clarity and brevity.
# ---------------------------------------------------------------------

# The dataset `Wool` (Hand et al., 1994, p. 328) contained in the `MLGdata` library were obtained from an experiment  aimed at evaluating the effect of three variables, length (`x1`), width (`x2`), and load (`x3`), on the number of test cycles until rupture (`y`) of a wool yarn.  

# For each of the three variables `x1`, `x2`, and `x3`, three levels were fixed:  
  
# - Length: 250, 300, 350 mm (coded as `-1`, `0`, `1`)  
# - Width: 8, 9, 10 mm (coded as `-1`, `0`, `1`)  
# - Load: 40, 45, 50 g (coded as `-1`, `0`, `1`)  

library(MLGdata)
data(Wool)
str(Wool)
summary(Wool)

# (a) ---------------------------------------------------------------------
# Omitted

# (b) Using pen and paper, specify a normal linear model]{.blue} for the logarithmic transformation of the response.

boxplot(log(y) ~ x1, data = Wool)
boxplot(log(y) ~ x2, data = Wool)
boxplot(log(y) ~ x3, data = Wool)

# (c) Fit in **R** the linear model from the previous point.  

m_log <- lm( log(y) ~ x1 + x2 + x3, data = Wool)
summary(m_log)

# (d) Assess the goodness of fit of the model and consider whether a transformation of the response other than the logarithmic one may be more appropriate.  

par(mfrow = c(2, 2))
plot(m_log, which = 1:4)
par(mfrow = c(1, 1))

m_lin <- lm( y ~ x1 + x2 + x3, data = Wool)
library(MASS)
boxcox(m_lin)

# (e) Write down the expression of the estimated curve.  

# (f) Obtain a 95% confidence interval for the mean number of cycles to rupture for a test with length = 300 mm, width = 10 mm, and load = 40 g. For the same values of length, width, and load, obtain a prediction interval for the response.  

# (g) For the same data, considering the untransformed response, specify a [generalized linear model]{.orange} with Gamma response and logarithmic link function.  

# (h) Fit in **R** the generalized linear model from the previous point.  

m_gamma <- glm(y ~ x1 + x2 + x3, family = Gamma(link = "log"), data = Wool)
summary(m_gamma)

# (i) Report the estimates and confidence intervals for the coefficients. Provide an interpretation of the obtained values.  

# (j) For each element of the `summary` output of the `glm` object in R, indicate which quantity is being calculated, matching them with the formulas in the slides.  

# (k) Evaluate the goodness of fit of the Gamma model.

par(mfrow = c(2, 2))
plot(m_gamma, which = 1:4)
par(mfrow = c(1, 1))

# (l) Write down the expression of the estimated curve.  

# (m) Obtain a 95% confidence interval for the mean response in an experiment with length = 300 mm, width = 10 mm, and load = 40 g, using the fitted Gamma model.  

# (n) Compare the results of the analysis based on the normal linear model with those of the analysis based on the Gamma model.  