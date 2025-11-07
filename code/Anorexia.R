# ---------------------------------------------------------------------
# Anorexia dataset
# DISCLAIMER: This solution is provided in partial form. Certain steps, justifications, and details have been omitted for clarity and brevity.
# Author: Tommaso Rigon
# ---------------------------------------------------------------------

# For n = 72 young girls diagnosed with anorexia records their body weights (in lbs) before and after an experimental treatment period. 

# During the study, the participants were randomly assigned to one of three therapy groups:  
# Control group: received the standard therapy (label c),  
# Family therapy (label f),  
# Cognitive behavioral therapy (label b).  

rm(list = ls())

anorexia <- read.table("https://tommasorigon.github.io/StatIII/data/Anorexia.dat", header = TRUE)

anorexia$therapy <- as.factor(anorexia$therapy)
str(anorexia)
head(anorexia)

# (a) ---------------------------------------------------------------------

anorexia$change <- anorexia$after - anorexia$before
boxplot(change ~ therapy, data = anorexia)

m1 <- lm(change ~ therapy, data = anorexia)
summary(m1)

# (b) ---------------------------------------------------------------------

par(mfrow = c(2, 2))
plot(m1, which = 1:4)
par(mfrow = c(1, 1))

# COMMENT: The diagnostic plots do not reveal any major issues.

# (c) ---------------------------------------------------------------------

coef(m1)
# (Intercept)    therapyc    therapyf 
#    3.006897   -3.456897    4.257809 

# COMMENTS: The reference category is the cognitive behavioral therapy group. 

# The intercept (3.006 lbs) corresponds to the mean change in that group. In this case, the significance of the intercept is meaningful (p-value = 0.03), suggesting that the average weight increase in the reference group is likely positive, although the evidence is somewhat borderline.

# The remaining two coefficients represent the differences in mean weight change for the control group and the family therapy group, respectively, relative to the cognitive behavioral therapy group: approximately -3.45 lbs and 4.25 lbs.

# (d) ---------------------------------------------------------------------

# Approach based on linear models
anorexia$therapy <- relevel(anorexia$therapy, ref = "f")
m2 <- lm(change ~ therapy, data = anorexia)
summary(m2)

# Approach based on basic inferential methods (from Statistica II).
# Minor differences in the t-statistics arise from different estimates of the variance.
t.test(anorexia$change[anorexia$therapy == "f"])

# COMMENT: The mean change in the family therapy group is 7.26 lbs, which is significantly different from zero.

# (e) ---------------------------------------------------------------------

anorexia$change_kg <- anorexia$change * 0.453592

m3 <- lm(change ~ therapy, data = anorexia)
summary(m3)

m4 <- lm(change_kg ~ therapy, data = anorexia)
summary(m4)

# COMMENT: The coefficients and standard errors are rescaled because of the unit conversion, but the R-squared values, tests, and t-statistics remain identical. This is a general property of linear models: changing the measurement scale of the response only rescales the coefficients, not the statistical conclusions.