# ---------------------------------------------------------------------
# Heart dataset
# DISCLAIMER: This solution is provided in partial form. Certain steps, justifications, and details have been omitted for clarity and brevity.
# Author: Tommaso Rigon
# ---------------------------------------------------------------------

# The data in the Heart dataframe included in the MLGdata library report the number of confirmed myocardial infarctions in a sample of 360 patients hospitalized with suspected infarction. These data were originally collected in Edinburgh Royal Infirmary.

# For each level of the enzyme Creatine Kinase (IU per liter), grouped into classes (ck), the dataset provides:
  
# the number of confirmed infarctions (ha),  
# the number of non-confirmed infarctions (nha),  
# and the midpoint value of the variable ck (mck).  

# The goal is to evaluate the influence of the Creatine Kinase enzyme level on the probability of infarction. 

rm(list = ls())
library(MLGdata)
data("Heart")
str(Heart)

# (a) ---------------------------------------------------------------------

# Omitted

# (b) ---------------------------------------------------------------------

Heart$num <- Heart$ha + Heart$nha
Heart$prop <- Heart$ha / Heart$num

plot(Heart$mck, Heart$prop, pch = 16)
abline(lm(prop ~ mck, data = Heart), lty = "dotted")

# COMMENT: the linear probability model is catastrophically bad. It predicts probabilities > 1, the fit is terrible. 

# (c) ---------------------------------------------------------------------

# ha ~ Binomial(num, pi_i), independently, with logit(pi_i) = \beta_1 + \beta_2 mck.

# (d) ---------------------------------------------------------------------

m1 <- glm(cbind(ha, nha) ~ mck, family = binomial, data = Heart)
summary(m1)

# (e) Report the estimates and confidence intervals for the coefficients. Provide an interpretation of the obtained estimates.  

confint(m1)

# (f) ---------------------------------------------------------------------

newdata <- data.frame(mck = seq(from = 0, to = 600, length = 200))
fit1 <- predict(m1, newdata = newdata, type = "response")

plot(Heart$mck, Heart$prop, pch = 16)
lines(newdata$mck, fit1)

# (g) ---------------------------------------------------------------------

# Omitted

# (h) Evaluate the goodness of fit of the model.  

# (i) Obtain a 95% confidence interval for the probability of infarction corresponding to a Creatine Kinase value of 150.  

# (l) Assess whether it is appropriate to introduce a quadratic term in mck in the linear predictor and evaluate the goodness of fit of the expanded model.  

m2 <- glm(cbind(ha, nha) ~ mck + I(mck^2), family = binomial, data = Heart)
summary(m2)

# (m) Evaluate whether the introduction of the quadratic term significantly increases the variability of the estimates. Discuss the interpretability of the quadratic model.  

