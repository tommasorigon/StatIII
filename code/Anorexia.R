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

boxplot(after - before ~ therapy, data = anorexia)

# (a) Fit a linear model where the outcome is the [post-treatment weight]{.blue} and the predictors are the [baseline weight]{.orange} and the [therapy group]{.orange}

m1 <- lm( after - before ~ therapy, data = anorexia)
summary(m1)

par(mfrow = c(2, 2))
plot(m1, which = 1:4)
par(mfrow = c(1, 1))


# (b) Check the underlying assumptions of the fitted models. If they are violated, propose a solution. 

# (c) [Interpret]{.blue} the estimated parameters of the model. 

# (d) Convert the weights to kilograms ($1\ \text{lb} = 0.453592\ \text{kg}$), repeat the analysis, and discuss whether the results change and why.  

# Optional: change the baseline for therapy
contrasts(anorexia$therapy) <- contr.treatment(n = 3, base = 2)
colnames(contrasts(anorexia$therapy)) <- c("b", "f")
