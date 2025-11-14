# ---------------------------------------------------------------------
# Germination dataset
# DISCLAIMER: This solution is provided in partial form. Certain steps, justifications, and details have been omitted for clarity and brevity.
# Author: Tommaso Rigon
# ---------------------------------------------------------------------

# The data contained in the Germination dataframe, that can be found in the MLGdata library, were obtained from a 2 x 2 factorial experiment (two factors seed and root, each with two levels) to compare two seeds, Orobanche aegyptiaca 75 (seed = 075) and Orobanche aegyptiaca 73 (seed = 073), germinated on two different root extracts: bean (root = F) and cucumber (root = C). 

# For each combination, the total number of seeds m_i (variable m) and the number of seeds that germinate S_i (variable s) are reported.  

rm(list = ls())
library(MLGdata)
data("Germination")

str(Germination) # The variable are correctly encoded
Germination

# (a) ---------------------------------------------------------------------

# Omitted

# (b) ---------------------------------------------------------------------

# Model without interaction (canonical link):

# S_i ~ Binomial(m_i, pi_i), and logit(pi_i) = \beta_1 + \beta_2 I(seed = 075) + \beta_3 I(root = F)

# Model with interaction (canonical link):

# S_i ~ Binomial(m_i, pi_i), and logit(pi_i) = \beta_1 + \beta_2 I(seed = 075) + \beta_3 I(root = F) + \beta_4 I(seed = 075, root = F).

# (c) --------------------------------------------------------------------- 

# Note that there are only four distinct values of the covariates, i.e., the combinations of seed and root. This means that the data can be grouped, yielding the following dataset with 4 observations.

Germination_grouped <- aggregate(cbind(s, m) ~ seed + root, sum, data = Germination) # Grouping
Germination_grouped$prop <- Germination_grouped$s / Germination_grouped$m # Adding empirical proportions
Germination_grouped

# Hence, a logistic model with an interaction term can be fitted (using the original data) as follows:
m2 <- glm(cbind(s, m - s) ~ seed * root, data = Germination, family = binomial)
summary(m2)

# From the theory discussed in the slides, we know that this dataset has the same likelihood as the original one under a binomial model, which means that we obtain the same estimates using the grouped dataset as well.
m2_group <- glm(cbind(s, m - s) ~ seed * root, data = Germination_grouped, family = binomial)
summary(m2_group)

# There are only four distinct estimated probabilities, corresponding to the four configurations of seed and root:
fitted(m2_group)

# These values coincide with the empirical proportions. More explicitly:
Germination_grouped

plogis(coef(m2)[1])                               # seed = 073 with root = C
plogis(coef(m2)[1] + coef(m2)[2])                 # seed = 075 with root = C
plogis(coef(m2)[1] + coef(m2)[3])                 # seed = 073 with root = F
plogis(coef(m2)[1] + coef(m2)[2] + coef(m2)[3] + coef(m2)[4]) # seed = 075 with root = F

# (d) --------------------------------------------------------------------- 

m1 <- glm(cbind(s, m - s) ~ seed  +  root, data = Germination, family = binomial)
summary(m1)

anova(m1, m2)
anova(m1, m2, test = "Rao")

# COMMENT: The interaction effect appears to be relevant, although somewhat borderline (with p-values around 0.01). All tests—Wald (from the summary), LRT, and Rao—reject the null hypothesis at the 5% level. In the presence of overdispersion, however, this conclusion could potentially be reversed.

# (e) --------------------------------------------------------------------- 
  
coef(m2)
confint(m2)

# COMMENT: The interpretation is somewhat complex due to the presence of the interaction, which requires considering all possible combinations of seed and root. Overall, root = F (bean) is associated with a lower probability of germination compared to root = C (cucumber), and this disadvantage becomes slightly stronger when combined with seed 073 (interaction effect). Seed 075 shows a small positive marginal effect.

# ADVANCED COMMENT: Once we account for overdispersion (see Exercises E), we find that the seed effect (both the main effect and the interaction) is not significant, and the following simpler model is more appropriate:

m_quasi <- glm(cbind(s, m - s) ~ root, data = Germination, family = quasibinomial)
confint(m_quasi)

# From this model, the interpretation becomes simpler: seed is irrelevant in determining the germination probability, whereas root = F has a strong negative effect.

# (f) ---------------------------------------------------------------------

# Omitted

# (g) ---------------------------------------------------------------------

par(mfrow = c(2, 2))
plot(m2, which = 1:4)
par(mfrow = c(1, 1))

X2 <- sum(residuals(m2, type = "pearson")^2)
X2
1 - pchisq(X2, df = m2$df.residual)

phi_tilde <- X2 / m2$df.residual
phi_tilde

# COMMENT: As anticipated, there are some relatively large residuals (p-value of X2 statistic is about 0.01) and a somewhat high estimate of phi, both of which suggest a degree of overdispersion.