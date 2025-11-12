# ---------------------------------------------------------------------
# Endometrial dataset
# DISCLAIMER: This solution is provided in partial form. Certain steps, justifications, and details have been omitted for clarity and brevity.
# Author: Tommaso Rigon
# ---------------------------------------------------------------------

# Heinze and Schemper (2002) described a study about endometrial cancer that analyzed how HG = histology grade (0 = low grade, 1 = high grade) relates to three risk factors: NV = neovasculation (1= present, 0 = absent), PI = pulsatility index of arteria uterina (ranging from 0 to 49), and EH = endometrium height (ranging from 0.27 to 3.61). 

# Import the data and consider the main effects logistic regression model in which the binary variable HG is the response and NV, PI, and EH are the covariates. 

rm(list = ls())
Endometrial <- read.table("https://tommasorigon.github.io/StatIII/data/Endometrial.txt", header = TRUE)

# (a) ---------------------------------------------------------------------

table(Endometrial$HG, Endometrial$NV)

# COMMENT: When NV = 1, the proportion of high-grade histology is 1;  that is, all patients have high-grade histology whenever neovascularization is present.  Therefore, conditionally on NV, we may obtain "degenerate probabilities."

# (b) ---------------------------------------------------------------------

m1 <- glm(HG ~ NV + PI + EH, family = "binomial", data = Endometrial)

summary(m1)
confint(m1)

# COMMENT: On my laptop, I get warning messages when running the confint command. However, there are a few aspects that  appear suspicious. First, the number of Fisher Scoring iterations (17) is unusually large, suggesting that the algorithm required many steps before reaching "convergence." Second, the point estimate for beta_2 (associated with NV) is extremely  large, which is unexpected given that NV is a dummy variable. The corresponding odds ratio, exp(beta_2) = 79,047,540,  is nonsensically high. The standard error is also excessively large, far beyond any reasonable value. 

# What actually happened? The issue with this dataset is known as quasi-separation, which complicates estimation because the maximum likelihood estimate for some coefficients does not exist (recall the table above...). The algorithm fails to converge because the likelihood is unbounded (e.g., beta_2 → ∞). This dataset is discussed in detail in Section 5.7.1 of Agresti (2015).

# With the tools covered in Statistica III, there is nothing we can do to fix this issue. We must accept that the maximum likelihood approach fails in this case and that the appropriate remedy would be to collect additional data.

# An advanced solution that does not require collecting additional data is provided below, but its details are obviously beyond the scope of the exam.

library(brglm2) 
# Logistic regression model in which an alternative estimator is used (not the MLE)
m_corrected <- glm(HG ~ NV + PI + EH, family = "binomial", data = Endometrial, method = "brglmFit")
summary(m_corrected)

