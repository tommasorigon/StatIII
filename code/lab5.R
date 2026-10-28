# LAB 5, Statistica III (Generalized Linear Models)
# Title: "Miscellanea". Datasets: Drugs, Cancer
# Author: Tommaso Rigon

# -------------------------------------------------------------------
# Dataset 1: Drugs - APPLIED ANALYSIS
# -------------------------------------------------------------------

# Data refers to a survey by Wright State University that asked 2276 students in their final year of high school in a rural area near Dayton, Ohio whether they had ever used alcohol, cigarettes, or marijuana. 

library(MLGdata)
data(Drugs2)
Drugs2

# M_yes = number of people who tried marijuana 
# M_no = number of people who haven't tried marijuana
# alc = Alcohol, sig = cigarettes

# QUESTION 1: does the usage of alcohol and cigarettes predict the usage of marijuana?
# QUESTION 2: is there an interaction effect between alcohol and cigarettes consumption?

Drugs2$prop <- Drugs2$M_yes / Drugs2$n
Drugs2

m_additive <- glm(cbind(M_yes, M_no) ~ alc + sig, family = binomial, data = Drugs2)
summary(m_additive)

# The odds ratios are very strong
exp(coef(m_additive)[c(2, 3)])

Drugs2$pred_additive <- fitted(m_additive)
Drugs2

m_full <- glm(cbind(M_yes, M_no) ~ alc * sig, family = binomial, data = Drugs2)
summary(m_full)

# The deviance is 0 (up to numerical error), why? This is indeed the saturated model. The predictions coincide with the proportions
Drugs2$full <- fitted(m_full)
Drugs2

# The usual test is equivalent to a goodness of fit, which suggests that an interaction effect is not necessary.
anova(m_additive, m_full, test = "Chisq")

# -------------------------------------------------------------------
# Dataset 2: Cancer - APPLIED ANALYSIS
# -------------------------------------------------------------------

rm(list = ls())

# The data, from Holford (1980), report survival and death outcomes for 539 males diagnosed with lung cancer. The prognostic factors considered are histology and stage of disease, with observations grouped into 2-month intervals of follow-up after diagnosis. For each combination of follow-up interval, histology, and disease stage, the data provides the number of deaths and the total number of person-months at risk during that interval. We treat the death counts in the table as independent Poisson variates.

cancer <- read.table("../data/Cancer.dat", header = TRUE)
cancer <- read.table("https://tommasorigon.github.io/StatIII/data/Cancer.dat", header = TRUE) # Download the file from the internet
str(cancer)

# count     = number of deaths in the cell
# risktime  = person-months at risk in the cell (see below)
# time      = follow-up window: 7 consecutive windows of 2 months each
# histology = type of histology (3 levels)
# stage     = stage of the disease (3 levels, 3 is the most advanced)

cancer$histology <- as.factor(cancer$histology)
cancer$stage <- as.factor(cancer$stage)
cancer$time <- as.factor(cancer$time)

# -------------------------------------------------------------------
# What is risktime?
# -------------------------------------------------------------------

# All the windows have the SAME length (2 months). risktime is an exposure measured in person-months: the total time that the patients of that cell have been observed alive within the window.
# - a patient alive for the whole window contributes 2 months;
# - a patient who dies after 0.5 months contributes 0.5 months.
aggregate(risktime ~ time, data = cancer, FUN = sum)
# The total of the first window is 925, below the maximum 2 x 539 = 1078. It then decreases down to 255, because fewer patients are at risk.

# The exposure varies a lot across cells (from 6 to 212 person-months), hence the number of deaths alone is not comparable. We model the death rate \lambda_i = \mu_i / t_i, where Y_i ~ Pois(\mu_i) and t_i = risktime_i.
# Since log(\mu_i) = log(t_i) + x_i^T \beta, this is done by including log(t_i) as an offset (a covariate with coefficient fixed to 1)
cancer$logrisktime <- log(cancer$risktime)

# -------------------------------------------------------------------
# Descriptive analysis
# -------------------------------------------------------------------

# Overall: 353 deaths over 3475 person-months, i.e. about 10 deaths every 100 person-months
sum(cancer$count) / sum(cancer$risktime)

# Crude death rates (deaths / person-months). Stage: sharp increase (0.046, 0.075, 0.183)
tapply(cancer$count, cancer$stage, sum) / tapply(cancer$risktime, cancer$stage, sum)

# Histology: apparent increase (0.090, 0.105, 0.142), which is due to the stage (see below)
tapply(cancer$count, cancer$histology, sum) / tapply(cancer$risktime, cancer$histology, sum)

# Time: deaths drop from 121 to 19, mostly because the person-months drop (925 to 255). The rate decreases much less (0.131 to 0.075) and not monotonically.
tapply(cancer$count, cancer$time, sum)
tapply(cancer$count, cancer$time, sum) / tapply(cancer$risktime, cancer$time, sum)

# Histology and stage: 62% of the person-months of histology 3 are in stage 3 (284 out of 458), against 31% for histology 1 and 29% for histology 2.
# Within each stage the rates barely differ across histologies, hence the apparent effect of histology 3 is confounded by the stage.
deaths_hist_stage <- xtabs(count ~ histology + stage, data = cancer)
pm_hist_stage <- xtabs(risktime ~ histology + stage, data = cancer)
pm_hist_stage
round(deaths_hist_stage / pm_hist_stage, 3)

# Time and stage: the person-months drop faster for stage 3 (443 to 46) than for stage 1 (255 to 124). The rate of stage 3 is always the highest, stages 1 and 2 are close and noisy.
# There is no clear sign that the pattern over time differs across stages.
deaths_time_stage <- xtabs(count ~ time + stage, data = cancer)
pm_time_stage <- xtabs(risktime ~ time + stage, data = cancer)
pm_time_stage
round(deaths_time_stage / pm_time_stage, 3)

# -------------------------------------------------------------------
# Poisson regression with offset
# -------------------------------------------------------------------

m_full <- glm(count ~ histology + stage + time, family = poisson, offset = logrisktime, data = cancer)
summary(m_full)

# What if we ignore the exposure? The time effects become strongly negative (fewer patients are at risk), the histology effects change sign and the deviance is much larger (80.0 against 43.9).
m_no_offset <- glm(count ~ histology + stage + time, family = poisson, data = cancer)
coef(m_no_offset)
deviance(m_no_offset)
deviance(m_full)

# Diagnostics plots
par(mfrow = c(2, 2))
plot(m_full, which = 1:4)
par(mfrow = c(1, 1))

# The dispersion is below 1 (0.83): there is no evidence of overdispersion
X2 <- sum(residuals(m_full, type = "pearson")^2)
X2 / m_full$df.residual

# INTERPRETATION: the exponentiated coefficients are RATE RATIOS (ratios of deaths per person-month), not odds ratios
exp(coef(m_full))
exp(confint(m_full))

# - Intercept: the baseline rate (histology 1, stage 1, first window) is 0.049, i.e. about 5 deaths every 100 person-months.
# - Stage: the estimated death rate at the third stage of disease is 3.76 = exp(1.32431) times that at the first stage, adjusting for follow-up time and histology. The corresponding 95% confidence interval is (2.81, 5.11), confirming a clear increase in death rate with advancing stage. The rate at the second stage is 1.60 times that at the first stage.
# - Histology: the rate ratios w.r.t. histology 1 are 1.18 and 1.11, with confidence intervals including 1.
# - Time: only the fifth window is significantly lower than the first (rate ratio 0.51). Since the effect is not monotone, we test the factor as a whole.

# COMMENTS: the histology is not important IF we already know the cancer stage (deviance difference 1.9 on 2 df, p-value 0.39). This does not contradict the crude rates, which are confounded by the stage.
m_no_histology <- glm(count ~ stage + time, family = poisson, offset = logrisktime, data = cancer)
anova(m_no_histology, m_full, test = "Chisq")

# The risk of death appears to be roughly constant over time (deviance difference 11.6 on 6 df, p-value 0.07, which is borderline, suggesting a possible slight negative trend). In other words, the death rate does not change significantly over time, provided that the cancer stage remains the same.
m_stage <- glm(count ~ stage, family = poisson, offset = logrisktime, data = cancer)
anova(m_stage, m_no_histology, test = "Chisq")

X2 <- sum(residuals(m_stage, type = "pearson")^2)
X2 / m_stage$df.residual

# Final model: with the stage only, the fitted rates coincide with the crude rates by stage (0.046, 0.075, 0.183). The rate ratios 1.62 and 3.96 are similar to those adjusted for histology and time (1.60 and 3.76).
exp(coef(m_stage))
