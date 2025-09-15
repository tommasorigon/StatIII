# LAB 4, Statistica III (Generalized Linear Models)
# Title: "Binary and binomial regression". Datasets: Beetles, Credit scoring
# Author: Tommaso Rigon


# Dataset 1: Beetles data-----------------------------------------------------------------------

rm(list = ls())

# Dataset 2: Credit scoring -----------------------------------------------------------------------
rm(list = ls())
library(MLGdata)
data("Credit")

# Data for 1000 clients of a south german bank, 700 good payers and 300 bad payers. They are used to construct a credit scoring method.
str(Credit)

summary(Credit)

# This is a case-control study, therefore logistic regression is very much a good idea