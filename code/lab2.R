# LAB 2, Statistica III (Generalized Linear Models)
# Title: "Generalized linear models". Datasets: Clotting, Chimps
# Author: Tommaso Rigon

# Overview of the GLM function -----------------------------------------------------------

# Dataset 1: Clotting---------------------------------------------------------------------
rm(list = ls())

library(MLGdata)
# Blood clotting times. Mean blood clotting times in seconds for nine percentage concentrations of normal plasma and two lots of clotting agent. McCullagh, P. and Nelder, J. A. (1989) Generalized Linear Models (2nd Edition). London: Chapman and Hall.
data("Clotting")

str(Clotting)

# Dataset 2: Chimps ----------------------------------------------------------------------
rm(list = ls())
data("Chimps")

str(Chimps)
