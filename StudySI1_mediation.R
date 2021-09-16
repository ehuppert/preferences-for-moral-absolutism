#Supplemental Study 1 mediation analysis

#Load packages

library(here)
library(tidyverse)
library(lavaan)


#Read in data

s7 <- read.csv(here("HypocrisyStudySI_1.csv"))

#Model

#IV: proclamation (0 = control, 1 = honesty)

#Mediators: hypocrisy (hypocrisy_1), guilt (discomfort_1)

#DV: Morality (moral)

mult_med_mod_1 = "hypocrisy_1 ~ a1*proclamation
discomfort_1 ~ a2*proclamation
moral ~ b1*hypocrisy_1 + b2*discomfort_1 + c*proclamation

indirect1 := a1*b1
indirect2 := a2*b2
direct := c
total := c + (a1*b1) + (a2*b2) 
#covariances
hypocrisy_1  ~~ discomfort_1 "

fit_mutiple1 = sem(mult_med_mod_1, se = "boot", bootstrap = 1000, data = s7, 
                   likelihood = "wishart")
summary(fit_mutiple1, standardized = T, rsq = T)
parameterEstimates((fit_mutiple1))

