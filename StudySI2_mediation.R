#Supplemental Study 2 mediation analysis

#Load packages

library(here)
library(tidyverse)
library(lavaan)
library(psych)

#Read in data

s8 <- read.csv(here("HypocrisyStudySI_2.csv"))

#0 = absolute, 1 = flexible
s8$proclamation <- as.factor(s8$proclamation)
levels(s8$proclamation)

#Composite Evaluation Variables

#Hypocrisy
hypocrisy7_df <- data.frame(s8$hypocrisy_1, s8$hypocrisy_2, s8$hypocrisy_3)
summary(hypocrisy7_df)
describe(hypocrisy7_df)
psych::alpha(hypocrisy7_df) #alpha = 0.86
s8<- mutate(s8, hypocrisy7_composite = ((s8$hypocrisy_1 + s8$hypocrisy_2 + s8$hypocrisy_3)/3))

#Morality
cor.test(s8$moral_1, s8$moral_2) #r = 0.91
s8<- mutate(s8, moral7_composite  = ((s8$moral_1 + s8$moral_2)/2))

#Model

#IV: proclamation (0 = absolute, 1 = flexible)

#Mediators: hypocrisy (hypocrisy7_composite), guilt (discomfort_1)

#DV: morality (moral7_composite)

mult_med_mod_7 = "hypocrisy7_composite ~ a1*proclamation
discomfort_1 ~ a2*proclamation
moral7_composite~ b1*hypocrisy7_composite + b2*discomfort_1 + c*proclamation

indirect1 := a1*b1
indirect2 := a2*b2
direct := c
total := c + (a1*b1) + (a2*b2) 
#covariances
hypocrisy7_composite ~~ discomfort_1"

fit_mutiple7 = sem(mult_med_mod_7, se = "boot", bootstrap = 1000, data = s8, 
                   likelihood = "wishart")
summary(fit_mutiple7, standardized = T, rsq = T)
parameterEstimates((fit_mutiple7))
