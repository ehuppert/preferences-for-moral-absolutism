#Study SI3 multiple mediation analysis

#Load packages

library(here)
library(tidyverse)
library(lavaan)
library(psych)

#Read in data

s9 <- read.csv(here("HypocrisyStudySI_3.csv"))

#Proclamation
s9$proclamation <- as.factor(s9$proclamation)
levels(s9$proclamation)
#0 = absolute, 1 = flexible


#Create composite variables

#Hypocrisy
hypocrisySI3_df <- data.frame(s9$hypocrisy_1, s9$hypocrisy_2, s9$hypocrisy_3, s9$hypocrisy_4)
summary(hypocrisySI3_df)
psych::alpha(hypocrisySI3_df) #alpha = 0.86
s9 <- mutate(s9, HypocrisySI3_composite  = ((s9$hypocrisy_1 + s9$hypocrisy_2 + s9$hypocrisy_3 + s9$hypocrisy_4)/4))

#Morality
moralSI3_df <- data.frame(s9$moral_1, s9$moral_2, s9$moral_3)
psych::alpha(moralSI3_df) #alpha = 0.94
s9 <- mutate(s9, MoralSI3_composite  = ((s9$moral_1 + s9$moral_2 + s9$moral_3)/3))

#Model

#IV: proclamation (0 = absolute, 1 = flexible)

#Mediators: discounting_2 (perceived lying frequency), goal_2 (honesty ideals)

#DV: morality (MoralSI3_composite)

med_mod_exploratory_8 = "discounting_2  ~ a1*proclamation
goal_2 ~ a2*proclamation
MoralSI3_composite ~ b1*discounting_2 + b2*goal_2 + c*proclamation

indirect1 := a1*b1
indirect2 := a2*b2
direct := c
total := c + (a1*b1) + (a2*b2) 
#covariances
discounting_2  ~~ goal_2"

fit_med_mod_exp_8 = sem(med_mod_exploratory_8, se = "boot", bootstrap = 1000, 
                    data = s9, likelihood = "wishart")
summary(fit_med_mod_exp_8, standardized = T, rsq = T)
parameterEstimates(fit_med_mod_exp_8)

# Conduct with Counts as Lying added as well

#IV: proclamation (0 = absolute, 1 = flexible)

#Mediators: discounting_2 (perceived lying frequency), goal_2 (honesty ideals), counts as lying

#DV: morality (MoralSI3_composite)

med_mod_exploratory_9 = "discounting_2  ~ a1*proclamation
goal_2 ~ a2*proclamation
counts ~ a3*proclamation
MoralSI3_composite ~ b1*discounting_2 + b2*goal_2 + b3*counts + c*proclamation

indirect1 := a1*b1
indirect2 := a2*b2
indirect3 := a3*b3
direct := c
total := c + (a1*b1) + (a2*b2) + (a3*b3)
#covariances
discounting_2  ~~ goal_2
discounting_2  ~~ counts
goal_2 ~~ counts
"

fit_med_mod_exp_9 = sem(med_mod_exploratory_9, se = "boot", bootstrap = 1000, 
                        data = s9, likelihood = "wishart")
summary(fit_med_mod_exp_9, standardized = T, rsq = T)
parameterEstimates(fit_med_mod_exp_9)
                   