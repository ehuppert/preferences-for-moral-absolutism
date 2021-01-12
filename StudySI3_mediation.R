#Study 5 multiple mediation analysis

#Load packages

library(here)
library(tidyverse)
library(lavaan)
library(psych)

#Read in data

s5 <- read.csv(here("HypocrisyStudy5_cleaned.csv"))

#Reorder proclamation

s5$proclamation <- factor(s5$proclamation, levels = c("flexible", "absolute"))

#Create composite variables

#Hypocrisy
hypocrisy5_df <- data.frame(s5$hypocrisy_1, s5$hypocrisy_2, s5$hypocrisy_3, s5$hypocrisy_4)
summary(hypocrisy5_df)
psych::alpha(hypocrisy5_df) #alpha = 0.86
s5 <- mutate(s5, Hypocrisy5_composite  = ((s5$hypocrisy_1 + s5$hypocrisy_2 + s5$hypocrisy_3 + s5$hypocrisy_4)/4))

#Morality
moral5_df <- data.frame(s5$moral_1, s5$moral_2, s5$moral_3)
psych::alpha(moral5_df) #alpha = 0.94
s5 <- mutate(s5, Moral5_composite  = ((s5$moral_1 + s5$moral_2 + s5$moral_3)/3))

#Model

#IV: proclamation (0 = flexible, 1 = absolute)

#Mediators: discounting_2 (perceived lying frequency), goal_2 (honesty ideals)

#DV: morality (Moral5_composite)

med_mod_exploratory_5 = "discounting_2  ~ a1*proclamation
goal_2 ~ a2*proclamation
Moral5_composite ~ b1*discounting_2 + b2*goal_2 + c*proclamation

indirect1 := a1*b1
indirect2 := a2*b2
direct := c
total := c + (a1*b1) + (a2*b2) 
#covariances
discounting_2  ~~ goal_2"

fit_med_mod_exp_5 = sem(med_mod_exploratory_5, se = "boot", bootstrap = 10000, 
                    data = s5, likelihood = "wishart")
summary(fit_med_mod_exp_5, standardized = T, rsq = T)
parameterEstimates(fit_med_mod_exp_5)

                   