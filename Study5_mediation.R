#Study 4 multiple mediation analysis

#Load packages

library(here)
library(tidyverse)
library(lavaan)
library(psych)

#Read in data

s4 <- read.csv(here("HypocrisyStudy4_cleaned.csv"))

#Reorder proclamation

s4$proclamation <- factor(s4$proclamation, levels = c("ambiguous", "absolute"), labels = c("flexible", "absolute"))

#Create composite variables

#Time one
#Hypocrisy
hypocrisy_4_check <- data.frame(s4$hypocrisy_1, s4$hypocrisy_2, s4$hypocrisy_4, s4$hypocrisy_5)
summary(hypocrisy_4_check)
describe(hypocrisy_4_check)
psych::alpha(hypocrisy_4_check) # raw alpha = 0.89 
s4 <- mutate(s4, Hypocrisy.1_composite4 = ((s4$hypocrisy_1 + s4$hypocrisy_2 + s4$hypocrisy_4 + s4$hypocrisy_5)/4))

#Morality
morality4_check <- data.frame(s4$moral_1, s4$moral_2, s4$moral_3)
summary(morality4_check )
describe(morality4_check )
psych::alpha(morality4_check ) # raw = 0.97
s4 <- mutate(s4, Moral.1_composite4 = ((s4$moral_1 +s4$moral_2 + s4$moral_3)/3))

#Future Honesty
Honest4_df <- data.frame(s4$honest_freq_1, s4$honest_likely_1, s4$honest_extreme_1, s4$committment_honesty_1)
summary(Honest4_df)
describe(Honest4_df)
psych::alpha(Honest4_df) # raw= 0.85
s4 <- mutate(s4, Honest.1_composite4= ((s4$honest_freq_1 + s4$honest_likely_1 + s4$honest_extreme_1 + s4$committment_honesty_1)/4))

#Time two
#Hypocrisy
hypocrisy_4_check.2 <- data.frame(s4$hypocrisy_1.2, s4$hypocrisy_2.2, s4$hypocrisy_4.2, s4$hypocrisy_5.2)
summary(hypocrisy_4_check.2)
describe(hypocrisy_4_check.2)
psych::alpha(hypocrisy_4_check.2) #alpha = 0.91
s4 <- mutate(s4, 
             Hypocrisy.2_composite4 = (s4$hypocrisy_1.2 + s4$hypocrisy_2.2 + s4$hypocrisy_4.2 + s4$hypocrisy_5.2)/4)

#Morality
morality4_check.2 <- data.frame(s4$moral_1.2, s4$moral_2.2, s4$moral_3.2)
summary(morality4_check.2)
describe(morality4_check.2)
psych::alpha(morality4_check.2) #alpha = 0.97
s4 <- mutate(s4, Moral.2_composite4  = ((s4$moral_1.2 + s4$moral_2.2 + s4$moral_3.2)/3))

#Future Honesty
Honest4_df.2  <- data.frame(s4$honest_freq_2, s4$honest_likely_2, s4$honest_extreme_2, s4$committment_honesty_2)
summary(Honest4_df.2)
describe(Honest4_df.2)
psych::alpha(Honest4_df.2) #alpha = 0.82
s4 <- mutate(s4, Honest.2_composite4 = ((s4$honest_freq_2 + s4$honest_likely_2 + s4$honest_extreme_2 + s4$committment_honesty_2)/4))


#Model

#IV: proclamation (0 = flexible, 1 = absolute)

#Mediators at time two: future honesty (Honest.2_composite4), hypocrisy (Hypocrisy.2_composite4)

#DV: morality (Moral.2_composite4)


mod_med_4 = "Hypocrisy.2_composite4  ~ a1*proclamation
Honest.2_composite4  ~ a2*proclamation
Moral.2_composite4 ~ b1*Hypocrisy.2_composite4  + b2*Honest.2_composite4  + c*proclamation

indirect1 := a1*b1
indirect2 := a2*b2
direct := c
total := c + (a1*b1) + (a2*b2) 
#covariances
Hypocrisy.2_composite4   ~~ Honest.2_composite4"

fit_s4 = sem(mod_med_4, se = "boot", bootstrap = 10000, 
             data = s4, likelihood = "wishart")

#Model Summary
summary(fit_s4, standardized = T, rsq = T)
parameterEstimates((fit_s4))
