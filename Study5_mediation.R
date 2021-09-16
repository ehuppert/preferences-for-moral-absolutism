#Study 5 multiple mediation analysis

#Load packages

library(here)
library(tidyverse)
library(lavaan)
library(psych)

#Read in data

s5 <- read.csv(here("HypocrisyStudy5_cleaned.csv"))

#Reorder proclamation

s5$proclamation <- factor(s5$proclamation, levels = c("ambiguous", "absolute"), labels = c("flexible", "absolute"))

#Create composite variables

#Time one
#Hypocrisy
hypocrisy_5_check <- data.frame(s5$hypocrisy_1, s5$hypocrisy_2, s5$hypocrisy_4, s5$hypocrisy_5)
summary(hypocrisy_5_check)
describe(hypocrisy_5_check)
psych::alpha(hypocrisy_5_check) # raw alpha = 0.89 
s5 <- mutate(s5, Hypocrisy.1_composite5 = ((s5$hypocrisy_1 + s5$hypocrisy_2 + s5$hypocrisy_4 + s5$hypocrisy_5)/4))

#Morality
morality5_check <- data.frame(s5$moral_1, s5$moral_2, s5$moral_3)
summary(morality5_check )
describe(morality5_check )
psych::alpha(morality5_check ) # raw = 0.97
s5 <- mutate(s5, Moral.1_composite5 = ((s5$moral_1 +s5$moral_2 + s5$moral_3)/3))

#Future Honesty
Honest5_df <- data.frame(s5$honest_freq_1, s5$honest_likely_1, s5$honest_extreme_1, s5$committment_honesty_1)
summary(Honest5_df)
describe(Honest5_df)
psych::alpha(Honest5_df) # raw= 0.85
s5 <- mutate(s5, Honest.1_composite5 = ((s5$honest_freq_1 + s5$honest_likely_1 + s5$honest_extreme_1 + s5$committment_honesty_1)/4))

#Time two
#Hypocrisy
hypocrisy_5_check.2 <- data.frame(s5$hypocrisy_1.2, s5$hypocrisy_2.2, s5$hypocrisy_4.2, s5$hypocrisy_5.2)
summary(hypocrisy_5_check.2)
describe(hypocrisy_5_check.2)
psych::alpha(hypocrisy_5_check.2) #alpha = 0.91
s5 <- mutate(s5, 
             Hypocrisy.2_composite5 = (s5$hypocrisy_1.2 + s5$hypocrisy_2.2 + s5$hypocrisy_4.2 + s5$hypocrisy_5.2)/4)

#Morality
morality5_check.2 <- data.frame(s5$moral_1.2, s5$moral_2.2, s5$moral_3.2)
summary(morality5_check.2)
describe(morality5_check.2)
psych::alpha(morality5_check.2) #alpha = 0.97
s5 <- mutate(s5, Moral.2_composite5  = ((s5$moral_1.2 + s5$moral_2.2 + s5$moral_3.2)/3))

#Future Honesty
Honest5_df.2  <- data.frame(s5$honest_freq_2, s5$honest_likely_2, s5$honest_extreme_2, s5$committment_honesty_2)
summary(Honest5_df.2)
describe(Honest5_df.2)
psych::alpha(Honest5_df.2) #alpha = 0.82
s5 <- mutate(s5, Honest.2_composite5 = ((s5$honest_freq_2 + s5$honest_likely_2 + s5$honest_extreme_2 + s5$committment_honesty_2)/4))


#Model

#IV: proclamation (0 = flexible, 1 = absolute)

#Mediators at time two: future honesty (Honest.2_composite5), hypocrisy (Hypocrisy.2_composite5)

#DV: morality (Moral.2_composite5)


mod_med_5 = "Hypocrisy.2_composite5  ~ a1*proclamation
Honest.2_composite5  ~ a2*proclamation
Moral.2_composite5 ~ b1*Hypocrisy.2_composite5  + b2*Honest.2_composite5  + c*proclamation

indirect1 := a1*b1
indirect2 := a2*b2
direct := c
total := c + (a1*b1) + (a2*b2) 
#covariances
Hypocrisy.2_composite5   ~~ Honest.2_composite5"

fit_s5 = sem(mod_med_5, se = "boot", bootstrap = 1000, 
             data = s5, likelihood = "wishart")

#Model Summary
summary(fit_s5, standardized = T, rsq = T)
parameterEstimates((fit_s5))
