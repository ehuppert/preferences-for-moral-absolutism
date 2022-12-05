#Politician multiple mediation analysis

#Load packages

library(here)
library(lavaan)
library(psych)
library(tidyverse)

#Read in data

s1 <- read.csv(here("HypocrisyStudy1_cleaned.csv"))

s1$Proclamation <- as.factor(s1$Proclamation)
levels(s1$Proclamation)
#absolute = 0, flexible = 1

#Create composite variables

#Hypocrisy
hypocrisy1_check.df <- data.frame(s1$Hypocrisy_1, s1$Hypocrisy_2, s1$Hypocrisy_3, s1$Hypocrisy_4, s1$Hypocrisy_5)
summary(hypocrisy1_check.df)
describe(hypocrisy1_check.df)
psych::alpha(hypocrisy1_check.df) # raw alpha = 0.90
s1 <-  mutate(s1, 
              Hypocrisy_composite = ((s1$Hypocrisy_1 + s1$Hypocrisy_2 + s1$Hypocrisy_3 + s1$Hypocrisy_4 + s1$Hypocrisy_5)/5))

#Morality
moral1_check.df <- data.frame(s1$Moral_1, s1$Moral_2, s1$Moral_3)
summary(moral1_check.df)
describe(moral1_check.df)
psych::alpha(moral1_check.df) # rawalpha = 0.95
s1 <- mutate(s1, 
             Moral_composite = ((s1$Moral_1 + s1$Moral_2 + s1$Moral_3)/3))

#Future Honesty
honest1_check.df <- data.frame(s1$Honest_frequency, s1$Honest_likelihood, s1$Honest_extreme, s1$Honest_committment)
summary(honest1_check.df)
describe(honest1_check.df)
psych::alpha(honest1_check.df) # raw alpha = 0.76
s1<- mutate(s1, 
            Honest_composite = ((s1$Honest_frequency + s1$Honest_likelihood + s1$Honest_extreme + s1$Honest_committment)/4))


#Morality model

#IV: proclamation (0 = absolute, 1 = flexible)

#Mediators: future honesty (Honest_composite), hypocrisy (Hypocrisy_composite)

#DV: morality (Moral_composite)

multipleMed_politics = "Hypocrisy_composite  ~ a1*Proclamation 
Honest_composite ~ a2*Proclamation 
Moral_composite ~ b1*Hypocrisy_composite + b2*Honest_composite + c*Proclamation 

indirect1 := a1*b1
indirect2 := a2*b2
direct := c
total := c + (a1*b1) + (a2*b2) 
#covariances
Hypocrisy_composite ~~ Honest_composite"

fit_mult_politics = sem(multipleMed_politics, se = "boot", bootstrap = 1000, data = s1, 
                        likelihood = "wishart")
summary(fit_mult_politics, standardized = T, rsq = T)
parameterEstimates((fit_mult_politics))

#Voting intentions model

#IV: proclamation (0 = absolute, 1 = flexible)

#Mediators: future honesty (Honest_composite), hypocrisy (Hypocrisy_composite)

#DV: voting intentions (Voting)

multipleMed_politics_vote = "Hypocrisy_composite  ~ a1*Proclamation 
Honest_composite ~ a2*Proclamation 
Voting~ b1*Hypocrisy_composite + b2*Honest_composite + c*Proclamation 

indirect1 := a1*b1
indirect2 := a2*b2
direct := c
total := c + (a1*b1) + (a2*b2) 
#covariances
Hypocrisy_composite  ~~ Honest_composite"

fit_mult_politics_vote = sem(multipleMed_politics_vote, se = "boot", bootstrap = 1000, data = s1, 
                             likelihood = "wishart")
summary(fit_mult_politics_vote, standardized = T, rsq = T)
parameterEstimates((fit_mult_politics_vote))



