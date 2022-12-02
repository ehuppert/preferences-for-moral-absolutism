#Load packages

library(here)
library(tidyverse)
library(lavaan)
library(psych)

#Read in data

s13 <- read.csv(here("HypocrisyStudySI7_cleaned.csv")) #265


# Add in composite variables
#Hypocrisy
hypocrisy2_check <- data.frame(s13$Hypocrisy_1, s13$Hypocrisy_2, s13$Hypocrisy_3, s13$Hypocrisy_4, s13$Hypocrisy_5)
summary(hypocrisy2_check)
describe(hypocrisy2_check)
psych::alpha(hypocrisy2_check) #alpha = 0.89 
s13 <- mutate(s13, Hypocrisy_composite2= 
               ((s13$Hypocrisy_1 + s13$Hypocrisy_2 + s13$Hypocrisy_3 + s13$Hypocrisy_4 +   s13$Hypocrisy_5)/5))

#Morality
moral2_check <- data.frame(s13$Moral_1, s13$Moral_2, s13$Moral_3)
summary(moral2_check)
describe(moral2_check)
psych::alpha(moral2_check) #alpha = 0.94 

s13 <- mutate(s13, Moral_composite2 = ((s13$Moral_1 + s13$Moral_2 +s13$Moral_3)/3))

#Future Honesty
honesty2_check  <- data.frame(s13$Honest_frequency, s13$Honest_likelihood, s13$Honest_extreme, s13$Honest_committment)
summary(honesty2_check)
describe(honesty2_check)
psych::alpha(honesty2_check) #alpha = 0.74
s13 <- mutate(s13, Honest_composite2 = 
               ((s13$Honest_frequency + s13$Honest_likelihood + s13$Honest_extreme + s13$Honest_committment)/4))

#Social benefit of the proclamation
cor.test(s13$Social_1, s13$Social_2) #r = 0.87

s13 <- mutate(s13, SI_composite2 = ((s13$Social_1 + s13$Social_2)/2))


# Use subsetted data with no control condition
absflex_only <- s13 %>%
  filter (Proclamation != "control")

glimpse(absflex_only$Proclamation)
absflex_only$Proclamation <- as.factor(absflex_only$Proclamation)
absflex_only$Proclamation <- droplevels(absflex_only$Proclamation)
levels(absflex_only$Proclamation)

#MORAL 
#IV: proclamation (1 = flexible, 0 = absolute)

#Mediators: future honesty (Honest_composite2), hypocrisy (Hypocrisy_composite2), social benefit (SI_composite2)

#DV: morality (Moral_composite2)

multipleMed_politics13 = "Hypocrisy_composite2  ~ a1*Proclamation
Honest_composite2 ~ a2*Proclamation
SI_composite2 ~ a3*Proclamation
Moral_composite2 ~ b1*Hypocrisy_composite2  + b2*Honest_composite2 + b3*SI_composite2 + c*Proclamation

indirect1 := a1*b1
indirect2 := a2*b2
indirect3 := a3*b3
direct := c
total := c + (a1*b1) + (a2*b2) + (a3*b3)
#covariances
Hypocrisy_composite2   ~~ Honest_composite2
Hypocrisy_composite2  ~~ SI_composite2
Honest_composite2 ~~ SI_composite2
"

fit_mult_politics_new2 = sem(multipleMed_politics13 , se = "boot", bootstrap = 1000, data = absflex_only, likelihood = "wishart")
summary(fit_mult_politics_new2, standardized = T, rsq = T)
parameterEstimates((fit_mult_politics_new2))

#VOTE
#IV: proclamation (1 = flexible, 0 = absolute)

#Mediators: future honesty (Honest_composite2), hypocrisy (Hypocrisy_composite2 ), social benefit (SI_composite2)

#DV:voting = Voting

multipleMed_politics2_vote = "Hypocrisy_composite2   ~ a1*Proclamation
Honest_composite2 ~ a2*Proclamation
SI_composite2 ~ a3*Proclamation
Voting ~ b1*Hypocrisy_composite2  + b2*Honest_composite2 + b3*SI_composite2 + c*Proclamation

indirect1 := a1*b1
indirect2 := a2*b2
indirect3 := a3*b3
direct := c
total := c + (a1*b1) + (a2*b2) + (a3*b3)
#covariances
Hypocrisy_composite2   ~~ Honest_composite2
Hypocrisy_composite2 ~~ SI_composite2
Honest_composite2 ~~ SI_composite2
"

fit_mult_politics2_vote = sem(multipleMed_politics2_vote, se = "boot", bootstrap = 1000, data = absflex_only, likelihood = "wishart")
summary(fit_mult_politics2_vote, standardized = T, rsq = T)
parameterEstimates((fit_mult_politics2_vote))



