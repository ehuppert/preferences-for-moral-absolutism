#Load packages

library(here)
library(tidyverse)
library(lavaan)
library(psych)

#Read in data

s2 <- read.csv(here("HypocrisyStudy2_cleaned.csv")) #265

s2$ID <- as.factor(s2$ID)
s2$Proclamation <- as.factor(s2$Proclamation)
levels(s2$Proclamation)

# Add in composite variables time two
#Hypocrisy
hypocrisy2_check <- data.frame(s2$Hypocrisy_1, s2$Hypocrisy_2, s2$Hypocrisy_3, s2$Hypocrisy_4, s2$Hypocrisy_5)
summary(hypocrisy2_check)
describe(hypocrisy2_check)
psych::alpha(hypocrisy2_check) #alpha = 0.88
s2 <- mutate(s2, Hypocrisy_composite2= 
               ((s2$Hypocrisy_1 + s2$Hypocrisy_2 + s2$Hypocrisy_3 +
                   s2$Hypocrisy_4 + s2$Hypocrisy_5)/5))

#Morality
moral2_t2_check <- data.frame(s2$Moral1_T2, s2$Moral2_T2, s2$Moral3_T2)
summary(moral2_t2_check)
describe(moral2_t2_check)
psych::alpha(moral2_t2_check) #alpha = 0.97
s2 <- mutate(s2, 
             Moral_composite2_t2 = 
               ((s2$Moral1_T2 + s2$Moral2_T2+s2$Moral3_T2)/3))

#Future Honesty
honesty2_t2_check  <- data.frame(s2$Honest_frequencyT2, s2$Honest_likelihoodT2, s2$Honest_extremeT2, s2$Honest_committmentT2)
summary(honesty2_t2_check)
describe(honesty2_t2_check)
psych::alpha(honesty2_t2_check) #alpha = 0.81
s2 <- mutate(s2, 
             Honest_composite2_t2 = 
               ((s2$Honest_frequencyT2 + s2$Honest_likelihoodT2 +
                   s2$Honest_extremeT2 + s2$Honest_committmentT2)/4))


# Use subsetted data with no control condition
control_exclude_full <- s2 %>%
  filter(Proclamation != "control") #355


glimpse(control_exclude_full$Proclamation)
control_exclude_full$Proclamation <- as.factor(control_exclude_full$Proclamation)
control_exclude_full$Proclamation <- droplevels(control_exclude_full$Proclamation)
levels(control_exclude_full$Proclamation)
#0 = absolute, 1 = flexible

#MORAL 
#IV: proclamation (0 = absolute, 1 = flexible)

#Mediators: time two future honesty (Honest_composite2_t2), hypocrisy (Hypocrisy_composite2)

#DV: time two morality (Moral_composite2_t2)

multipleMed_politics2 = "Hypocrisy_composite2  ~ a1*Proclamation
Honest_composite2_t2 ~ a2*Proclamation
Moral_composite2_t2 ~ b1*Hypocrisy_composite2  + b2*Honest_composite2_t2 + c*Proclamation

indirect1 := a1*b1
indirect2 := a2*b2
direct := c
total := c + (a1*b1) + (a2*b2) 
#covariances
Hypocrisy_composite2   ~~ Honest_composite2_t2
"

fit_mult_politics_new2 = sem(multipleMed_politics2, se = "boot", bootstrap = 1000, 
                             data = control_exclude_full, likelihood = "wishart")
summary(fit_mult_politics_new2, standardized = T, rsq = T)
parameterEstimates((fit_mult_politics_new2))

#VOTE
#IV: proclamation (0 = flexible, 1 = absolute)

#Mediators: time two future honesty (Honest_composite2_t2), hypocrisy (Hypocrisy_composite2)

#DV:voting = time two Voting

multipleMed_politics2_vote = "Hypocrisy_composite2   ~ a1*Proclamation
Honest_composite2_t2 ~ a2*Proclamation
Voting_T2 ~ b1*Hypocrisy_composite2  + b2*Honest_composite2_t2+  c*Proclamation

indirect1 := a1*b1
indirect2 := a2*b2
direct := c
total := c + (a1*b1) + (a2*b2) 
#covariances
Hypocrisy_composite2   ~~ Honest_composite2_t2
"

fit_mult_politics2_vote = sem(multipleMed_politics2_vote, se = "boot", bootstrap = 1000, 
                              data = control_exclude_full, likelihood = "wishart")
summary(fit_mult_politics2_vote, standardized = T, rsq = T)
parameterEstimates((fit_mult_politics2_vote))


