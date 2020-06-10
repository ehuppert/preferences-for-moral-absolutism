#Study 2 mediation on morality
#[see SPSS syntax in Study2.Rmd for trust model in SPSS and SPSS output for trust results]

#Load packages

library(here)
library(tidyverse)
library(lavaan)
library(psych)


#read in data

s2 <- read.csv(here("HypocrisyStudy2_cleaned.csv"))


#Create composite variables

#Hypocrisy
hypocrisy2_check <- data.frame(s2$Hypocrisy_1, s2$Hypocrisy_2, s2$Hypocrisy_3, s2$Hypocrisy_4, s2$Hypocrisy_5)
summary(hypocrisy2_check)
describe(hypocrisy2_check)
psych::alpha(hypocrisy2_check) # raw alpha = 0.92
s2 <- mutate(s2, 
             Hypocrisy_composite2 = ((s2$Hypocrisy_1 + s2$Hypocrisy_2 + s2$Hypocrisy_3 + s2$Hypocrisy_4 + s2$Hypocrisy_5)/5))

#Morality  
moral2_check <- data.frame(s2$Moral_1, s2$Moral_2, s2$Moral_3)
summary(moral2_check)
describe(moral2_check)
psych::alpha(moral2_check) # raw = .96
s2 <- mutate(s2, 
             Moral_composite2 = ((s2$Moral_1+s2$Moral_2+s2$Moral_3)/3))


#Future Honesty 
honest2_check <- data.frame(s2$Honest_frequency, s2$Honest_likely, s2$Honest_extreme, s2$Honest_committment)
summary(honest2_check)
describe(honest2_check)
psych::alpha(honest2_check) # rawalpha = 0.84
s2<- mutate(s2, 
            Honest_composite2 = ((s2$Honest_frequency + s2$Honest_likely + s2$Honest_extreme + s2$Honest_committment)/4))



#Model 

#IV = Sometimes = dummy-coded proclamation variable (sometimes = 1, rarely/never = 0)

#Mediators: future honesty (Honest_composite2), hypocrisy (Hypocrisy_composite2)

#Moderator = Behave lie 1 = prosocial lie, 0 = selfish truth

#DV: morality (Moral_composite2)

med_mod_study2 = "
Hypocrisy_composite2  ~ a11*sometimes + a12*behavlie + a13*sometimes:behavlie
Honest_composite2~ a21*sometimes + a22*behavlie+ a23*sometimes:behavlie
Moral_composite2 ~ b11*Hypocrisy_composite2 + b21*Honest_composite2 + c1*sometimes

#Indirect effects conditional on moderator (a11+a13*ModValue)*b11 + (a21+a23*ModValue)*b21 

indirect1_selfish := a11*b11
indirect2_selfish := a21*b21
indirect_selfish := indirect1_selfish + indirect2_selfish 

indirect1_prosocial := (a11+a13)*b11
indirect2_prosocial := (a21+a23)*b21
indirect_prosocial := indirect1_prosocial + indirect2_prosocial

indirect1_diff := indirect1_prosocial - indirect1_selfish
indirect2_diff := indirect2_prosocial - indirect2_selfish
indirect_diff := indirect_prosocial - indirect_selfish

#Direct effects conditional on moderator c1

direct := c1

#Total effects conditional on moderator
total_selfish := direct + indirect_selfish
total_prosocial := direct + indirect_prosocial

#Proportion mediated conditional on moderator
#To match the output of mediate package
prop_mediated_selfish := indirect_selfish / total_selfish
prop_mediated_prosocial := indirect_prosocial / total_prosocial
"


fit.Mod.Med.study2 <- sem(model = med_mod_study2, se = "boot", bootstrap = 1000, data = s2, 
                               likelihood = "wishart")
summary(fit.Mod.Med.study2, standardized = T, rsq = T)
parameterEstimates((fit.Mod.Med.study2))

#Note: see SPSS output for moderated mediation on trust.
