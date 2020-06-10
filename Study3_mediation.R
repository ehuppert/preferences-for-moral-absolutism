#Study 3 mediation on morality
#[see SPSS syntax in Study2.Rmd for trust model in SPSS and SPSS output for trust results]

#Load packages

library(here)
library(tidyverse)
library(lavaan)
library(psych)


#read in data

s3 <- read.csv(here("HypocrisyStudy3.csv"))


#Create composite variables

#Hypocrisy
hypocrisy_3_check  <- data.frame(s3$Hypocrisy_1, s3$Hypocrisy_2, s3$Hypocrisy_3, s3$Hypocrisy_4, s3$Hypocrisy_5)
summary(hypocrisy_3_check)
describe(hypocrisy_3_check)
psych::alpha(hypocrisy_3_check) # raw alpha = 0.96
s3 <- mutate(s3, Hypocrisy_composite3 = ((s3$Hypocrisy_1 + s3$Hypocrisy_2 + s3$Hypocrisy_3 + s3$Hypocrisy_4 + s3$Hypocrisy_5)/5))

#Morality
moral_3_check <- data.frame(s3$Moral_1, s3$Moral_2, s3$Moral_3)
summary(moral_3_check)
describe(moral_3_check)
psych::alpha(moral_3_check) # rawalpha = 0.98
s3 <- mutate(s3, Moral_composite3 = ((s3$Moral_1 + s3$Moral_2 + s3$Moral_3)/3))

#Future Honesty
honest_3_check <- data.frame(s3$Honest_frequency, s3$Honest_likely, s3$Honest_extreme, s3$Honest_committment)
summary(honest_3_check)
describe(honest_3_check)
psych::alpha(honest_3_check) # raw alpha = 0.92
s3 <- mutate(s3, Honest_composite3 = ((s3$Honest_frequency + s3$Honest_likely + s3$Honest_extreme + s3$Honest_committment)/4))

#Social Influene
cor.test(s3$Social_benefit1, s3$Social_benefit2) #r = 0.84
s3 <- mutate(s3, SI_composite3 = ((s3$Social_benefit1 + s3$Social_benefit2)/2))

#Model 

#IV = Sometimes = dummy-coded proclamation variable (sometimes = 1, rarely/never = 0)

#Mediators: future honesty (Honest_composite3, hypocrisy (Hypocrisy_composite3), social benefit (SI_composite3)

#Moderator = Behave lie 1 = selfish lie, 0 = prosocial truth

#DV: morality (Moral_composite3)


med_mod_study3 = '
Hypocrisy_composite3 ~ a11*sometimes + a12*behavlie + a13*sometimes:behavlie
Honest_composite3  ~ a21*sometimes + a22*behavlie + a23*sometimes:behavlie
SI_composite3 ~ a31*sometimes + a32*behavlie + a33*sometimes:behavlie
Moral_composite3~ b11*Hypocrisy_composite3 + b21*Honest_composite3  + b31*SI_composite3+ c1*sometimes + d1*behavlie + b12*Hypocrisy_composite3:behavlie + b22*Honest_composite3:behavlie + b32*SI_composite3:behavlie + c2*sometimes:behavlie

#Indirect effects conditional on moderator (a11+a13*ModValue)*(b11+b12*ModValue) + (a21+a23*ModValue)*(b21+b22*ModValue) + (a31+a33*ModValue)*(b31+b32*ModValue)

indirect1_prosocial := a11*b11
indirect2_prosocial := a21*b21
indirect3_prosocial := a31*b31
indirect_prosocial := indirect1_prosocial + indirect2_prosocial + indirect3_prosocial

indirect1_selfish := (a11+a13)*(b11+b12)
indirect2_selfish := (a21+a23)*(b21+b22)
indirect3_selfish := (a31+a33)*(b31+b32)
indirect_selfish := indirect1_selfish + indirect2_selfish + indirect3_selfish

indirect1_diff := indirect1_selfish - indirect1_prosocial
indirect2_diff := indirect2_selfish - indirect2_prosocial
indirect3_diff := indirect3_selfish - indirect3_prosocial
indirect_diff := indirect_selfish - indirect_prosocial

#Direct effects conditional on moderator (c1 + c2*ModValue)

direct_prosocial := c1
direct_selfish := c1 + c2
direct_diff := direct_selfish - direct_prosocial

#Total effects conditional on moderator
total_prosocial := direct_prosocial + indirect_prosocial
total_selfish := direct_selfish + indirect_selfish

#Proportion mediated conditional on moderator
#To match the output of mediate package
prop_mediated_prosocial := indirect_prosocial / total_prosocial
prop_mediated_selfish := indirect_selfish / total_selfish
'



fit.Mod.Med.study3 <- sem(model = med_mod_study3, se = "boot", bootstrap = 1000, data = s3, 
                          likelihood = "wishart")

summary(fit.Mod.Med.study3, standardized = T, rsq = T)
parameterEstimates((fit.Mod.Med.study3))


#Note: see SPSS output for moderated mediation on trust.