#Study 3 mediation on morality
#[see SPSS syntax in Study3.Rmd for trust model in SPSS and SPSS output for trust results]

#Load packages

library(here)
library(tidyverse)
library(lavaan)
library(psych)


#read in data

s3 <- read.csv(here("HypocrisyStudy3_cleaned.csv"))


#Create composite variables

#Hypocrisy
hypocrisy3_check <- data.frame(s3$Hypocrisy_1, s3$Hypocrisy_2, s3$Hypocrisy_3, s3$Hypocrisy_4, s3$Hypocrisy_5)
summary(hypocrisy3_check)
describe(hypocrisy3_check)
psych::alpha(hypocrisy3_check) 
s3 <- mutate(s3, 
             Hypocrisy_composite3 = ((s3$Hypocrisy_1 + s3$Hypocrisy_2 + s3$Hypocrisy_3 + s3$Hypocrisy_4 + s3$Hypocrisy_5)/5))

#Morality  
moral3_check <- data.frame(s3$Moral_1, s3$Moral_2, s3$Moral_3)
summary(moral3_check)
describe(moral3_check)
psych::alpha(moral3_check) # raw = .96
s3 <- mutate(s3, 
             Moral_composite3 = ((s3$Moral_1+s3$Moral_2+s3$Moral_3)/3))


#Future Honesty 
honest3_check <- data.frame(s3$Honest_frequency, s3$Honest_likely, s3$Honest_extreme, s3$Honest_committment)
summary(honest3_check)
describe(honest3_check)
psych::alpha(honest3_check) # rawalpha = 0.84
s3<- mutate(s3, 
            Honest_composite3 = ((s3$Honest_frequency + s3$Honest_likely + s3$Honest_extreme + s3$Honest_committment)/4))



#Model 

#IV = Sometimes = dummy-coded proclamation variable (sometimes = 1, rarely/never = 0)

#Mediators: future honesty (Honest_composite3), hypocrisy (Hypocrisy_composite3)

#Moderator = Behave lie 1 = prosocial lie, 0 = selfish truth

#DV: morality (Moral_composite3)

med_mod_study3 = "
Hypocrisy_composite3  ~ a11*sometimes + a12*behavlie + a13*sometimes:behavlie
Honest_composite3~ a21*sometimes + a22*behavlie+ a23*sometimes:behavlie
Moral_composite3 ~ b11*Hypocrisy_composite3 + b21*Honest_composite3 + c1*sometimes

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


fit.Mod.Med.study3 <- sem(model = med_mod_study3, se = "boot", bootstrap = 1000, data = s3, 
                               likelihood = "wishart")
summary(fit.Mod.Med.study3, standardized = T, rsq = T)
parameterEstimates((fit.Mod.Med.study3))

#Note: see SPSS output for moderated mediation on trust.
