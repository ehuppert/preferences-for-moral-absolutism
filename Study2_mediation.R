#Read in data

s2 <- read.csv(here("HypocrisyStudy2_cleaned.csv")) #265


# Add in composite variables
#Hypocrisy
hypocrisy2_check <- data.frame(s2$Hypocrisy_1, s2$Hypocrisy_2, s2$Hypocrisy_3, s2$Hypocrisy_4, s2$Hypocrisy_5)
summary(hypocrisy2_check)
describe(hypocrisy2_check)
psych::alpha(hypocrisy2_check) #alpha = 0.89 
s2 <- mutate(s2, Hypocrisy_composite2= 
               ((s2$Hypocrisy_1 + s2$Hypocrisy_2 + s2$Hypocrisy_3 + s2$Hypocrisy_4 +   s2$Hypocrisy_5)/5))

#Morality
moral2_check <- data.frame(s2$Moral_1, s2$Moral_2, s2$Moral_3)
summary(moral2_check)
describe(moral2_check)
psych::alpha(moral2_check) #alpha = 0.94 

s2 <- mutate(s2, Moral_composite2 = ((s2$Moral_1 + s2$Moral_2 +s2$Moral_3)/3))

#Future Honesty
honesty2_check  <- data.frame(s2$Honest_frequency, s2$Honest_likelihood, s2$Honest_extreme, s2$Honest_committment)
summary(honesty2_check)
describe(honesty2_check)
psych::alpha(honesty2_check) #alpha = 0.74
s2 <- mutate(s2, Honest_composite2 = 
               ((s2$Honest_frequency + s2$Honest_likelihood + s2$Honest_extreme + s2$Honest_committment)/4))

#Social benefit of the proclamation
cor.test(s2$Social_1, s2$Social_2) #r = 0.87

s2 <- mutate(s2, SI_composite2 = ((s2$Social_1 + s2$Social_2)/2))


# Use subsetted data with no control condition
absflex_only <- s2 %>%
  filter (Proclamation != "control")

glimpse(absflex_only$Proclamation)
absflex_only$proclamation <- droplevels(absflex_only$Proclamation)
levels(absflex_only$Proclamation)

#re-order proclamation levels for ease of interpreation

absflex_only$Proclamation <- factor(absflex_only$Proclamation, levels = c("flexible", "absolute"), labels = c("flexible", "absolute"))

#MORAL 
#IV: proclamation (0 = flexible, 1 = absolute)

#Mediators: future honesty (Honest_composite2), hypocrisy (h_composite2), social benefit (SI_composite2)

#DV: morality (m_composite2)

multipleMed_politics_new2 = "h_composite2  ~ a1*proclamation
Honest_composite2 ~ a2*proclamation
SI_composite2 ~ a3*proclamation
m_composite2 ~ b1*h_composite2 + b2*Honest_composite2 + b3*SI_composite2 + c*proclamation

indirect1 := a1*b1
indirect2 := a2*b2
indirect3 := a3*b3
direct := c
total := c + (a1*b1) + (a2*b2) + (a3*b3)
#covariances
h_composite2  ~~ Honest_composite2
h_composite2 ~~ SI_composite2
Honest_composite2 ~~ SI_composite2
"

fit_mult_politics_new2 = sem(multipleMed_politics_new2 , se = "boot", bootstrap = 1000, data = absflex_only, likelihood = "wishart")
summary(fit_mult_politics_new2, standardized = T, rsq = T)
parameterEstimates((fit_mult_politics_new2))

#VOTE
#IV: proclamation (0 = flexible, 1 = absolute)

#Mediators: future honesty (Honest_composite2), hypocrisy (h_composite2), social benefit (SI_composite2)

#DV:voting = vote

multipleMed_politics_new2.2 = "h_composite2  ~ a1*proclamation
Honest_composite2 ~ a2*proclamation
SI_composite2 ~ a3*proclamation
vote ~ b1*h_composite2 + b2*Honest_composite2 + b3*SI_composite2 + c*proclamation

indirect1 := a1*b1
indirect2 := a2*b2
indirect3 := a3*b3
direct := c
total := c + (a1*b1) + (a2*b2) + (a3*b3)
#covariances
h_composite2  ~~ Honest_composite2
h_composite2 ~~ SI_composite2
Honest_composite2 ~~ SI_composite2
"

fit_mult_politics_new2.2 = sem(multipleMed_politics_new2.2  , se = "boot", bootstrap = 1000, data = absflex_only, likelihood = "wishart")
summary(fit_mult_politics_new2.2, standardized = T, rsq = T)
parameterEstimates((fit_mult_politics_new2.2))


# just honesty ####

#MORAL 
#IV: proclamation (0 = flexible, 1 = absolute)

#Mediators: future honesty (Honest_composite2)

#DV: morality (m_composite2)

mediation_politics_new2.3 = "Honest_composite2  ~ a1*proclamation
m_composite2 ~ b1*Honest_composite2 + c*proclamation

indirect1 := a1*b1
direct := c
total := c + (a1*b1) 
"

fit_mult_politics_new2.3 = sem(mediation_politics_new2.3 , se = "boot", bootstrap = 1000, data = absflex_only, likelihood = "wishart")
summary(fit_mult_politics_new2.3, standardized = T, rsq = T)
parameterEstimates((fit_mult_politics_new2.3))

#DV: voting (vote)

mediation_politics_new2.4 = "Honest_composite2  ~ a1*proclamation
vote ~ b1*Honest_composite2 + c*proclamation

indirect1 := a1*b1
direct := c
total := c + (a1*b1) 
"

fit_mult_politics_new2.4 = sem(mediation_politics_new2.4 , se = "boot", bootstrap = 1000, data = absflex_only, likelihood = "wishart")
summary(fit_mult_politics_new2.4, standardized = T, rsq = T)
parameterEstimates((fit_mult_politics_new2.4))

# just honesty and SI benefit ####

#MORAL

#Honesty
multipleMed_politics_new2.5 = "Honest_composite2  ~ a1*proclamation
SI_composite2 ~ a2*proclamation
m_composite2 ~ b1*Honest_composite2 + b2*SI_composite2+ c*proclamation

indirect1 := a1*b1
indirect2 := a2*b2
direct := c
total := c + (a1*b1) + (a2*b2) 
#covariances
Honest_composite2  ~~ SI_composite2
"

fit_mult_politics_new2.5 = sem(multipleMed_politics_new2.5, se = "boot", bootstrap = 1000, data = absflex_only, likelihood = "wishart")
summary(fit_mult_politics_new2.5, standardized = T, rsq = T)
parameterEstimates((fit_mult_politics_new2.5))

#VOTE

multipleMed_politics_new2.6 = "Honest_composite2  ~ a1*proclamation
SI_composite2 ~ a2*proclamation
vote ~ b1*Honest_composite2 + b2*SI_composite2+ c*proclamation

indirect1 := a1*b1
indirect2 := a2*b2
direct := c
total := c + (a1*b1) + (a2*b2) 
#covariances
Honest_composite2  ~~ SI_composite2
"

fit_mult_politics_new2.6 = sem(multipleMed_politics_new2.6, se = "boot", bootstrap = 1000, data = absflex_only, likelihood = "wishart")
summary(fit_mult_politics_new2.6, standardized = T, rsq = T)
parameterEstimates((fit_mult_politics_new2.6))

