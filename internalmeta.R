####Internal meta-analysis of studies from "Being dishonest about dishonesty"
###by Justin F. Landy###


##Load metafor
library(metafor)


##Overall meta-analysis comparing moral judgments of absolute vs. flexible proclamations, collapsing across other manipulations

#Study labels
stud.ovr <- c("
              Study 1", "Study 2 Time 2", "Study 3 Time 2", "Study 4", "Study 5", 
              "Supp 2", "Supp 3", "Supp 4", "Supp 5 US Time 2", "Supp 7")

#Sample size vectors
n.abso <- c(528, 170, 306, 192, 201, 167, 299, 177, 46, 83)
n.flex <- c(493, 185, 294, 204, 198, 160, 291, 173, 54, 79)

#Mean vectors
m.abso <- c(2.87, 2.79, 4.54, 5.03, 4.22, 3.59, 2.76, 2.81, 2.33, 2.70)
m.flex <- c(2.32, 1.96, 3.92, 4.08, 3.59, 3.59, 2.80, 2.44, 2.71, 2.39)

#SD vectors
sd.abso <- c(1.23, 1.33, 1.59, 1.43, 2.12, 1.68, 1.26, 1.32, 1.31, 1.07)
sd.flex <- c(1.03, 1.01, 1.39, 1.42, 1.63, 1.62, 1.16, 1.28, 1.46, 1.10)

#Calculate effect sizes and variances
es.ovr <- escalc("SMD", 
                 m1i = m.abso, m2i = m.flex, sd1i = sd.abso, sd2i = sd.flex, n1i = n.abso, n2i = n.flex, slab = stud.ovr)

#Run meta-analysis
meta.ovr <- rma.uni(es.ovr)
meta.ovr
forest(meta.ovr)
