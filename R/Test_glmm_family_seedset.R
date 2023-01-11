## TEST POISSON VS NEGATIVE BINOMIAL
library(lme4)
library(tidyr)
library(MASS)

# import data
source("R/1_Import_AppleQualityData.R")
source("R/2_Analysis_SeedSet_glmm.R")


# Aroma Berle
AromaBerle <- AromaSvelvik %>% 
  filter(Location == 'Berle') %>% 
  filter(Treatment != 'C')

#Poisson
SeedSet_AromaB0 <- glmer(Seeds_fully_developed ~ 1 + (1 | Tree), family = "poisson", data = AromaBerle)
SeedSet_AromaB1 <- glmer(Seeds_fully_developed ~ Treatment + (1 | Tree), family = "poisson", data = AromaBerle)

AIC(SeedSet_AromaB0, SeedSet_AromaB1)

summary(SeedSet_AromaB0)


#Negative binomial
SeedSet_AB0 <- glmer.nb(Seeds_fully_developed ~ 1 + (1 | Tree), data = AromaBerle)
SeedSet_AB1 <- glmer.nb(Seeds_fully_developed ~ Treatment + (1 | Tree), data = AromaBerle)

AIC(SeedSet_AB0, SeedSet_AB1)

summary(SeedSet_AB0)

p_res <- resid(SeedSet_AromaB0)
plot(fitted(SeedSet_AromaB0), p_res, col = 'steelblue', pcn=16, xlab = 'seedset', ylab = 'Standardized Residuals', main = 'Poisson')
abline(0,0)

nb_res <- resid(SeedSet_AB0)
plot(fitted(SeedSet_AB0), nb_res, col = 'steelblue', pcn=16, xlab = 'seedset', ylab = 'Standardized Residuals', main = 'Negative Binomial')
abline(0,0)


#Residual plot with negative binomial less spread out, therefore the best model fit

#Significance test:
A <- logLik(SeedSet_AromaB0)
B <- logLik(SeedSet_AB0)
teststat <- -2 * (as.numeric(A) - as.numeric(B))
pchisq(teststat, df = 1, lower.tail = FALSE)

#result = 0.6, no significant difference

###### HØYEN #######
# Aroma Høyen
AromaHoyen <- AromaSvelvik %>% 
  filter(Location == 'Høyen') %>% 
  filter(Treatment != 'C')

#Poisson
SeedSet_AromaH0 <- glmer(Seeds_fully_developed ~ 1 + (1 | Tree), family = "poisson", data = AromaHoyen)
SeedSet_AromaH1 <- glmer(Seeds_fully_developed ~ Treatment + (1 | Tree), family = "poisson", data = AromaHoyen)

AIC(SeedSet_AromaH0, SeedSet_AromaH1)

summary(SeedSet_AromaH1)


#Negative binomial
SeedSet_AH0 <- glmer.nb(Seeds_fully_developed ~ 1 + (1 | Tree), data = AromaHoyen)
SeedSet_AH1 <- glmer.nb(Seeds_fully_developed ~ Treatment + (1 | Tree), data = AromaHoyen)

AIC(SeedSet_AH0, SeedSet_AH1)

summary(SeedSet_AH1)

#Compare plots
p_res3 <- resid(SeedSet_AromaH1)
plot(fitted(SeedSet_AromaH1), p_res3, col = 'steelblue', pcn=16, xlab = 'seedset', ylab = 'Standardized Residuals', main = 'Poisson')
abline(0,0)

nb_res3 <- resid(SeedSet_AH1)
plot(fitted(SeedSet_AH1), nb_res3, col = 'steelblue', pcn=16, xlab = 'seedset', ylab = 'Standardized Residuals', main = 'Negative Binomial')
abline(0,0)

#Small difference, however negative binomial are slightly less spread out, meaning this is the best model

#Significance test:
C <- logLik(SeedSet_AromaH1)
D <- logLik(SeedSet_AH1)
teststat2 <- -2 * (as.numeric(C) - as.numeric(D))
pchisq(teststat2, df = 1, lower.tail = FALSE)

#result 0.1, no significant difference

# - Aroma
AromaSando <- AromaSvelvik %>% 
  filter(Location == 'Sando') %>% 
  filter(Treatment != 'C')

#Poisson
SeedSet_AromaS0 <- glmer(Seeds_fully_developed ~ 1 + (1 | Tree), family = "poisson", data = AromaSando)
SeedSet_AromaS1 <- glmer(Seeds_fully_developed ~ Treatment + (1 | Tree), family = "poisson", data = AromaSando)

AIC(SeedSet_AromaS0, SeedSet_AromaS1)

summary(SeedSet_AromaS1)

#Negative binomial
SeedSet_AS0 <- glmer.nb(Seeds_fully_developed ~ 1 + (1 | Tree), data = AromaSando)
SeedSet_AS1 <- glmer.nb(Seeds_fully_developed ~ Treatment + (1 | Tree), data = AromaSando)

AIC(SeedSet_AS0, SeedSet_AS1)

summary(SeedSet_AS1)

#Compare plots
p_res2 <- resid(SeedSet_AromaS1)
plot(fitted(SeedSet_AromaS1), p_res2, col = 'steelblue', pcn=16, xlab = 'seedset', ylab = 'Standardized Residuals', main = 'Poisson')
abline(0,0)

nb_res2 <- resid(SeedSet_AS1)
plot(fitted(SeedSet_AS1), nb_res2, col = 'steelblue', pcn=16, xlab = 'seedset', ylab = 'Standardized Residuals', main = 'Negative Binomial')
abline(0,0)

#No difference in plot

#Significance test:
E <- logLik(SeedSet_AromaS1)
G <- logLik(SeedSet_AS1)
teststat3 <- -2 * (as.numeric(E) - as.numeric(G))
pchisq(teststat3, df = 1, lower.tail = FALSE)

#result 0.8, no significant difference

############################

### Difference in seed set between orchards in East Norway
# - Aroma

AromaEast <- Svelvik %>% 
  filter(Apple_variety == 'Aroma')

#Poisson
SeedSet_AromaSvelvik0 <- glmer(Seeds_fully_developed ~ 1 + (1 | Tree:Location), family = "poisson", data = AromaEast)
SeedSet_AromaSvelvik1 <- glmer(Seeds_fully_developed ~ Treatment + (1 | Tree:Location), family = "poisson", data = AromaEast)
SeedSet_AromaSvelvik2 <- glmer(Seeds_fully_developed ~ Treatment + Location + (1 | Tree:Location), family = "poisson", data = AromaEast)

AIC(SeedSet_AromaSvelvik0, SeedSet_AromaSvelvik1, SeedSet_AromaSvelvik2)

summary(SeedSet_AromaSvelvik2)

#Negative binomial
SeedSet_ASvelvik0 <- glmer.nb(Seeds_fully_developed ~ 1 + (1 | Tree:Location), data = AromaEast)
SeedSet_ASvelvik1 <- glmer.nb(Seeds_fully_developed ~ Treatment + (1 | Tree:Location), data = AromaEast)
SeedSet_ASvelvik2 <- glmer.nb(Seeds_fully_developed ~ Treatment + Location + (1 | Tree:Location), data = AromaEast)

AIC(SeedSet_ASvelvik0, SeedSet_ASvelvik1, SeedSet_ASvelvik2)

summary(SeedSet_ASvelvik2)

#Compare plots
p_res4 <- resid(SeedSet_AromaSvelvik2)
plot(fitted(SeedSet_AromaSvelvik2), p_res4, col = 'steelblue', pcn=16, xlab = 'seedset', ylab = 'Standardized Residuals', main = 'Poisson')
abline(0,0)

nb_res4 <- resid(SeedSet_ASvelvik2)
plot(fitted(SeedSet_ASvelvik2), nb_res4, col = 'steelblue', pcn=16, xlab = 'seedset', ylab = 'Standardized Residuals', main = 'Negative Binomial')
abline(0,0)


#Poisson residuals less spread out, but very small difference


#Significance test:
H <- logLik(SeedSet_AromaSvelvik2)
I <- logLik(SeedSet_ASvelvik2)
teststat4 <- -2 * (as.numeric(H) - as.numeric(I))
pchisq(teststat4, df = 1, lower.tail = FALSE)

#result 0.08, no significant difference